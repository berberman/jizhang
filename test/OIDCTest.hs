{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cryptographic and OIDC trust-boundary regressions, including signed HTTP code
-- exchange. Protocol fixtures use a shared fixed clock and require no database.
module OIDCTest (oidcTests) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Aeson (Object, Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Jizhang.Auth.Security
import Jizhang.OIDC
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI (parseURI, uriQuery)
import OIDCProvider
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- Synthetic keys used only by these isolated tests. Assertions never render secrets.
keyText :: Text
keyText = TE.decodeUtf8 (B64.encode (BS.pack [0 .. 31]))

-- | Parse only synthetic key material, reporting failure without rendering the key.
withKey :: (SecretKey -> IO ()) -> IO ()
withKey action = case parseSecretKey keyText of
  Left _ -> assertFailure "Synthetic encryption key was rejected"
  Right key -> action key

oidcTests :: TestTree
oidcTests = testGroup "OIDC" [securityTests, runtimeTests, protocolTests, testGroup "authorization code exchange" exchangeTests]

securityTests :: TestTree
securityTests =
  testGroup
    "security primitives"
    [ testCase "S256 hashes the encoded verifier (RFC 7636 vector)" $
        pkceChallenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
          @?= "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM",
      testCase "AES-GCM preserves Unicode and rejects a different row or purpose" $ withKey $ \key -> do
        let plaintext = "synthetic secret: 密碼 🔐"
            aad = "oidc_transactions/row-a/pkce"
        ciphertext <- encryptSecret key aad plaintext
        assertBool "Unicode secret did not round-trip" (decryptSecret key aad ciphertext == Right plaintext)
        assertBool "Ciphertext was accepted for another row" (isLeft (decryptSecret key "oidc_transactions/row-b/pkce" ciphertext))
        assertBool "Ciphertext was accepted for another purpose" (isLeft (decryptSecret key "oidc_transactions/row-a/result" ciphertext)),
      testCase "AES-GCM rejects a different key and every tampered envelope component" $ withKey $ \key -> do
        ciphertext <- encryptSecret key "user_sessions/row/id_token" "synthetic-id-token"
        let otherText = TE.decodeUtf8 (B64.encode (BS.replicate 32 255))
        case parseSecretKey otherText of
          Left _ -> assertFailure "Alternate synthetic key was rejected"
          Right other -> assertBool "Wrong key decrypted ciphertext" (isLeft (decryptSecret other "user_sessions/row/id_token" ciphertext))
        case T.splitOn "." ciphertext of
          [version, nonce, body, tag] -> forM_
            [ T.intercalate "." ["v2", nonce, body, tag],
              T.intercalate "." [version, corrupt nonce, body, tag],
              T.intercalate "." [version, nonce, corrupt body, tag],
              T.intercalate "." [version, nonce, body, corrupt tag],
              T.intercalate "." [version, "", body, tag],
              T.intercalate "." [version, nonce, body, ""]
            ]
            $ \tampered -> assertBool "Tampered encrypted secret was accepted" (isLeft (decryptSecret key "user_sessions/row/id_token" tampered))
          _ -> assertFailure "Encryption produced an invalid envelope",
      testCase "AES-GCM permits an empty payload and uses independent nonces" $ withKey $ \key -> do
        first <- encryptSecret key "synthetic/aad" ""
        second <- encryptSecret key "synthetic/aad" ""
        assertBool "Empty secret did not round-trip" (decryptSecret key "synthetic/aad" first == Right "")
        assertBool "Encryption reused a nonce" (first /= second),
      testCase "encryption keys require canonical standard base64 and exactly 256 bits" $ do
        forM_
          [ "",
            "not-base64",
            keyText <> "\n",
            T.dropEnd 1 keyText,
            TE.decodeUtf8 (B64.encode (BS.replicate 31 0)),
            TE.decodeUtf8 (B64.encode (BS.replicate 33 0))
          ]
          $ \invalid ->
            assertBool "Malformed encryption key was accepted" (isLeft (parseSecretKey invalid)),
      testCase "digest comparison distinguishes equal length values and length mismatches" $ do
        let digest = tokenDigest "synthetic-state-a"
        assertBool "Equal digests did not compare equal" (secureEqual digest (tokenDigest "synthetic-state-a"))
        assertBool "Different digests compared equal" (not (secureEqual digest (tokenDigest "synthetic-state-b")))
        assertBool "Truncated digest compared equal" (not (secureEqual digest (BS.take 31 digest)))
    ]
  where
    corrupt value = case T.uncons value of
      Nothing -> "A"
      Just (c, rest) -> T.cons (if c == 'A' then 'B' else 'A') rest

runtimeTests :: TestTree
runtimeTests =
  testGroup
    "runtime trust boundary"
    [ testCase "missing deployment settings leave local authentication runtime available but OIDC unready" $ do
        runtime <- newAuthRuntime Nothing Nothing Nothing []
        assertBool "Unconfigured runtime became OIDC-ready" (isLeft (runtimeReady runtime)),
      testCase "configured base prefixes survive all generated callback paths" $ do
        runtime <- newAuthRuntime (Just "https://app.example/api/v2/") (Just "https://app.example/web/") (Just keyText) []
        runtimeReady runtime @?= Right ()
        callbackUrl runtime @?= Right "https://app.example/api/v2/auth/oidc/callback"
        backchannelLogoutUrl runtime @?= Right "https://app.example/api/v2/auth/oidc/backchannel-logout"
        postLogoutRedirectUrl runtime @?= Right "https://app.example/api/v2/auth/oidc/logout/callback",
      testCase "cross-origin frontend requires an explicit allowed origin" $ do
        startupFails (Just "https://api.example") (Just "https://frontend.example/") (Just keyText) []
        runtime <- newAuthRuntime (Just "https://api.example") (Just "https://frontend.example/") (Just keyText) ["https://frontend.example"]
        runtimeReady runtime @?= Right (),
      testCase "HTTP development URLs permit only literal loopback hosts" $ do
        forM_ ["http://localhost:8080", "http://127.0.0.1:8080", "http://[::1]:8080"] $ \uri ->
          assertBool "Literal loopback URL was rejected" (not (isLeft (validateBaseUrl False uri)))
        forM_ ["http://localhost.example", "http://127.0.0.2", "http://2130706433", "http://[::ffff:127.0.0.1]", "http://example.com"] $ \uri ->
          assertBool "Nonliteral loopback HTTP URL was accepted" (isLeft (validateBaseUrl False uri)),
      testCase "configuration rejects userinfo, query, fragment, wildcard and relative URLs" $
        forM_ ["/api", "https://user:secret@app.example/", "https://app.example/?secret=x", "https://app.example/#callback", "https://*.example/", "https://app.example:0/", "https://app.example:65536/"] $ \uri ->
          assertBool "Unsafe deployment URL was accepted" (isLeft (validateBaseUrl False uri)),
      testCase "explicitly malformed settings fail startup instead of silently disabling OIDC" $ do
        startupFails (Just "http://app.example") Nothing Nothing []
        startupFails Nothing (Just "https://app.example") Nothing []
        startupFails Nothing Nothing (Just "invalid-key") []
    ]

-- | Explicit malformed deployment settings must fail construction, not disable OIDC.
startupFails :: Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> Assertion
startupFails public frontend key origins = do
  result <- try (newAuthRuntime public frontend key origins) :: IO (Either IOException AuthRuntime)
  assertBool "Malformed explicit configuration did not fail startup" (isLeft result)

fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 10 17) 43200

fixtureRedirect :: Text
fixtureRedirect = "http://127.0.0.1:8080/auth/oidc/callback"

-- | Freeze provider and consumer clocks together so claim-age checks are exact;
-- the provider still performs real HTTP and RSA signing on an ephemeral port.
withFixture :: (OidcProvider -> AuthRuntime -> ProviderSettings -> IO ()) -> IO ()
withFixture action = withOidcProvider
  defaultProviderConfig
    { providerClock = pure fixedNow,
      providerRedirectUri = fixtureRedirect
    }
  $ \provider -> do
    runtime <- newAuthRuntime (Just "http://127.0.0.1:8080") (Just "http://127.0.0.1:3000/") (Just keyText) ["http://127.0.0.1:3000"]
    let settings = ProviderSettings 0 (providerIssuer provider) providerClientId providerClientSecret
    action provider runtime {authClock = pure fixedNow} settings

-- | Reject protocol input without printing the rejected payload or successful value.
expectProtocolFailure :: Either ProtocolError a -> Assertion
expectProtocolFailure result = case result of
  Left _ -> pure ()
  Right _ -> assertFailure "Invalid protocol input was accepted"

expectProtocolSuccess :: Either ProtocolError a -> (a -> IO ()) -> IO ()
expectProtocolSuccess result action = case result of
  Left err -> assertFailure ("Protocol operation failed: " <> show err)
  Right value -> action value

logoutClaims :: OidcProvider -> Object
logoutClaims provider = case object
  [ "iss" .= providerIssuer provider,
    "aud" .= providerClientId,
    "iat" .= (floor (utcTimeToPOSIXSeconds fixedNow) :: Integer),
    "jti" .= ("synthetic-logout-event" :: Text),
    "sid" .= ("synthetic-session" :: Text),
    "events" .= object ["http://schemas.openid.net/event/backchannel-logout" .= object []]
  ] of
  Object value -> value
  _ -> error "Object construction failed"

protocolTests :: TestTree
protocolTests =
  testGroup
    "signed HTTP provider protocol"
    [ testCase "real discovery validates the exact issuer and usable RS256 metadata" $ withFixture $ \provider runtime settings -> do
        result <- discoverProvider runtime settings
        expectProtocolSuccess result $ \verified -> do
          assertBool "End-session endpoint missing" (providerEndSessionEndpoint verified /= Nothing)
          assertBool "Back-channel support missing" (providerBackchannelLogoutSupported verified)
        providerTokenExchangeCount provider >>= (@?= 0),
      testCase "back-channel logout accepts signed tokens without exp" $ withFixture $ \provider runtime settings -> do
        token <- signLogoutToken provider (logoutClaims provider)
        result <- validateLogoutToken runtime settings token
        expectProtocolSuccess result $ \verified -> do
          logoutIssuer verified @?= providerIssuer provider
          logoutSid verified @?= Just "synthetic-session"
          logoutSubject verified @?= Nothing,
      testCase "back-channel logout accepts sub-only and both identifiers" $ withFixture $ \provider runtime settings -> do
        let claims = KM.insert "sub" (String "synthetic-alice") (logoutClaims provider)
        forM_ [claims, KM.delete "sid" claims] $ \payload -> do
          token <- signLogoutToken provider payload
          validateLogoutToken runtime settings token >>= \result -> expectProtocolSuccess result $ \verified ->
            logoutSubject verified @?= Just "synthetic-alice",
      testCase "back-channel logout refreshes a rotated unknown signing key" $ withFixture $ \provider runtime settings -> do
        cachedProvider runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
        rotateProviderKey provider
        token <- signLogoutToken provider (logoutClaims provider)
        validateLogoutToken runtime settings token >>= \result -> expectProtocolSuccess result (const (pure ()))
        providerTokenExchangeCount provider >>= (@?= 0),
      testCase "bad signatures, none, HS256 and encrypted JWT shapes are rejected" $ withFixture $ \provider runtime settings -> do
        token <- signLogoutToken provider (logoutClaims provider)
        case T.splitOn "." token of
          [header, payload, signature] -> do
            let changeSignature = case T.uncons signature of
                  Just (c, rest) -> T.cons (if c == 'A' then 'B' else 'A') rest
                  Nothing -> "A"
                encodeHeader value = TE.decodeUtf8 (B64URL.encodeUnpadded (LBS.toStrict (encode value)))
                noneHeader = encodeHeader (object ["alg" .= ("none" :: Text)])
                symmetricHeader = encodeHeader (object ["alg" .= ("HS256" :: Text)])
                encryptedHeader = encodeHeader (object ["alg" .= ("RSA-OAEP" :: Text), "enc" .= ("A256GCM" :: Text)])
            forM_
              [ header <> "." <> payload <> "." <> changeSignature,
                noneHeader <> "." <> payload <> ".",
                symmetricHeader <> "." <> payload <> "." <> signature,
                encryptedHeader <> ".encrypted.key.iv.payload.tag"
              ]
              $ \invalid ->
                validateLogoutToken runtime settings invalid >>= expectProtocolFailure
          _ -> assertFailure "Fixture produced an invalid signed-token shape",
      testGroup
        "back-channel claim rejection"
        [ testCase label $ withFixture $ \provider runtime settings -> do
            token <- signLogoutToken provider (alter (logoutClaims provider))
            validateLogoutToken runtime settings token >>= expectProtocolFailure
        | (label, alter) <- invalidLogoutClaims
        ]
    ]

invalidLogoutClaims :: [(String, Object -> Object)]
invalidLogoutClaims =
  [ ("wrong issuer", KM.insert "iss" (String "https://other.example/")),
    ("wrong audience", KM.insert "aud" (String "another-client")),
    ("multiple audiences require azp", KM.insert "aud" (toArray [providerClientId, "another-client"])),
    ("wrong authorized party", KM.insert "azp" (String "another-client")),
    ("invalid event", KM.insert "events" Null),
    ("nonempty event payload", KM.insert "events" (object ["http://schemas.openid.net/event/backchannel-logout" .= object ["unexpected" .= True]])),
    ("nonce forbidden even if null", KM.insert "nonce" Null),
    ("invalid jti", KM.insert "jti" Null),
    ("empty jti", KM.insert "jti" (String "")),
    ("no session or subject", KM.delete "sid"),
    ("empty session", KM.insert "sid" (String "")),
    ("empty subject", KM.insert "sub" (String "")),
    ("issued too far in the past", KM.insert "iat" (Number (fromInteger (epoch - 301)))),
    ("issued too far in the future", KM.insert "iat" (Number (fromInteger (epoch + 61)))),
    ("expired optional expiration", KM.insert "exp" (Number (fromInteger epoch)))
  ]
  where
    epoch = floor (utcTimeToPOSIXSeconds fixedNow) :: Integer
    toArray values = case object ["values" .= values] of
      Object o -> maybe Null id (KM.lookup "values" o)
      _ -> Null

authorizeCode :: AuthRuntime -> ProviderSettings -> Text -> Text -> IO Text
authorizeCode runtime settings verifier nonce = do
  result <- cachedProvider runtime settings
  case result of
    Left err -> assertFailure ("Discovery failed: " <> show err) >> pure ""
    Right provider -> do
      request <- HTTP.parseRequest (T.unpack (authorizationUrl provider settings fixtureRedirect "synthetic-state" nonce (pkceChallenge verifier) False <> "&decision=allow"))
      response <- HTTP.httpNoBody request {HTTP.redirectCount = 0} (authManager runtime)
      assertBool "Authorization did not return a redirect" (statusCode (HTTP.responseStatus response) == 303)
      let parsed = do
            location <- lookup "Location" (HTTP.responseHeaders response)
            uri <- parseURI (T.unpack (TE.decodeUtf8 location))
            lookup "code" (parseSimpleQuery (TE.encodeUtf8 (T.pack (uriQuery uri))))
      case parsed of
        Nothing -> assertFailure "Authorization returned no code" >> pure ""
        Just code -> pure (TE.decodeUtf8 code)

exchangeFixture :: AuthRuntime -> ProviderSettings -> IO (Either ProtocolError VerifiedOidcIdentity)
exchangeFixture runtime settings = do
  verifier <- randomToken
  nonce <- randomToken
  code <- authorizeCode runtime settings verifier nonce
  exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest nonce) Nothing

exchangeTests :: [TestTree]
exchangeTests =
  [ testCase "real Basic-authenticated PKCE exchange verifies signed identity and UserInfo" $ withFixture $ \provider runtime settings -> do
      exchangeFixture runtime settings >>= \result -> expectProtocolSuccess result $ \identity -> do
        verifiedIssuer identity @?= providerIssuer provider
        verifiedSubject identity @?= "synthetic-alice"
        profileEmailVerified (verifiedProfile identity) @?= False
        verifiedReauthenticatedAt identity @?= Nothing
      providerTokenExchangeCount provider >>= (@?= 1),
    testCase "unknown kid refresh never repeats the token exchange" $ withFixture $ \provider runtime settings -> do
      cachedProvider runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
      rotateProviderKey provider
      exchangeFixture runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
      providerTokenExchangeCount provider >>= (@?= 1),
    testCase "authorization codes cannot be exchanged twice" $ withFixture $ \provider runtime settings -> do
      verifier <- randomToken
      nonce <- randomToken
      code <- authorizeCode runtime settings verifier nonce
      let exchange = exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest nonce) Nothing
      exchange >>= \result -> expectProtocolSuccess result (const (pure ()))
      exchange >>= expectProtocolFailure
      providerTokenExchangeCount provider >>= (@?= 2),
    testCase "wrong PKCE proof cannot exchange a code" $ withFixture $ \_ runtime settings -> do
      verifier <- randomToken
      wrong <- randomToken
      nonce <- randomToken
      code <- authorizeCode runtime settings verifier nonce
      exchangeAuthorizationCode runtime settings fixtureRedirect code wrong (tokenDigest nonce) Nothing >>= expectProtocolFailure,
    testCase "wrong stored nonce fails after valid PKCE exchange" $ withFixture $ \_ runtime settings -> do
      verifier <- randomToken
      nonce <- randomToken
      code <- authorizeCode runtime settings verifier nonce
      exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest "different-nonce") Nothing >>= expectProtocolFailure,
    testCase "explicit reauthentication proves matching fresh auth_time" $ withFixture $ \_ runtime settings -> do
      verifier <- randomToken
      nonce <- randomToken
      code <- authorizeCode runtime settings verifier nonce
      exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest nonce) (Just fixedNow) >>= \result ->
        expectProtocolSuccess result $ \identity -> verifiedReauthenticatedAt identity @?= Just fixedNow,
    testGroup
      "signed ID claim rejection"
      [ testCase label $ withFixture $ \provider runtime settings -> do
          setProviderBehavior provider defaultProviderBehavior {behaviorClaims = KM.singleton key value}
          exchangeFixture runtime settings >>= expectProtocolFailure
      | (label, key, value) <- invalidIdentityClaims
      ],
    testGroup
      "required ID claims"
      [ testCase (T.unpack claim) $ withFixture $ \provider runtime settings -> do
          setProviderBehavior provider defaultProviderBehavior {behaviorOmitClaims = [claim]}
          exchangeFixture runtime settings >>= expectProtocolFailure
      | claim <- ["iss", "sub", "aud", "exp", "iat", "nonce"]
      ],
    testCase "UserInfo subject mismatch rejects an otherwise valid token" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorUserInfo = KM.singleton "sub" (String "synthetic-bob")}
      exchangeFixture runtime settings >>= expectProtocolFailure,
    testCase "malformed token JSON fails closed" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorMalformedToken = True}
      exchangeFixture runtime settings >>= expectProtocolFailure,
    testCase "token endpoint redirects are not followed" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorTokenRedirect = Just (providerIssuer provider <> "token")}
      exchangeFixture runtime settings >>= expectProtocolFailure
      providerTokenExchangeCount provider >>= (@?= 1),
    testCase "discovery redirects are not followed" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorDiscoveryRedirect = Just (providerIssuer provider <> ".well-known/openid-configuration")}
      discoverProvider runtime settings >>= expectProtocolFailure,
    testCase "metadata response exceeding one MiB is rejected" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorMetadata = KM.singleton "oversized" (String (T.replicate 1048576 "x"))}
      discoverProvider runtime settings >>= expectProtocolFailure,
    testCase "five-minute cache expires and settings revisions never reuse old discovery" $ withFixture $ \provider runtime settings -> do
      cachedProvider runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
      setProviderBehavior provider defaultProviderBehavior {behaviorMetadata = KM.singleton "issuer" (String "https://other.example/")}
      cachedProvider runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
      cachedProvider runtime {authClock = pure (addUTCTime 301 fixedNow)} settings >>= expectProtocolFailure
      setProviderBehavior provider defaultProviderBehavior
      cachedProvider runtime settings >>= \result -> expectProtocolSuccess result (const (pure ()))
      setProviderBehavior provider defaultProviderBehavior {behaviorMetadata = KM.singleton "issuer" (String "https://other.example/")}
      cachedProvider runtime settings {settingsRevision = 1} >>= expectProtocolFailure,
    testCase "token timeout fails without automatically retrying a consumed code" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorTokenDelayMicros = 11000000}
      exchangeFixture runtime settings >>= expectProtocolFailure
      providerTokenExchangeCount provider >>= (@?= 1),
    testCase "explicit reauthentication rejects absent auth_time" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorOmitClaims = ["auth_time"]}
      verifier <- randomToken
      nonce <- randomToken
      code <- authorizeCode runtime settings verifier nonce
      exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest nonce) (Just fixedNow) >>= expectProtocolFailure,
    testGroup
      "explicit reauthentication rejects stale or future auth_time"
      [ testCase label $ withFixture $ \provider runtime settings -> do
          let value = Number (fromInteger (floor (utcTimeToPOSIXSeconds fixedNow) + offset))
          setProviderBehavior provider defaultProviderBehavior {behaviorClaims = KM.singleton "auth_time" value}
          verifier <- randomToken
          nonce <- randomToken
          code <- authorizeCode runtime settings verifier nonce
          exchangeAuthorizationCode runtime settings fixtureRedirect code verifier (tokenDigest nonce) (Just fixedNow) >>= expectProtocolFailure
      | (label, offset) <- [("stale proof", -61), ("future proof", 61)]
      ],
    testCase "multiple audiences require a matching authorized party" $ withFixture $ \provider runtime settings -> do
      let audience = case object ["aud" .= [providerClientId, "other-client"]] of
            Object o -> maybe Null id (KM.lookup "aud" o)
            _ -> Null
      setProviderBehavior provider defaultProviderBehavior {behaviorClaims = KM.singleton "aud" audience}
      exchangeFixture runtime settings >>= expectProtocolFailure
      setProviderBehavior provider defaultProviderBehavior {behaviorClaims = KM.fromList [("aud", audience), ("azp", String providerClientId)]}
      exchangeFixture runtime settings >>= \result -> expectProtocolSuccess result $ \identity -> verifiedSubject identity @?= "synthetic-alice",
    testCase "UserInfo hints override ID hints only after subject verification" $ withFixture $ \provider runtime settings -> do
      setProviderBehavior provider defaultProviderBehavior {behaviorUserInfo = KM.fromList [("name", String "Updated profile"), ("email_verified", Bool True)]}
      exchangeFixture runtime settings >>= \result -> expectProtocolSuccess result $ \identity -> do
        profileDisplayName (verifiedProfile identity) @?= Just "Updated profile"
        profileEmailVerified (verifiedProfile identity) @?= True,
    testGroup
      "metadata trust boundary"
      [ testCase label $ withFixture $ \provider runtime settings -> do
          setProviderBehavior provider defaultProviderBehavior {behaviorMetadata = KM.singleton key value}
          discoverProvider runtime settings >>= expectProtocolFailure
      | (label, key, value) <- invalidMetadata
      ]
  ]

invalidIdentityClaims :: [(String, K.Key, Value)]
invalidIdentityClaims =
  [ ("wrong issuer", "iss", String "https://other.example/"),
    ("empty subject", "sub", String ""),
    ("wrong audience", "aud", String "other-client"),
    ("wrong azp", "azp", String "other-client"),
    ("future iat", "iat", epochValue 61),
    ("future nbf", "nbf", epochValue 61),
    ("expired exp", "exp", epochValue 0),
    ("wrong nonce", "nonce", String "other-nonce"),
    ("wrong access token hash", "at_hash", String "wrong-hash"),
    ("invalid authentication time", "auth_time", String "not-a-timestamp")
  ]
  where
    epochValue offset = Number (fromInteger (floor (utcTimeToPOSIXSeconds fixedNow) + offset))

invalidMetadata :: [(String, K.Key, Value)]
invalidMetadata =
  [ ("issuer mismatch", "issuer", String "https://other.example/"),
    ("cross-origin authorization endpoint", "authorization_endpoint", String "https://other.example/authorize"),
    ("cross-origin token endpoint", "token_endpoint", String "https://other.example/token"),
    ("cross-origin JWKS", "jwks_uri", String "https://other.example/jwks"),
    ("cross-origin UserInfo", "userinfo_endpoint", String "https://other.example/userinfo"),
    ("cross-origin logout", "end_session_endpoint", String "https://other.example/logout"),
    ("no S256", "code_challenge_methods_supported", asValue ["plain" :: Text]),
    ("no code grant", "response_types_supported", asValue ["token" :: Text]),
    ("no asymmetric signing", "id_token_signing_alg_values_supported", asValue ["HS256" :: Text]),
    ("no Basic authentication", "token_endpoint_auth_methods_supported", asValue ["client_secret_post" :: Text])
  ]
  where
    asValue values = case object ["value" .= values] of
      Object o -> maybe Null id (KM.lookup "value" o)
      _ -> Null
