{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A real, loopback-only OIDC server for protocol tests and browser acceptance.
-- Keys and credentials are synthetic; never use this provider in production.
module OIDCProvider
  ( ProviderConfig (..),
    defaultProviderConfig,
    ProviderBehavior (..),
    defaultProviderBehavior,
    OidcProvider,
    providerIssuer,
    providerClientId,
    providerClientSecret,
    withOidcProvider,
    runOidcProvider,
    setProviderBehavior,
    rotateProviderKey,
    providerTokenExchangeCount,
    TokenBarrier,
    pauseTokenResponses,
    awaitTokenRequest,
    releaseTokenResponse,
    signLogoutToken,
    postBackchannelLogout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Jizhang.Auth.Security (pkceChallenge, randomToken, secureEqual, tokenDigest)
import qualified Jose.Jwa as Jwa
import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

providerClientId, providerClientSecret :: Text
providerClientId = "jizhang-smoke"
providerClientSecret = "synthetic-oidc-client-secret"

-- | Registered callback destinations and an independent clock for code/claim ages.
-- HTTP fixtures must keep this clock aligned with the consuming runtime.
data ProviderConfig = ProviderConfig
  { providerRedirectUri :: Text,
    providerPostLogoutUri :: Text,
    providerBackchannelUri :: Text,
    providerClock :: IO UTCTime
  }

-- | Fixed application URLs for browser runs; HTTP tests rewrite backend ports.
defaultProviderConfig :: ProviderConfig
defaultProviderConfig =
  ProviderConfig
    "http://127.0.0.1:8080/auth/oidc/callback"
    "http://127.0.0.1:8080/auth/oidc/logout/callback"
    "http://127.0.0.1:8080/auth/oidc/backchannel-logout"
    getCurrentTime

-- | Claim overrides take precedence, then omissions remove keys. Token behavior is
-- read after the response barrier, allowing a test to alter an in-flight exchange.
data ProviderBehavior = ProviderBehavior
  { behaviorSubject :: Text,
    behaviorClaims :: Object,
    behaviorOmitClaims :: [Text],
    behaviorUserInfo :: Object,
    behaviorMetadata :: Object,
    behaviorTokenError :: Maybe Text,
    behaviorTokenRedirect :: Maybe Text,
    behaviorTokenDelayMicros :: Int,
    behaviorDiscoveryDelayMicros :: Int,
    behaviorDiscoveryRedirect :: Maybe Text,
    behaviorMalformedToken :: Bool,
    behaviorAuthenticationAgeSeconds :: Integer
  }

defaultProviderBehavior :: ProviderBehavior
defaultProviderBehavior = ProviderBehavior "synthetic-alice" KM.empty [] KM.empty KM.empty Nothing Nothing 0 0 Nothing False 0

-- | Arrival and release latches shared by all successful token exchanges while set.
data TokenBarrier = TokenBarrier (MVar ()) (MVar ())

-- | Authorization-time bindings survive until one valid redemption consumes them.
data Code = Code
  { codeRedirect :: ByteString,
    codeChallenge :: Text,
    codeNonce :: Text,
    codeSubject :: Text,
    codeCreated :: Integer,
    codeAuthTime :: Integer
  }

-- | One MVar serializes code consumption, key rotation and exchange accounting.
data ProviderState = ProviderState
  { stateCodes :: Map.Map ByteString Code,
    stateAccessTokens :: Map.Map ByteString Text,
    stateKey :: (Jwk.Jwk, Jwk.Jwk),
    stateExchanges :: Int
  }

-- | Per-server state; behavior and barrier controls are shared across its requests.
data OidcProvider = OidcProvider
  { providerIssuer :: Text,
    fixtureConfig :: ProviderConfig,
    fixtureState :: MVar ProviderState,
    fixtureBehavior :: IORef ProviderBehavior,
    fixtureBarrier :: IORef (Maybe TokenBarrier)
  }

newProvider :: Int -> ProviderConfig -> IO OidcProvider
newProvider port config = do
  keys <- generateKeys
  state <- newMVar (ProviderState Map.empty Map.empty keys 0)
  behavior <- newIORef defaultProviderBehavior
  barrier <- newIORef Nothing
  pure $ OidcProvider ("http://127.0.0.1:" <> T.pack (show port) <> "/application/o/jizhang/") config state behavior barrier

generateKeys :: IO (Jwk.Jwk, Jwk.Jwk)
generateKeys = do
  kid <- randomToken
  -- KeyId has a FromJSON instance but its constructor is intentionally hidden.
  case fromJSON (String kid) of
    Error _ -> ioError $ userError "Fixture key identifier construction failed"
    Success keyId -> Jwk.generateRsaKeyPair 256 keyId Jwk.Sig (Just $ Jwa.Signed Jwa.RS256)

-- | Scope a loopback server with fresh keys/state to the callback lifetime; the
-- initialization latch prevents requests from observing a partially built provider.
withOidcProvider :: ProviderConfig -> (OidcProvider -> IO a) -> IO a
withOidcProvider config action = do
  slot <- newEmptyMVar
  let app request respond = do
        provider <- readMVar slot
        providerApp provider request respond
  Warp.testWithApplicationSettings fixtureSettings (pure app) $ \port -> do
    provider <- newProvider port config
    putMVar slot provider
    action provider

-- | Run the interactive synthetic provider until the server is stopped externally.
runOidcProvider :: Int -> IO ()
runOidcProvider port = do
  provider <- newProvider port defaultProviderConfig
  putStrLn $ "Synthetic OIDC provider: " <> T.unpack (providerIssuer provider)
  Warp.runSettings (Warp.setPort port fixtureSettings) (providerApp provider)

-- Never let Warp's exception renderer dump a callback or HTTP request carrying
-- state, code, ID-token hint or a back-channel form into terminal output.
fixtureSettings :: Warp.Settings
fixtureSettings =
  Warp.setOnException (\_ _ -> hPutStrLn stderr "Synthetic OIDC provider request failed") $
    Warp.setHost "127.0.0.1" Warp.defaultSettings

-- | Replace all fault controls, not just changed fields; issued codes remain intact.
setProviderBehavior :: OidcProvider -> ProviderBehavior -> IO ()
setProviderBehavior provider = writeIORef (fixtureBehavior provider)

-- | Replace the signing pair without clearing codes or access tokens, so consumers
-- must refresh cached JWKS when the newly generated key identifier appears.
rotateProviderKey :: OidcProvider -> IO ()
rotateProviderKey provider = do
  key <- generateKeys
  modifyMVar_ (fixtureState provider) $ \s -> pure s {stateKey = key}

-- | Count every token endpoint attempt, including rejected grants, to detect retries.
providerTokenExchangeCount :: OidcProvider -> IO Int
providerTokenExchangeCount provider = stateExchanges <$> readMVar (fixtureState provider)

-- | Install a fresh latch after valid code consumption but before response behavior.
-- Release it during exception cleanup to avoid leaving HTTP workers blocked.
pauseTokenResponses :: OidcProvider -> IO TokenBarrier
pauseTokenResponses provider = do
  barrier <- TokenBarrier <$> newEmptyMVar <*> newEmptyMVar
  writeIORef (fixtureBarrier provider) (Just barrier)
  pure barrier

-- | Wait for arrival without consuming the signal; apply a timeout at the call site.
awaitTokenRequest :: TokenBarrier -> IO ()
awaitTokenRequest (TokenBarrier arrived _) = readMVar arrived

-- | Open the release latch idempotently; it stays open until a new barrier is installed.
releaseTokenResponse :: TokenBarrier -> IO ()
releaseTokenResponse (TokenBarrier _ release) = void $ tryPutMVar release ()

waitTokenBarrier :: OidcProvider -> IO ()
waitTokenBarrier provider = do
  barrier <- readIORef (fixtureBarrier provider)
  case barrier of
    Nothing -> pure ()
    Just (TokenBarrier arrived release) -> do
      void $ tryPutMVar arrived ()
      readMVar release

providerApp :: OidcProvider -> Application
providerApp provider request respond = do
  behavior <- readIORef (fixtureBehavior provider)
  let path = pathInfo request
      get = requestMethod request == methodGet
      post = requestMethod request == methodPost
      json = respond . jsonResponse status200
      bad = respond $ jsonResponse status400 $ object ["error" .= ("invalid_request" :: Text)]
  case path of
    ["application", "o", "jizhang", ".well-known", "openid-configuration"] | get -> do
      threadDelay $ max 0 (behaviorDiscoveryDelayMicros behavior)
      case behaviorDiscoveryRedirect behavior of
        Just location -> respond $ redirect location
        Nothing -> json $ Object $ KM.union (behaviorMetadata behavior) (metadata provider)
    ["application", "o", "jizhang", "jwks"] | get -> do
      key <- fst . stateKey <$> readMVar (fixtureState provider)
      json $ object ["keys" .= [key]]
    ["application", "o", "jizhang", "authorize"] | get -> authorize provider request respond
    ["application", "o", "jizhang", "token"] | post -> token provider request respond
    ["application", "o", "jizhang", "userinfo"] | get -> do
      s <- readMVar (fixtureState provider)
      let bearer = lookup hAuthorization (requestHeaders request) >>= BS.stripPrefix "Bearer "
      case bearer >>= (`Map.lookup` stateAccessTokens s) of
        Nothing -> respond $ jsonResponse status401 $ object ["error" .= ("invalid_token" :: Text)]
        Just subject -> json $ Object $ KM.union (behaviorUserInfo behavior) (profile subject)
    ["application", "o", "jizhang", "end-session"] | get -> logout provider request respond
    ["controls", "rotate"] | post -> rotateProviderKey provider >> respond (redirect "/")
    ["controls", "backchannel"] | post -> do
      form <- parseSimpleQuery . LBS.toStrict <$> strictRequestBody request
      let subject = TE.decodeUtf8 $ fromMaybe "synthetic-alice" (one "subject" form)
      result <- postBackchannelLogout provider $ KM.fromList [("sub", String subject)]
      respond $ jsonResponse status200 $ object ["status" .= result]
    [] | get -> respond $ htmlResponse controlsPage
    _ -> bad

metadata :: OidcProvider -> Object
metadata provider = case object
  [ "issuer" .= providerIssuer provider,
    "authorization_endpoint" .= endpoint provider "authorize",
    "token_endpoint" .= endpoint provider "token",
    "jwks_uri" .= endpoint provider "jwks",
    "userinfo_endpoint" .= endpoint provider "userinfo",
    "end_session_endpoint" .= endpoint provider "end-session",
    "response_types_supported" .= ["code" :: Text],
    "grant_types_supported" .= ["authorization_code" :: Text],
    "subject_types_supported" .= ["public" :: Text],
    "id_token_signing_alg_values_supported" .= ["RS256" :: Text],
    "token_endpoint_auth_methods_supported" .= ["client_secret_basic" :: Text],
    "code_challenge_methods_supported" .= ["S256" :: Text],
    "scopes_supported" .= ["openid" :: Text, "profile", "email"],
    "backchannel_logout_supported" .= True,
    "backchannel_logout_session_supported" .= True
  ] of
  Object result -> result
  _ -> KM.empty

endpoint :: OidcProvider -> Text -> Text
endpoint provider suffix = providerIssuer provider <> suffix

-- | Reject absent, empty or duplicate protocol parameters rather than choosing one.
one :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
one name parameters = case [v | (k, v) <- parameters, k == name] of
  [value] | not (BS.null value) -> Just value
  _ -> Nothing

queryPairs :: Request -> [(ByteString, ByteString)]
queryPairs request = [(key, fromMaybe "" value) | (key, value) <- queryString request]

-- | Validate registered redirect/PKCE bindings before presenting synthetic consent.
-- Positive max_age can refresh auth_time; zero intentionally leaves old proof intact.
authorize :: OidcProvider -> Application
authorize provider request respond = do
  behavior <- readIORef (fixtureBehavior provider)
  let q = queryPairs request
      valid = do
        client <- one "client_id" q
        uri <- one "redirect_uri" q
        response <- one "response_type" q
        scope <- one "scope" q
        challenge <- one "code_challenge" q
        challengeMethod <- one "code_challenge_method" q
        nonce <- one "nonce" q
        state <- one "state" q
        if client == TE.encodeUtf8 providerClientId
          && uri == TE.encodeUtf8 (providerRedirectUri $ fixtureConfig provider)
          && response == "code"
          && scope == "openid profile email"
          && challengeMethod == "S256"
          && BS.length challenge == 43
          then Just (uri, challenge, nonce, state)
          else Nothing
  case valid of
    Nothing -> respond $ jsonResponse status400 $ object ["error" .= ("invalid_request" :: Text)]
    Just (uri, challenge, nonce, state) -> case one "decision" q of
      Nothing -> respond $ htmlResponse $ authorizationPage q
      Just "deny" -> respond $ redirectQuery uri [("error", "access_denied"), ("state", state), ("iss", TE.encodeUtf8 $ providerIssuer provider)]
      Just "allow" -> do
        let subject = case one "subject" q of
              Just "synthetic-bob" -> "synthetic-bob"
              Just "synthetic-alice" -> "synthetic-alice"
              Just "synthetic-carol" -> "synthetic-carol"
              Just "synthetic-dave" -> "synthetic-dave"
              _ -> behaviorSubject behavior
        code <- TE.encodeUtf8 <$> randomToken
        now <- nowSeconds provider
        let age = max 0 (behaviorAuthenticationAgeSeconds behavior)
            maximumAge = one "max_age" q >>= readMaybe . T.unpack . TE.decodeUtf8
            refreshed = maybe False (\limit -> limit > 0 && age > limit) maximumAge
            authTime = if refreshed then now else now - age
        modifyMVar_ (fixtureState provider) $ \s -> pure s {stateCodes = Map.insert code (Code uri (TE.decodeUtf8 challenge) (TE.decodeUtf8 nonce) subject now authTime) (stateCodes s)}
        respond $ redirectQuery uri [("code", code), ("state", state), ("iss", TE.encodeUtf8 $ providerIssuer provider)]
      _ -> respond $ jsonResponse status400 $ object ["error" .= ("invalid_request" :: Text)]

-- | Consume a valid code atomically before blocking or injecting transport failures;
-- a failed response therefore cannot make the same grant redeemable again.
token :: OidcProvider -> Application
token provider request respond = do
  form <- parseSimpleQuery . LBS.toStrict <$> strictRequestBody request
  let expectedBasic = "Basic " <> B64.encode (urlEncode True (TE.encodeUtf8 providerClientId) <> ":" <> urlEncode True (TE.encodeUtf8 providerClientSecret))
      authorized = case [value | (name, value) <- requestHeaders request, name == hAuthorization] of
        [value] -> secureEqual value expectedBasic
        _ -> False
  now <- nowSeconds provider
  result <- modifyMVar (fixtureState provider) $ \s -> do
    let selected = do
          value <- one "code" form
          code <- Map.lookup value (stateCodes s)
          verifier <- one "code_verifier" form
          uri <- one "redirect_uri" form
          grant <- one "grant_type" form
          if authorized
            && grant == "authorization_code"
            && uri == codeRedirect code
            && now < codeCreated code + 600
            && secureEqual (tokenDigest $ pkceChallenge $ TE.decodeUtf8 verifier) (tokenDigest $ codeChallenge code)
            then Just (value, code)
            else Nothing
        counted = s {stateExchanges = stateExchanges s + 1}
    case selected of
      Nothing -> pure (counted, Nothing)
      Just (value, code) -> pure (counted {stateCodes = Map.delete value (stateCodes s)}, Just code)
  case result of
    Nothing -> respond $ jsonResponse status400 $ object ["error" .= ("invalid_grant" :: Text)]
    Just code -> do
      waitTokenBarrier provider
      behavior <- readIORef (fixtureBehavior provider)
      threadDelay $ max 0 (behaviorTokenDelayMicros behavior)
      case (behaviorTokenRedirect behavior, behaviorTokenError behavior) of
        (Just location, _) -> respond $ redirect location
        (_, Just category) -> respond $ jsonResponse status400 $ object ["error" .= category]
        _ | behaviorMalformedToken behavior -> respond $ responseLBS status200 [(hContentType, "application/json")] "not-json"
        _ -> do
          access <- randomToken
          let base =
                KM.union (profile $ codeSubject code) $
                  KM.fromList
                    [ ("iss", String $ providerIssuer provider),
                      ("aud", String providerClientId),
                      ("iat", toJSON now),
                      ("exp", toJSON $ now + 300),
                      ("nonce", String $ codeNonce code),
                      ("sid", String $ "sid-" <> codeSubject code),
                      ("auth_time", toJSON $ codeAuthTime code),
                      ("at_hash", String $ accessHash access)
                    ]
              overridden = KM.union (behaviorClaims behavior) base
              claims = foldr (KM.delete . K.fromText) overridden (behaviorOmitClaims behavior)
          jwt <- signClaims provider claims
          modifyMVar_ (fixtureState provider) $ \s -> pure s {stateAccessTokens = Map.insert (TE.encodeUtf8 access) (codeSubject code) (stateAccessTokens s)}
          respond $
            jsonResponse status200 $
              object
                ["access_token" .= access, "id_token" .= jwt, "token_type" .= ("Bearer" :: Text), "expires_in" .= (300 :: Int)]

profile :: Text -> Object
profile subject =
  KM.fromList
    [ ("sub", String subject),
      ("preferred_username", String $ if bob then "bob" else "alice"),
      ("name", String $ if bob then "Synthetic Bob" else "Synthetic Alice"),
      ("email", String $ if bob then "bob@example.test" else "alice@example.test"),
      ("email_verified", Bool False)
    ]
  where
    bob = subject == "synthetic-bob"

accessHash :: Text -> Text
accessHash = TE.decodeUtf8 . BS.takeWhile (/= 61) . B64URL.encode . BS.take 16 . tokenDigest

-- | JWT and grant ages use whole POSIX seconds, independent of delay microseconds.
nowSeconds :: OidcProvider -> IO Integer
nowSeconds provider = floor . utcTimeToPOSIXSeconds <$> providerClock (fixtureConfig provider)

signClaims :: OidcProvider -> Object -> IO Text
signClaims provider claims = do
  key <- snd . stateKey <$> readMVar (fixtureState provider)
  signed <- Jwt.encode [key] (Jwt.JwsEncoding Jwa.RS256) (Jwt.Claims $ LBS.toStrict $ encode $ Object claims)
  case signed of
    Right jwt -> pure $ TE.decodeUtf8 $ Jwt.unJwt jwt
    Left _ -> ioError $ userError "Synthetic provider signing failed"

-- | Overrides can intentionally remove normative claims by supplying invalid
-- values; unlike ID tokens, valid logout tokens deliberately have no exp.
signLogoutToken :: OidcProvider -> Object -> IO Text
signLogoutToken provider overrides = do
  now <- nowSeconds provider
  jti <- randomToken
  let base =
        KM.fromList
          [ ("iss", String $ providerIssuer provider),
            ("aud", String providerClientId),
            ("iat", toJSON now),
            ("jti", String jti),
            ("events", object ["http://schemas.openid.net/event/backchannel-logout" .= object []])
          ]
  signClaims provider $ KM.union overrides base

-- | Post a freshly signed form token to the registered back-channel URL and return
-- its status without redirects; transport failures omit request/token diagnostics.
postBackchannelLogout :: OidcProvider -> Object -> IO Int
postBackchannelLogout provider claims = do
  jwt <- signLogoutToken provider claims
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  initial <- HTTP.parseRequest $ T.unpack $ providerBackchannelUri $ fixtureConfig provider
  let request =
        HTTP.urlEncodedBody
          [("logout_token", TE.encodeUtf8 jwt)]
          initial
            { HTTP.method = methodPost,
              HTTP.redirectCount = 0
            }
  (statusCode . HTTP.responseStatus <$> HTTP.httpNoBody request manager) `catch` transportFailure
  where
    transportFailure :: HTTP.HttpException -> IO Int
    transportFailure _ = ioError $ userError "Synthetic provider back-channel request failed"

logout :: OidcProvider -> Application
logout provider request respond = do
  let q = queryPairs request
      valid = do
        client <- one "client_id" q
        uri <- one "post_logout_redirect_uri" q
        state <- one "state" q
        hint <- one "id_token_hint" q
        if client == TE.encodeUtf8 providerClientId && uri == TE.encodeUtf8 (providerPostLogoutUri $ fixtureConfig provider)
          then Just (uri, state, hint)
          else Nothing
  case valid of
    Nothing -> respond $ jsonResponse status400 $ object ["error" .= ("invalid_request" :: Text)]
    Just (uri, state, hint) -> do
      key <- fst . stateKey <$> readMVar (fixtureState provider)
      verified <- Jwt.decode [key] (Just $ Jwt.JwsEncoding Jwa.RS256) hint
      case verified of
        Right (Jwt.Jws _) -> case one "decision" q of
          Just "allow" -> respond $ redirectQuery uri [("state", state)]
          Just "deny" -> respond $ redirectQuery uri [("state", state), ("error", "access_denied")]
          _ -> respond $ htmlResponse $ "<h1>Sign out of synthetic provider?</h1><form method=get>" <> hiddenFields q <> "<button name=decision value=allow>Sign out</button><button name=decision value=deny>Cancel</button></form>"
        _ -> respond $ jsonResponse status400 $ object ["error" .= ("invalid_request" :: Text)]

jsonResponse :: Status -> Value -> Response
jsonResponse status value = responseLBS status [(hContentType, "application/json"), (hCacheControl, "no-store")] (encode value)

redirect :: Text -> Response
redirect location = responseLBS status303 [(hLocation, TE.encodeUtf8 location), (hCacheControl, "no-store")] ""

redirectQuery :: ByteString -> [(ByteString, ByteString)] -> Response
redirectQuery uri fields = redirect $ TE.decodeUtf8 $ uri <> renderSimpleQuery True fields

htmlResponse :: Text -> Response
htmlResponse body =
  responseLBS status200 [(hContentType, "text/html; charset=utf-8"), (hCacheControl, "no-store"), ("Referrer-Policy", "no-referrer")] $
    LBS.fromStrict $
      TE.encodeUtf8 $
        "<!doctype html><meta name=viewport content='width=device-width,initial-scale=1'><title>Synthetic OIDC provider</title><style>body{font:18px system-ui;max-width:48rem;margin:2rem auto;padding:1rem}button,select{font:inherit;margin:12px;padding:12px}label{display:block;margin:12px}</style>" <> body

escapeHtml :: Text -> Text
escapeHtml = T.concatMap $ \c -> case c of
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  _ -> T.singleton c

hiddenFields :: [(ByteString, ByteString)] -> Text
hiddenFields parameters =
  T.concat
    [ "<input type=hidden name=\"" <> escapeHtml (TE.decodeUtf8 name) <> "\" value=\"" <> escapeHtml (TE.decodeUtf8 value) <> "\">"
    | (name, value) <- parameters,
      name /= "decision",
      name /= "subject"
    ]

authorizationPage :: [(ByteString, ByteString)] -> Text
authorizationPage parameters =
  "<h1>Synthetic provider consent</h1><p>This test server uses synthetic identities only.</p><form method=get>"
    <> hiddenFields parameters
    <> "<label>Identity<select name=subject><option>synthetic-alice</option><option>synthetic-bob</option><option>synthetic-carol</option><option>synthetic-dave</option></select></label><button name=decision value=allow>Authorize</button><button name=decision value=deny>Deny</button></form>"

controlsPage :: Text
controlsPage = "<h1>Synthetic OIDC provider</h1><p>Start sign-in from the application. These controls operate on synthetic sessions only.</p><form action=/controls/rotate method=post><button>Rotate signing key</button></form><form action=/controls/backchannel method=post><label>Terminate subject<select name=subject><option>synthetic-alice</option><option>synthetic-bob</option></select></label><button>Send signed back-channel logout</button></form>"
