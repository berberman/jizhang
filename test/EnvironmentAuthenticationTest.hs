{-# LANGUAGE OverloadedStrings #-}

-- | Environment-owned authentication regressions cover restart reconciliation,
-- read-only operator routes and failure atomicity against real persisted grants.
module EnvironmentAuthenticationTest (environmentAuthenticationTests) where

import APITest (setupAdmin, withTestAppServerLoggerEnv)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import qualified Jizhang.API as API
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types (AdminBootstrap (..))
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (pkceChallenge, randomToken)
import Jizhang.Config (EnvironmentAuthentication (..))
import qualified Jizhang.OIDC as O
import Log (Logger, runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.URI (parseSimpleQuery)
import qualified Network.Wai.Handler.Warp as Warp
import OIDCFlowTest (Harness (..), continuation, decodeResponse, expectStatus, getUrl, request)
import OIDCProvider
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Bind desired startup settings to the current loopback provider's issuer.
configuration :: OidcProvider -> EnvironmentAuthentication
configuration provider =
  EnvironmentAuthentication
    True
    True
    True
    "Environment provider"
    (Just $ providerIssuer provider)
    (Just providerClientId)
    (Just providerClientSecret)

-- | Vary only encryption material while retaining the deployment URL trust boundary.
runtimeWithKey :: BS.ByteString -> IO O.AuthRuntime
runtimeWithKey bytes =
  O.newAuthRuntime
    (Just "http://127.0.0.1:8080")
    (Just "http://127.0.0.1:3000/")
    (Just $ TE.decodeUtf8 $ B64.encode bytes)
    ["http://127.0.0.1:3000"]

-- | Reset disposable jizhang_test once, then share connection, lock and signing key
-- across recreations. TEST_DATABASE_URL must not target a concurrently used database.
withEnvironment :: (Logger -> AppEnv -> Harness -> Text -> IO ()) -> IO ()
withEnvironment action = withStdOutLogger $ \logger -> withOidcProvider defaultProviderConfig $ \provider -> do
  runtime <- runtimeWithKey (BS.pack [0 .. 31])
  withTestAppServerLoggerEnv
    logger
    (Just $ AdminBootstrap "operator" "operator-password")
    (\env -> env {appAuthRuntime = runtime, appEnvironmentAuthentication = Just $ configuration provider})
    $ \env clientEnv port manager conn -> do
      token <- setupAdmin clientEnv "operator" "operator-password"
      action logger env (Harness provider manager port conn) token

-- | Rebuild the WAI application without resetting storage or replacing its lock/key.
recreate :: Logger -> AppEnv -> Harness -> (Harness -> IO a) -> IO a
recreate logger env (Harness provider manager _ conn) action = do
  wai <- runLogT "environment-restart" logger maxBound (API.app env)
  Warp.testWithApplication (pure wai) $ \port -> action (Harness provider manager port conn)

send :: Harness -> Maybe Text -> BS.ByteString -> Text -> Value -> IO (HTTP.Response LBS.ByteString)
send h token = sendHeaders h (maybe [] (\t -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)]) token)

-- | Supply bearer or cookie/XSRF headers explicitly; redirects are not followed.
sendHeaders :: Harness -> RequestHeaders -> BS.ByteString -> Text -> Value -> IO (HTTP.Response LBS.ByteString)
sendHeaders (Harness _ manager port _) headers method path value = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
  HTTP.httpLbs
    initial
      { HTTP.method = method,
        HTTP.redirectCount = 0,
        HTTP.requestHeaders = ("Content-Type", "application/json") : headers,
        HTTP.requestBody = HTTP.RequestBodyLBS (encode value)
      }
    manager

admin :: Harness -> Text -> BS.ByteString -> Text -> Value -> IO (HTTP.Response LBS.ByteString)
admin h token = send h (Just token)

-- | Capture both operator cookies and the unprefixed XSRF value for guarded writes.
operatorCookies :: Harness -> IO (BS.ByteString, BS.ByteString)
operatorCookies h = do
  response <-
    send h Nothing "POST" "/admin/auth/session/login" $
      object ["username" .= ("operator" :: Text), "password" .= ("operator-password" :: Text)]
  expectStatus 200 response
  let cookie name = case [ BS.takeWhile (/= 59) value
                         | (header, value) <- HTTP.responseHeaders response,
                           header == "Set-Cookie",
                           (name <> "=") `BS.isPrefixOf` value
                         ] of
        [value] -> pure value
        _ -> fail "Missing operator session cookie"
  jwt <- cookie "JWT-Cookie"
  xsrf <- cookie "XSRF-TOKEN"
  pure (jwt <> "; " <> xsrf, BS.drop 11 xsrf)

settings :: Harness -> Text -> IO Value
settings h token =
  admin h token "GET" "/admin/authentication" Null >>= \response ->
    expectStatus 200 response >> decodeResponse response

field :: Text -> Value -> Value
field name (Object fields) = fromMaybe Null (KM.lookup (K.fromText name) fields)
field _ _ = Null

row :: Connection -> IO (Int64, Maybe Text)
row conn = do
  [value] <- query_ conn "SELECT revision, client_secret_encrypted FROM auth_settings"
  pure value

-- Comparing opaque serialized snapshots keeps ciphertext/token material out of
-- assertion diagnostics while detecting any unintended mutation on failure.
snapshot :: Connection -> IO [Only Text]
snapshot conn =
  query_
    conn
    "SELECT jsonb_build_array((SELECT jsonb_agg(to_jsonb(s)) FROM auth_settings s), (SELECT jsonb_agg(to_jsonb(i) ORDER BY i.id) FROM account_invitations i), (SELECT jsonb_agg(to_jsonb(t) ORDER BY t.id) FROM oidc_transactions t), (SELECT jsonb_agg(to_jsonb(s) ORDER BY s.id) FROM user_sessions s), (SELECT jsonb_agg(to_jsonb(i) ORDER BY i.issuer, i.subject) FROM external_identities i))::text"

-- | Exclude settings so configuration changes can be checked independently of grants.
grantsSnapshot :: Connection -> IO [Only Text]
grantsSnapshot conn =
  query_
    conn
    "SELECT jsonb_build_array((SELECT jsonb_agg(to_jsonb(i) ORDER BY i.id) FROM account_invitations i), (SELECT jsonb_agg(to_jsonb(t) ORDER BY t.id) FROM oidc_transactions t), (SELECT jsonb_agg(to_jsonb(s) ORDER BY s.id) FROM user_sessions s), (SELECT jsonb_agg(to_jsonb(i) ORDER BY i.issuer, i.subject) FROM external_identities i))::text"

assertStartupFailure :: Logger -> AppEnv -> Text -> Assertion
assertStartupFailure logger env expected = do
  result <- try (void $ runLogT "environment-failure" logger maxBound (API.app env)) :: IO (Either SomeException ())
  case result of
    Right () -> assertFailure "Invalid environment configuration returned an application"
    Left err -> do
      let message = T.pack $ displayException err
      assertBool "Startup reports controlled configuration failure" ("Environment authentication configuration failed: " `T.isInfixOf` message)
      assertBool "Startup reports expected safe error" (expected `T.isInfixOf` message)
      forM_ [providerClientSecret, "different-private-secret"] $ \secret ->
        assertBool "Startup error does not disclose secret" (not $ secret `T.isInfixOf` message)

-- | Compare opaque snapshots rather than printing credential-bearing rows on failure.
unchangedFailure :: Logger -> AppEnv -> Harness -> Text -> Assertion
unchangedFailure logger env (Harness _ _ _ conn) expected = do
  before <- snapshot conn
  assertStartupFailure logger env expected
  after <- snapshot conn
  assertBool "Failed startup leaves settings, identities, grants and sessions unchanged" (before == after)

registerLocal :: Harness -> IO Text
registerLocal h = do
  response <- request h "/auth/register" (Auth.RegisterRequest "local" "Local" "local@example.test" "password123")
  expectStatus 200 response
  login <- request h "/auth/login" (Auth.LoginRequest "local" "password123")
  expectStatus 200 login
  Auth.accessToken <$> (decodeResponse login :: IO Auth.LoginResponse)

-- | Provision an enabled SSO target with no local password for invitation tests.
createPending :: Harness -> Text -> IO UUID
createPending h@(Harness _ _ _ conn) token = do
  admin
    h
    token
    "POST"
    "/admin/users"
    ( object
        [ "createUsername" .= ("pending" :: Text),
          "createDisplayName" .= ("Pending" :: Text),
          "createEmail" .= ("pending@example.test" :: Text),
          "createMode" .= ("sso" :: Text),
          "createPassword" .= Null
        ]
    )
    >>= expectStatus 200
  [Only uid] <- query_ conn "SELECT id FROM users WHERE username='pending'"
  pure uid

issue :: Harness -> Text -> UUID -> Text -> IO Text
issue h token uid purpose = do
  response <- admin h token "POST" ("/admin/users/" <> T.pack (show uid) <> "/" <> purpose) Null
  expectStatus 200 response
  DTO.InvitationIssued _ url _ <- decodeResponse response
  maybe (fail "Invitation omitted token") (pure . TE.decodeUtf8) $
    lookup "token" $
      parseSimpleQuery $
        TE.encodeUtf8 $
          T.dropWhile (/= '?') url

inspect :: Harness -> Text -> Int -> Assertion
inspect h token status = request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus status

session :: Harness -> Text -> Int -> Assertion
session h token status = send h (Just token) "GET" "/users/me" Null >>= expectStatus status

start :: Harness -> IO (Text, DTO.OidcStartResponse)
start h = do
  verifier <- randomToken
  response <- request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) Nothing)
  expectStatus 200 response
  begun <- decodeResponse response
  pure (verifier, begun)

-- Complete a flow created before a restart, retaining the browser verifier.
finish :: Harness -> (Text, DTO.OidcStartResponse) -> IO DTO.OidcExchangeResponse
finish h (verifier, DTO.OidcStartResponse flow url) = do
  authorization <- getUrl h (url <> "&decision=allow") False
  expectStatus 303 authorization
  callback <- location authorization
  redirected <- getUrl h callback True
  expectStatus 303 redirected
  frontend <- location redirected
  let callbackQuery = T.dropWhile (/= '?') $ T.dropWhile (/= '#') frontend
  code <-
    maybe (fail "Missing callback handoff") (pure . TE.decodeUtf8) $
      lookup "code" $
        parseSimpleQuery $
          TE.encodeUtf8 callbackQuery
  exchanged <- request h "/auth/oidc/exchange" (DTO.OidcExchangeRequest flow code verifier)
  expectStatus 200 exchanged
  decodeResponse exchanged
  where
    location response = maybe (fail "Missing redirect") (pure . TE.decodeUtf8) (lookup "Location" $ HTTP.responseHeaders response)

badDiscovery :: ProviderBehavior
badDiscovery = defaultProviderBehavior {behaviorMetadata = KM.fromList [("issuer", String "https://wrong.example.test/")]}

environmentAuthenticationTests :: TestTree
environmentAuthenticationTests =
  testGroup
    "Environment authentication"
    [ testCase "startup owns effective options, signed registration and private read-only operator settings" $ withEnvironment $ \_ _ h token -> do
        response <- send h Nothing "GET" "/auth/options" Null
        expectStatus 200 response
        options <- decodeResponse response
        assertBool "effective public options" (options == DTO.AuthOptions True True True "Environment provider")
        effective <- settings h token
        assertEqual "ownership" (Bool True) (field "environmentManaged" effective)
        assertEqual "secret status" (Bool True) (field "hasClientSecret" effective)
        assertEqual "local signup" (Bool True) (field "localRegistrationEnabled" effective)
        assertEqual "SSO signup" (Bool True) (field "ssoRegistrationEnabled" effective)
        let wire = LBS.toStrict $ encode effective
        assertBool "operator response contains no secret" (not $ TE.encodeUtf8 providerClientSecret `BS.isInfixOf` wire)
        proof <- continuation h
        registered <- request h "/auth/oidc/register" (DTO.OidcRegisterRequest proof "alice" "Alice" "alice@example.test")
        expectStatus 200 registered
        sso <- Auth.accessToken <$> (decodeResponse registered :: IO Auth.LoginResponse)
        session h sso 200,
      testCase "identical recreation preserves ciphertext, revision, outstanding flow, invitations and JWTs without discovery" $ withEnvironment $ \logger env h@(Harness provider _ _ conn) token -> do
        local <- registerLocal h
        uid <- createPending h token
        link <- issue h token uid "identity-invitation"
        [Only localId] <- query_ conn "SELECT id FROM users WHERE username='local'"
        reset <- issue h token localId "password-reset"
        begun <- start h
        (cookies, xsrf) <- operatorCookies h
        before <- row conn
        setProviderBehavior provider badDiscovery
        recreate logger env h $ \fresh -> do
          after <- row conn
          assertBool "no-op retains revision and exact ciphertext" (before == after)
          session fresh local 200
          void $ settings fresh token
          sendHeaders fresh [("Cookie", cookies), ("X-XSRF-TOKEN", xsrf)] "GET" "/admin/auth/session/me" Null >>= expectStatus 200
          inspect fresh link 200
          inspect fresh reset 200
          setProviderBehavior provider defaultProviderBehavior
          result <- finish fresh begun
          case result of
            DTO.OidcRegistrationRequired proof _ _ ->
              request
                fresh
                "/auth/oidc/register"
                (DTO.OidcRegisterRequest proof "alice" "Alice" "alice@example.test")
                >>= expectStatus 200
            _ -> assertFailure "Outstanding flow did not survive unchanged startup"
          request fresh "/auth/password-reset" (DTO.PasswordResetRequest reset "replacement-password") >>= expectStatus 204
          request fresh "/auth/login" (Auth.LoginRequest "local" "replacement-password") >>= expectStatus 200
          setProviderBehavior provider defaultProviderBehavior {behaviorSubject = "pending-subject"}
          verifier <- randomToken
          invitationStart <- request fresh "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) (Just link))
          expectStatus 200 invitationStart
          invitationFlow <- decodeResponse invitationStart
          invitationResult <- finish fresh (verifier, invitationFlow)
          case invitationResult of
            DTO.OidcLinkConfirmationRequired proof _ _ -> request fresh "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest proof) >>= expectStatus 200
            _ -> assertFailure "Surviving linking invitation did not complete signed proof"
          inspect fresh link 400
          inspect fresh reset 400,
      testCase "real flag, label, secret and disable changes reconcile once and invalidate only provider-bound grants" $ withEnvironment $ \logger env h@(Harness provider _ _ conn) token -> do
        local <- registerLocal h
        proof <- continuation h
        registered <- request h "/auth/oidc/register" (DTO.OidcRegisterRequest proof "alice" "Alice" "alice@example.test")
        expectStatus 200 registered
        sso <- Auth.accessToken <$> (decodeResponse registered :: IO Auth.LoginResponse)
        uid <- createPending h token
        [Only localId] <- query_ conn "SELECT id FROM users WHERE username='local'"
        reset <- issue h token localId "password-reset"
        link <- issue h token uid "identity-invitation"
        (_, DTO.OidcStartResponse flow _) <- start h
        (revision, ciphertext) <- row conn
        let desired = (configuration provider) {envLocalRegistrationEnabled = False, envSsoRegistrationEnabled = False, envProviderName = "Changed label"}
            changed = env {appEnvironmentAuthentication = Just desired}
        -- Signup/label changes require no discovery, even with a broken provider.
        setProviderBehavior provider badDiscovery
        recreate logger changed h $ \fresh -> do
          (rev, encrypted) <- row conn
          assertEqual "one revision for public change" (revision + 1) rev
          optionsResponse <- send fresh Nothing "GET" "/auth/options" Null
          expectStatus 200 optionsResponse
          options <- decodeResponse optionsResponse
          assertBool "public flags and label reflect environment change" (options == DTO.AuthOptions False False True "Changed label")
          assertBool "equal plaintext preserves ciphertext" (ciphertext == encrypted)
          inspect fresh link 400
          inspect fresh reset 200
          [Only phase] <- query conn "SELECT phase FROM oidc_transactions WHERE id=?" (Only flow) :: IO [Only Text]
          assertEqual "provider flow invalidated" "failed" phase
          session fresh local 200
          session fresh sso 200
          request fresh "/auth/register" (Auth.RegisterRequest "blocked" "Blocked" "blocked@example.test" "password123") >>= expectStatus 403
          request fresh "/auth/login" (Auth.LoginRequest "local" "password123") >>= expectStatus 200
        setProviderBehavior provider defaultProviderBehavior
        (secretLink, secretFlow) <- recreate logger changed h $ \fresh -> do
          begun <- start fresh
          result <- finish fresh begun
          case result of
            DTO.OidcAuthenticated _ -> pure ()
            _ -> assertFailure "Known SSO identity must sign in with public signup closed"
          setProviderBehavior provider defaultProviderBehavior {behaviorSubject = "unregistered-subject"}
          unregistered <- continuation fresh
          request fresh "/auth/oidc/register" (DTO.OidcRegisterRequest unregistered "blocked_sso" "Blocked" "blocked_sso@example.test") >>= expectStatus 403
          setProviderBehavior provider defaultProviderBehavior
          nextLink <- issue fresh token uid "identity-invitation"
          (_, DTO.OidcStartResponse nextFlow _) <- start fresh
          pure (nextLink, nextFlow)
        -- A valid metadata proof is not proof of the supplied client password.
        let rotated = desired {envClientSecret = Just "different-private-secret"}
        (disableLink, disableFlow) <- recreate logger (env {appEnvironmentAuthentication = Just rotated}) h $ \fresh -> do
          (rev, encrypted) <- row conn
          assertEqual "secret change advances once" (revision + 2) rev
          assertBool "secret change encrypts new bytes" (ciphertext /= encrypted)
          inspect fresh secretLink 400
          inspect fresh reset 200
          [Only oldPhase] <- query conn "SELECT phase FROM oidc_transactions WHERE id=?" (Only secretFlow) :: IO [Only Text]
          assertEqual "secret rotation invalidates earlier flow" "failed" oldPhase
          (_, DTO.OidcStartResponse failedFlow url) <- start fresh
          authorization <- getUrl fresh (url <> "&decision=allow") False
          callback <- maybe (fail "Missing provider redirect") (pure . TE.decodeUtf8) (lookup "Location" $ HTTP.responseHeaders authorization)
          getUrl fresh callback True >>= expectStatus 303
          [Only phase] <- query conn "SELECT phase FROM oidc_transactions WHERE id=?" (Only failedFlow) :: IO [Only Text]
          assertEqual "wrong secret cannot complete exchange" "failed" phase
          nextLink <- issue fresh token uid "identity-invitation"
          (_, DTO.OidcStartResponse nextFlow _) <- start fresh
          pure (nextLink, nextFlow)
        let disabled = rotated {envOidcEnabled = False}
        recreate logger (env {appEnvironmentAuthentication = Just disabled}) h $ \fresh -> do
          (rev, _) <- row conn
          assertEqual "disable advances once" (revision + 3) rev
          optionsResponse <- send fresh Nothing "GET" "/auth/options" Null
          expectStatus 200 optionsResponse
          options <- decodeResponse optionsResponse
          assertBool "public options reflect disabled provider" (options == DTO.AuthOptions False False False "Changed label")
          session fresh sso 401
          session fresh local 200
          inspect fresh reset 200
          inspect fresh disableLink 400
          [Only oldPhase] <- query conn "SELECT phase FROM oidc_transactions WHERE id=?" (Only disableFlow) :: IO [Only Text]
          assertEqual "disable invalidates earlier flow" "failed" oldPhase,
      testCase "linked identities prevent issuer/client changes or clear without disturbing grants or sessions" $ withEnvironment $ \logger env h@(Harness provider _ _ _) _ -> do
        proof <- continuation h
        request h "/auth/oidc/register" (DTO.OidcRegisterRequest proof "alice" "Alice" "alice@example.test") >>= expectStatus 200
        void $ start h
        let desired = configuration provider
        forM_
          [ desired {envOidcEnabled = False, envIssuer = Nothing},
            desired {envOidcEnabled = False, envClientId = Nothing},
            desired {envClientId = Just "different-client"},
            desired {envOidcEnabled = False, envIssuer = Just "https://different.example.test/"}
          ]
          $ \value ->
            unchangedFailure logger (env {appEnvironmentAuthentication = Just value}) h "Unlink existing SSO identities",
      testCase "invalid runtime, drafts, decryption and required metadata fail before returning an application" $ withEnvironment $ \logger env h@(Harness provider _ _ _) _ -> do
        void $ start h
        let desired = configuration provider
        forM_
          [ desired {envProviderName = "bad\nlabel"},
            desired {envIssuer = Just "not-a-url"},
            desired {envClientId = Just " "},
            desired {envClientSecret = Nothing}
          ]
          $ \value ->
            unchangedFailure logger (env {appEnvironmentAuthentication = Just value}) h ""
        wrong <- runtimeWithKey (BS.pack [32 .. 63])
        unchangedFailure logger (env {appAuthRuntime = wrong}) h ""
        absent <- O.newAuthRuntime Nothing Nothing Nothing []
        unchangedFailure logger (env {appAuthRuntime = absent}) h ""
        setProviderBehavior provider badDiscovery
        unchangedFailure logger (env {appEnvironmentAuthentication = Just desired {envClientSecret = Just "different-private-secret"}}) h "discovery"
        let unavailable = desired {envIssuer = Just "http://127.0.0.1:1/"}
        unchangedFailure logger (env {appEnvironmentAuthentication = Just unavailable}) h "unavailable",
      testCase "disabled authoritative configuration clears nullable fields and permits partial providers without discovery" $ withEnvironment $ \logger env h@(Harness provider _ _ conn) token -> do
        let partial = (configuration provider) {envOidcEnabled = False, envIssuer = Just "http://127.0.0.1:1/", envClientId = Nothing, envClientSecret = Nothing}
        recreate logger (env {appEnvironmentAuthentication = Just partial}) h $ \fresh -> do
          response <- settings fresh token
          assertEqual "secret cleared, not retained as an admin null draft" (Bool False) (field "hasClientSecret" response)
          [Only cleared] <- query_ conn "SELECT client_id IS NULL AND client_secret_encrypted IS NULL FROM auth_settings"
          assertBool "authoritative nullable fields really clear" cleared,
      testCase "snapshot race rejects stale full snapshot without retry or grant invalidation" $ withEnvironment $ \logger env h@(Harness provider _ _ conn) token -> do
        void $ start h
        local <- registerLocal h
        uid <- createPending h token
        void $ issue h token uid "identity-invitation"
        [Only localId] <- query_ conn "SELECT id FROM users WHERE username='local'"
        reset <- issue h token localId "password-reset"
        grantsBefore <- grantsSnapshot conn
        before <- snapshot conn
        (revision, encrypted) <- row conn
        connectionString <- B8.pack . fromMaybe "dbname=jizhang_test" <$> lookupEnv "TEST_DATABASE_URL"
        called <- newIORef (0 :: Int)
        let clock = do
              count <- atomicModifyIORef' called (\n -> (n + 1, n))
              if count == 0
                then bracket (connectPostgreSQL connectionString) close $ \other ->
                  void $ execute_ other "UPDATE auth_settings SET provider_name='Concurrent writer', revision=revision+1"
                else pure ()
              getCurrentTime
            desired = (configuration provider) {envLocalRegistrationEnabled = False}
        assertStartupFailure
          logger
          ( env
              { appEnvironmentAuthentication = Just desired,
                appAuthRuntime = (appAuthRuntime env) {O.authClock = clock}
              }
          )
          "Authentication settings changed"
        (rev, ciphertext) <- row conn
        assertEqual "only concurrent write commits" (revision + 1) rev
        assertBool "failed apply did not reencrypt" (encrypted == ciphertext)
        [Only name] <- query_ conn "SELECT provider_name FROM auth_settings" :: IO [Only Text]
        assertEqual "concurrent snapshot remains authoritative" "Concurrent writer" name
        [Only waiting] <- query_ conn "SELECT count(*) FROM oidc_transactions WHERE phase='waiting'" :: IO [Only Int]
        assertEqual "preexisting live flow not invalidated" 1 waiting
        readIORef called >>= assertEqual "no automatic apply retry" 1
        after <- snapshot conn
        assertBool "race fixture performed its intended committed change" (before /= after)
        grantsAfter <- grantsSnapshot conn
        assertBool "stale startup preserves every live grant and session byte-for-byte" (grantsBefore == grantsAfter)
        session h local 200
        inspect h reset 200
        [Only untouched] <- query_ conn "SELECT count(*) FROM account_invitations WHERE purpose='oidc_link' AND revoked_at IS NULL AND consumed_at IS NULL" :: IO [Only Int]
        assertEqual "concurrent write does not revoke link" 1 untouched,
      testCase "managed endpoints reject admin writes and spoofing; database hand-back preserves state and unlocks revision saves" $ withEnvironment $ \logger env h@(Harness provider _ _ conn) token -> do
        local <- registerLocal h
        (cookies, xsrf) <- operatorCookies h
        (revision, _) <- row conn
        let draft =
              toJSON $
                DTO.AuthenticationSettingsRequest
                  revision
                  False
                  False
                  True
                  "Admin draft"
                  (Just $ providerIssuer provider)
                  (Just providerClientId)
                  (Just "different-private-secret")
            actions =
              [ ("PUT", "/admin/authentication", draft),
                ("DELETE", "/admin/authentication/provider?revision=" <> T.pack (show revision), Null),
                ("POST", "/admin/authentication/test", draft)
              ]
        before <- snapshot conn
        setProviderBehavior provider badDiscovery
        forM_ actions $ \(method, path, value) -> do
          response <- admin h token method path value
          expectStatus 409 response
          assertEqual "managed safe error" "Authentication settings are managed by environment variables" (HTTP.responseBody response)
          send h Nothing method path value >>= expectStatus 401
          send h (Just local) method path value >>= expectStatus 401
          sendHeaders h [("Cookie", cookies)] method path value >>= expectStatus 401
          sendHeaders h [("Cookie", cookies), ("X-XSRF-TOKEN", xsrf)] method path value >>= expectStatus 409
        case draft of
          Object fields -> admin h token "PUT" "/admin/authentication" (Object $ KM.insert "environmentManaged" (Bool False) fields) >>= expectStatus 400
          _ -> fail "Draft was not an object"
        after <- snapshot conn
        retainedBefore <- row conn
        assertBool "blocked writes do not change stored state" (before == after)
        recreate logger (env {appEnvironmentAuthentication = Nothing}) h $ \fresh -> do
          effective <- settings fresh token
          assertEqual "database hand-back unlocks ownership" (Bool False) (field "environmentManaged" effective)
          retained <- row conn
          assertBool "ownership-only hand-back retains revision and ciphertext" (retained == retainedBefore)
          handedBack <- snapshot conn
          assertBool "hand-back preserves complete settings, identities, grants and sessions" (handedBack == after)
          admin
            fresh
            token
            "PUT"
            "/admin/authentication"
            ( toJSON $
                DTO.AuthenticationSettingsRequest
                  revision
                  True
                  True
                  True
                  "Operator owns again"
                  (Just $ providerIssuer provider)
                  (Just providerClientId)
                  Nothing
            )
            >>= expectStatus 200
          (savedRevision, _) <- row conn
          assertEqual "normal admin explicit save resumes" (revision + 1) savedRevision
    ]
