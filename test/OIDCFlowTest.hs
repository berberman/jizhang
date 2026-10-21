{-# LANGUAGE OverloadedStrings #-}

-- | HTTP lifecycle regressions for browser-bound handoffs, explicit account choices
-- and provider-revision races, backed by signed loopback tokens and PostgreSQL.
module OIDCFlowTest
  ( oidcFlowTests,
    Harness (..),
    withHarness,
    withHarnessConfigured,
    request,
    getUrl,
    expectStatus,
    decodeResponse,
    continuation,
  )
where

import APITest (withTestAppServer)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.OIDC (OIDCAPI)
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (encryptSecret, pkceChallenge, randomToken)
import qualified Jizhang.OIDC as O
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI (parseURI, uriFragment)
import OIDCProvider
import Servant ((:<|>) (..))
import Servant.Client (BaseUrl (..), Scheme (Http), client, mkClientEnv, runClientM)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Provider, HTTP manager, ephemeral application port and live database connection.
data Harness = Harness OidcProvider HTTP.Manager Int Connection

-- | Reset the test database and enable the default signed provider for one case.
withHarness :: (Harness -> IO ()) -> IO ()
withHarness = withHarnessConfigured id

-- | Apply overrides before startup, then persist encrypted provider settings.
-- APITest drops tables in jizhang_test; concurrent users of that database are unsafe.
withHarnessConfigured :: (AppEnv -> AppEnv) -> (Harness -> IO ()) -> IO ()
withHarnessConfigured configure action = withOidcProvider defaultProviderConfig $ \provider -> do
  runtime <-
    O.newAuthRuntime
      (Just "http://127.0.0.1:8080")
      (Just "http://127.0.0.1:3000/")
      (Just $ TE.decodeUtf8 $ B64.encode $ BS.pack [0 .. 31])
      ["http://127.0.0.1:3000"]
  withTestAppServer Nothing (\env -> configure (env {appAuthRuntime = runtime})) $ \_ port manager conn -> do
    encrypted <- case O.authSecretKey runtime of
      Nothing -> assertFailure "Synthetic runtime key unavailable" >> fail "unreachable"
      Just key -> encryptSecret key "auth_settings:1:client_secret" providerClientSecret
    void $
      execute
        conn
        "UPDATE auth_settings SET oidc_enabled = true, sso_registration_enabled = true, provider_name = 'Synthetic provider', issuer = ?, client_id = ?, client_secret_encrypted = ?, revision = revision + 1"
        (providerIssuer provider, providerClientId, encrypted)
    action (Harness provider manager port conn)

-- | Post JSON without automatic redirects or implicit bearer authentication.
request :: (ToJSON a) => Harness -> Text -> a -> IO (HTTP.Response LBS.ByteString)
request (Harness _ manager port _) path body = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
  HTTP.httpLbs
    initial
      { HTTP.method = "POST",
        HTTP.redirectCount = 0,
        HTTP.requestHeaders = [("Content-Type", "application/json")],
        HTTP.requestBody = HTTP.RequestBodyLBS (encode body)
      }
    manager

-- | Rewrite only the port for backend URLs advertised at the fixed deployment base;
-- provider URLs keep their own port, and every redirect remains explicit.
getUrl :: Harness -> Text -> Bool -> IO (HTTP.Response LBS.ByteString)
getUrl (Harness _ manager port _) url local = do
  initial <- HTTP.parseRequest (T.unpack url)
  HTTP.httpLbs initial {HTTP.port = if local then port else HTTP.port initial, HTTP.redirectCount = 0} manager

expectStatus :: Int -> HTTP.Response a -> Assertion
expectStatus expected response = assertEqual "HTTP status" expected (statusCode $ HTTP.responseStatus response)

-- | Suppress decoder diagnostics because authentication bodies can contain secrets.
decodeResponse :: (FromJSON a) => HTTP.Response LBS.ByteString -> IO a
decodeResponse response = case eitherDecode (HTTP.responseBody response) of
  Right value -> pure value
  Left _ -> assertFailure "Unexpected authentication response shape" >> fail "unreachable"

location :: HTTP.Response a -> IO Text
location response = case lookup "Location" (HTTP.responseHeaders response) of
  Just value -> pure (TE.decodeUtf8 value)
  Nothing -> assertFailure "Redirect omitted Location" >> fail "unreachable"

start :: Harness -> Text -> IO DTO.OidcStartResponse
start harness verifier = do
  response <- request harness "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) Nothing)
  expectStatus 200 response
  decodeResponse response

providerCallback :: Harness -> DTO.OidcStartResponse -> IO Text
providerCallback harness (DTO.OidcStartResponse _ url) = do
  response <- getUrl harness (url <> "&decision=allow") False
  expectStatus 303 response
  location response

resolve :: Harness -> DTO.OidcStartResponse -> IO Text
resolve harness begun = do
  callback <- providerCallback harness begun
  response <- getUrl harness callback True
  expectStatus 303 response
  frontend <- location response
  let handoff = do
        uri <- parseURI (T.unpack frontend)
        let fragment = T.pack (uriFragment uri)
            query = T.dropWhile (/= '?') fragment
        lookup "code" (parseSimpleQuery $ TE.encodeUtf8 query)
  case handoff of
    Just value -> pure (TE.decodeUtf8 value)
    Nothing -> assertFailure "Callback did not provide a handoff" >> fail "unreachable"

exchange :: Harness -> UUID -> Text -> Text -> IO (HTTP.Response LBS.ByteString)
exchange harness flow code verifier = request harness "/auth/oidc/exchange" (DTO.OidcExchangeRequest flow code verifier)

-- | Prove the default unknown identity and return its unconsumed decision token;
-- this must not create an account or application session.
continuation :: Harness -> IO Text
continuation harness = do
  verifier <- randomToken
  begun@(DTO.OidcStartResponse flow _) <- start harness verifier
  code <- resolve harness begun
  response <- exchange harness flow code verifier
  expectStatus 200 response
  decoded <- decodeResponse response
  case decoded of
    DTO.OidcRegistrationRequired token _ _ -> pure token
    _ -> assertFailure "Unknown external identity was not left for an explicit decision" >> fail "unreachable"

-- | Require an otherwise empty harness to remain free of account-side writes.
assertNoAccountEffects :: Connection -> Assertion
assertNoAccountEffects conn = do
  counts <- query_ conn "SELECT (SELECT count(*) FROM users), (SELECT count(*) FROM external_identities), (SELECT count(*) FROM user_sessions)" :: IO [(Int, Int, Int)]
  assertEqual "no user, identity or session" [(0, 0, 0)] counts

oidcFlowTests :: TestTree
oidcFlowTests =
  testGroup
    "OIDC authorization lifecycle"
    [ testCase "typed public options distinguish provider availability from independent signup switches" $ withHarness $ \(Harness _ manager port conn) -> do
        let ((options :<|> _) :<|> _) = client (Proxy :: Proxy OIDCAPI)
            fetch = do
              result <- runClientM options (mkClientEnv manager $ BaseUrl Http "127.0.0.1" port "")
              case result of
                Right (DTO.AuthOptions local sso enabled name) -> pure (local, sso, enabled, name)
                Left _ -> assertFailure "Typed options client rejected server contract" >> fail "unreachable"
        initial <- fetch
        assertEqual "configured provider" (True, True, True, "Synthetic provider") initial
        void $ execute_ conn "UPDATE auth_settings SET local_registration_enabled = false, sso_registration_enabled = false, revision = revision + 1"
        changed <- fetch
        assertEqual "signup switches do not disable SSO login" (False, False, True, "Synthetic provider") changed,
      testCase "callback has no account side effects; browser mismatch preserves handoff; exchange and cancellation are one-use" $ withHarness $ \harness@(Harness provider _ _ conn) -> do
        verifier <- randomToken
        begun@(DTO.OidcStartResponse flow _) <- start harness verifier
        code <- resolve harness begun
        assertNoAccountEffects conn
        other <- randomToken
        refused <- exchange harness flow code other
        expectStatus 400 refused
        assertNoAccountEffects conn
        accepted <- exchange harness flow code verifier
        expectStatus 200 accepted
        decoded <- decodeResponse accepted
        token <- case decoded of
          DTO.OidcRegistrationRequired value _ True -> pure value
          _ -> assertFailure "Expected registration decision without session" >> fail "unreachable"
        assertNoAccountEffects conn
        exchange harness flow code verifier >>= expectStatus 400
        request harness "/auth/oidc/cancel" (DTO.OidcContinuationRequest token) >>= expectStatus 204
        request harness "/auth/oidc/register" (DTO.OidcRegisterRequest token "alice" "Alice" "alice@example.test") >>= expectStatus 400
        providerTokenExchangeCount provider >>= assertEqual "single provider exchange" 1,
      testCase "registration consumes once, keeps password null and provider verification false; later login preserves local profile" $ withHarness $ \harness@(Harness provider _ _ conn) -> do
        token <- continuation harness
        response <- request harness "/auth/oidc/register" (DTO.OidcRegisterRequest token "chosen_alice" "Chosen Alice" "chosen@example.test")
        expectStatus 200 response
        request harness "/auth/oidc/register" (DTO.OidcRegisterRequest token "different" "Different" "different@example.test") >>= expectStatus 400
        state <- query_ conn "SELECT username, display_name, email, email_verified, password_hash IS NULL FROM users" :: IO [(Text, Text, Text, Bool, Bool)]
        assertEqual "editable profile and no invented password" [("chosen_alice", "Chosen Alice", "chosen@example.test", False, True)] state
        setProviderBehavior provider defaultProviderBehavior
        verifier <- randomToken
        begun@(DTO.OidcStartResponse flow _) <- start harness verifier
        code <- resolve harness begun
        result <- exchange harness flow code verifier
        expectStatus 200 result
        decoded <- decodeResponse result
        case decoded of
          DTO.OidcAuthenticated _ -> pure ()
          _ -> assertFailure "Linked identity did not authenticate"
        after <- query_ conn "SELECT username, display_name, email, email_verified, password_hash IS NULL FROM users" :: IO [(Text, Text, Text, Bool, Bool)]
        assertEqual "repeat SSO does not overwrite profile or password" state after,
      testCase "same email never auto-links; explicit password link preserves account UUID" $ withHarness $ \harness@(Harness _ manager port conn) -> do
        registration <- request harness "/auth/register" (Auth.RegisterRequest "alice" "Alice" "alice@example.test" "password123")
        expectStatus 200 registration
        before <- query_ conn "SELECT id FROM users WHERE username = 'alice'" :: IO [Only UUID]
        token <- continuation harness
        links <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "email match did not link" [Only 0] links
        linked <- request harness "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "password123")
        expectStatus 200 linked
        session <- decodeResponse linked :: IO Auth.LoginResponse
        after <- query_ conn "SELECT id FROM users WHERE username = 'alice'" :: IO [Only UUID]
        assertEqual "explicit link retained account UUID" before after
        protected <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/users/me")
        HTTP.httpLbs protected {HTTP.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 (Auth.accessToken session))]} manager >>= expectStatus 200
        linksAfter <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "exactly one identity" [Only 1] linksAfter,
      testCase "five failed local proofs exhaust only the pending continuation" $ withHarness $ \harness@(Harness _ _ _ conn) -> do
        request harness "/auth/register" (Auth.RegisterRequest "alice" "Alice" "alice@example.test" "password123") >>= expectStatus 200
        token <- continuation harness
        forM_ [1 .. 5 :: Int] $ \_ -> do
          response <- request harness "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "wrong-password")
          expectStatus 400 response
        request harness "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "password123") >>= expectStatus 400
        counts <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "failed proofs did not link" [Only 0] counts
        request harness "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectStatus 200,
      testCase "signup switch changed after proof blocks finalization without consuming editable decision" $ withHarness $ \harness@(Harness _ _ _ conn) -> do
        token <- continuation harness
        void $ execute_ conn "UPDATE auth_settings SET sso_registration_enabled = false"
        request harness "/auth/oidc/register" (DTO.OidcRegisterRequest token "alice" "Alice" "alice@example.test") >>= expectStatus 403
        assertNoAccountEffects conn
        void $ execute_ conn "UPDATE auth_settings SET sso_registration_enabled = true"
        request harness "/auth/oidc/register" (DTO.OidcRegisterRequest token "alice" "Alice" "alice@example.test") >>= expectStatus 200,
      testCase "provider revision change during single exchange prevents callback finalization" $ withHarness $ \harness@(Harness provider _ _ conn) -> do
        verifier <- randomToken
        begun <- start harness verifier
        callback <- providerCallback harness begun
        barrier <- pauseTokenResponses provider
        finished <- newEmptyMVar
        bracket (forkFinally (getUrl harness callback True) $ putMVar finished) killThread $ \_ -> do
          arrived <- timeout 10000000 (awaitTokenRequest barrier)
          assertEqual "provider exchange reached barrier" (Just ()) arrived
          void $ execute_ conn "UPDATE auth_settings SET revision = revision + 1"
          releaseTokenResponse barrier
          result <- timeout 10000000 (takeMVar finished)
          case result of
            Just (Right response) -> do
              expectStatus 303 response
              target <- location response
              assertBool "safe revision error" ("error=configuration_changed" `T.isInfixOf` target)
            _ -> assertFailure "Callback did not finish after settings change"
        assertNoAccountEffects conn
        providerTokenExchangeCount provider >>= assertEqual "never re-exchanged provider code" 1,
      testCase "raw duplicate and valueless callback parameters are rejected before code exchange" $ withHarness $ \harness@(Harness provider _ port _) -> do
        forM_ ["state&state=valid&code=code", "state=valid&code&code=code", "state=valid&code=code&error", "state=valid&code=code&iss", "state=valid&code=code&iss=x&iss=y"] $ \query -> do
          response <- getUrl harness ("http://127.0.0.1:" <> T.pack (show port) <> "/auth/oidc/callback?" <> query) True
          expectStatus 400 response
          assertEqual "authentication not cached" (Just "no-store") (lookup "Cache-Control" $ HTTP.responseHeaders response)
          assertEqual "authentication not referred" (Just "no-referrer") (lookup "Referrer-Policy" $ HTTP.responseHeaders response)
        providerTokenExchangeCount provider >>= assertEqual "ambiguous callback never exchanged" 0
    ]
