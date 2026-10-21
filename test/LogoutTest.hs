{-# LANGUAGE OverloadedStrings #-}

-- | Logout regressions distinguish local session revocation, one-use provider
-- handoffs and signed back-channel fences, using the destructive OIDC harness.
module LogoutTest (logoutTests) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (void)
import Data.Aeson (ToJSON, Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), diffTimeToPicoseconds, getCurrentTime, picosecondsToDiffTime)
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (pkceChallenge, randomToken)
import Jizhang.API.App (AppEnv (appAuthRuntime))
import qualified Jizhang.OIDC as O
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI (parseURI, uriFragment)
import OIDCFlowTest
import OIDCProvider
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

logoutTests :: TestTree
logoutTests =
  testGroup
    "Application and provider logout"
    [ testCase "application logout revokes only its captured session and fails bound authorization" $ withHarness $ \h@(Harness provider _ _ conn) -> do
        first <- localAccount h
        second <- localLogin h
        verifier <- randomToken
        started <- protected h first "/users/me/oidc/start" (DTO.AccountOidcStartRequest DTO.LinkIdentity $ pkceChallenge verifier)
        expectStatus 200 started
        DTO.OidcStartResponse _ url <- decodeResponse started
        protected h first "/users/me/logout" Null >>= expectStatus 204
        active h first 401
        active h second 200
        authorization <- getUrl h (url <> "&decision=allow") False
        location authorization >>= \callback -> getUrl h callback True >>= expectStatus 303
        providerTokenExchangeCount provider >>= assertEqual "revoked bound flow never exchanges provider code" 0
        phases <- query_ conn "SELECT phase FROM oidc_transactions" :: IO [Only Text]
        assertEqual "bound request cannot resume" [Only "failed"] phases,
      testCase "provider logout revokes immediately, uses one-use server handoff, and cancel cannot restore session" $ withHarness $ \h -> do
        local <- localAccount h
        sso <- linkLocal h
        response <- protected h sso "/users/me/oidc/logout" Null
        expectStatus 200 response
        DTO.ProviderLogoutResponse url <- decodeResponse response
        assertBool "application JSON contains only API handoff, never provider hint" ("http://127.0.0.1:8080/auth/oidc/logout?token=" `T.isPrefixOf` url)
        active h sso 401
        active h local 200
        redirect <- getUrl h url True
        expectStatus 303 redirect
        providerUrl <- location redirect
        assertBool "hint is used only in backend provider redirect" ("id_token_hint=" `T.isInfixOf` providerUrl)
        getUrl h url True >>= expectStatus 400
        canceled <- getUrl h (providerUrl <> "&decision=deny") False
        expectStatus 303 canceled
        callback <- location canceled
        getUrl h (callback <> "&state") True >>= expectStatus 400
        finished <- getUrl h callback True
        expectStatus 303 finished
        location finished >>= assertEqual "fixed login destination" "http://127.0.0.1:3000/#/login?signedOut=1"
        getUrl h callback True >>= expectStatus 400
        active h sso 401
        active h local 200,
      testCase "provider logout accepts a nanosecond clock and returns a usable handoff" $ do
        time <- getCurrentTime >>= newIORef
        let configure env = env {appAuthRuntime = (appAuthRuntime env) {O.authClock = readIORef time}}
        withHarnessConfigured configure $ \h -> do
          void $ localAccount h
          sso <- linkLocal h
          now <- getCurrentTime
          let wholeMicros = diffTimeToPicoseconds (utctDayTime now) `div` 1000000
              precise = now {utctDayTime = picosecondsToDiffTime (wholeMicros * 1000000 + 123456)}
          writeIORef time precise
          response <- protected h sso "/users/me/oidc/logout" Null
          expectStatus 200 response
          DTO.ProviderLogoutResponse url <- decodeResponse response
          active h sso 401
          getUrl h url True >>= expectStatus 303,
      testCase "unavailable provider logout reports failure after committing local revocation" $ withHarness $ \h -> do
        local <- localAccount h
        protected h local "/users/me/oidc/logout" Null >>= expectStatus 503
        active h local 401
        localLogin h >>= \session -> active h session 200,
      testCase "expired provider handoff cannot redirect or resurrect a revoked session" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        void $ localAccount h
        sso <- linkLocal h
        response <- protected h sso "/users/me/oidc/logout" Null
        expectStatus 200 response
        DTO.ProviderLogoutResponse url <- decodeResponse response
        void $ execute_ conn "UPDATE oidc_logout_states SET handoff_expires_at = now() - interval '1 second'"
        getUrl h url True >>= expectStatus 400
        active h sso 401,
      testCase "sid and supplied subject must both match; local and unrelated SSO sessions survive" $ withHarness $ \h@(Harness provider _ _ _) -> do
        local <- localAccount h
        alice <- linkLocal h
        setProviderBehavior provider defaultProviderBehavior {behaviorSubject = "synthetic-bob"}
        bobContinuation <- continuation h
        response <- request h "/auth/oidc/register" (DTO.OidcRegisterRequest bobContinuation "bob" "Bob" "bob@example.test")
        expectStatus 200 response
        bob <- decodeResponse response
        wrong <- signLogoutToken provider $ KM.fromList [("sid", String "sid-synthetic-alice"), ("sub", String "synthetic-bob")]
        backchannel h [("logout_token", TE.encodeUtf8 wrong)] >>= expectStatus 200
        active h alice 200
        right <- signLogoutToken provider $ KM.fromList [("sid", String "sid-synthetic-alice")]
        backchannel h [("logout_token", TE.encodeUtf8 right)] >>= expectStatus 200
        active h alice 401
        active h local 200
        active h bob 200,
      testCase "replayed valid jti does not revoke a later sign-in; disabled provider still accepts notifications" $ withHarness $ \h@(Harness provider _ _ conn) -> do
        local <- localAccount h
        first <- linkLocal h
        token <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice")]
        backchannel h [("logout_token", TE.encodeUtf8 token)] >>= expectStatus 200
        active h first 401
        second <- ssoLogin h
        backchannel h [("logout_token", TE.encodeUtf8 token)] >>= expectStatus 200
        active h second 200
        void $ execute_ conn "UPDATE auth_settings SET oidc_enabled = false"
        fresh <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice")]
        backchannel h [("logout_token", TE.encodeUtf8 fresh)] >>= expectStatus 200
        active h second 401
        active h local 200,
      testCase "duplicate form tokens and invalid signed claims never revoke sessions" $ withHarness $ \h@(Harness provider _ _ _) -> do
        void $ localAccount h
        sso <- linkLocal h
        valid <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice")]
        backchannel h [("logout_token", TE.encodeUtf8 valid), ("logout_token", TE.encodeUtf8 valid)] >>= expectStatus 400
        invalid <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice"), ("nonce", String "not-allowed")]
        denied <- backchannel h [("logout_token", TE.encodeUtf8 invalid)]
        expectStatus 400 denied
        assertBool "generic failure omits token" (not $ TE.encodeUtf8 invalid `BS.isInfixOf` LBS.toStrict (HTTP.responseBody denied))
        active h sso 200,
      testCase "backchannel notification fences an unknown identity while token exchange is in flight" $ withHarness $ \h@(Harness provider _ _ conn) -> do
        verifier <- randomToken
        begun <- request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) Nothing)
        expectStatus 200 begun
        DTO.OidcStartResponse _ url <- decodeResponse begun
        authorized <- getUrl h (url <> "&decision=allow") False
        callback <- location authorized
        -- Pause after the provider consumes the code, then commit logout before
        -- releasing its signed response to expose the in-flight proof race.
        barrier <- pauseTokenResponses provider
        result <- newEmptyMVar
        pending <- forkFinally (getUrl h callback True) (putMVar result)
        ( do
            timeout 5000000 (awaitTokenRequest barrier) >>= maybe (assertFailure "Provider exchange did not reach barrier") pure
            token <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice")]
            backchannel h [("logout_token", TE.encodeUtf8 token)] >>= expectStatus 200
            releaseTokenResponse barrier
            finished <- takeMVar result >>= either (const $ assertFailure "Callback request failed" >> fail "unreachable") pure
            expectStatus 303 finished
            destination <- location finished
            assertBool "callback cannot resolve a proof crossing logout" ("error=" `T.isInfixOf` destination)
            counts <- query_ conn "SELECT (SELECT count(*) FROM users), (SELECT count(*) FROM user_sessions)" :: IO [(Int, Int)]
            assertEqual "no session or account escaped the fence" [(0, 0)] counts
            providerTokenExchangeCount provider >>= assertEqual "no retry" 1
          )
          `finally` (releaseTokenResponse barrier >> killThread pending),
      testCase "notification after proof also fences registration before its first write" $ withHarness $ \h@(Harness provider _ _ conn) -> do
        token <- continuation h
        logout <- signLogoutToken provider $ KM.fromList [("sub", String "synthetic-alice")]
        backchannel h [("logout_token", TE.encodeUtf8 logout)] >>= expectStatus 200
        response <- request h "/auth/oidc/register" (DTO.OidcRegisterRequest token "alice" "Alice" "alice@example.test")
        assertBool "retained proof must be rejected" (statusCode (HTTP.responseStatus response) `elem` [400, 409])
        counts <- query_ conn "SELECT (SELECT count(*) FROM users), (SELECT count(*) FROM user_sessions)" :: IO [(Int, Int)]
        assertEqual "registration has no partial effects" [(0, 0)] counts
    ]

-- | Reuse the original bearer after logout; following redirects would hide status.
protected :: (ToJSON a) => Harness -> Auth.LoginResponse -> Text -> a -> IO (HTTP.Response LBS.ByteString)
protected (Harness _ manager port _) session path body = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
  HTTP.httpLbs
    initial
      { HTTP.method = "POST",
        HTTP.redirectCount = 0,
        HTTP.requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> TE.encodeUtf8 (Auth.accessToken session))],
        HTTP.requestBody = HTTP.RequestBodyLBS (encode body)
      }
    manager

active :: Harness -> Auth.LoginResponse -> Int -> Assertion
active (Harness _ manager port _) session status = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/users/me")
  response <- HTTP.httpLbs initial {HTTP.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 (Auth.accessToken session))]} manager
  expectStatus status response

localAccount :: Harness -> IO Auth.LoginResponse
localAccount h = do
  request h "/auth/register" (Auth.RegisterRequest "alice" "Alice" "alice@example.test" "password123") >>= expectStatus 200
  localLogin h

localLogin :: Harness -> IO Auth.LoginResponse
localLogin h = do
  response <- request h "/auth/login" (Auth.LoginRequest "alice" "password123")
  expectStatus 200 response
  decodeResponse response

linkLocal :: Harness -> IO Auth.LoginResponse
linkLocal h = do
  token <- continuation h
  response <- request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "password123")
  expectStatus 200 response
  decodeResponse response

ssoLogin :: Harness -> IO Auth.LoginResponse
ssoLogin h = do
  verifier <- randomToken
  begun <- request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) Nothing)
  expectStatus 200 begun
  DTO.OidcStartResponse flow url <- decodeResponse begun
  authorization <- getUrl h (url <> "&decision=allow") False
  callback <- location authorization
  finished <- getUrl h callback True
  expectStatus 303 finished
  frontend <- location finished
  let code = do
        uri <- parseURI (T.unpack frontend)
        let fragment = T.dropWhile (/= '?') $ T.pack $ uriFragment uri
        lookup "code" $ parseSimpleQuery $ TE.encodeUtf8 fragment
  handoff <- maybe (assertFailure "Sign-in did not return a handoff" >> fail "unreachable") (pure . TE.decodeUtf8) code
  response <- request h "/auth/oidc/exchange" (DTO.OidcExchangeRequest flow handoff verifier)
  expectStatus 200 response
  result <- decodeResponse response
  case result of
    DTO.OidcAuthenticated session -> pure session
    _ -> assertFailure "Existing identity did not sign in" >> fail "unreachable"

backchannel :: Harness -> [(ByteString, ByteString)] -> IO (HTTP.Response LBS.ByteString)
backchannel (Harness _ manager port _) form = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/auth/oidc/backchannel-logout")
  HTTP.httpLbs (HTTP.urlEncodedBody form initial {HTTP.method = "POST", HTTP.redirectCount = 0}) manager

location :: HTTP.Response a -> IO Text
location response = maybe (assertFailure "Expected redirect" >> fail "unreachable") (pure . TE.decodeUtf8) (lookup "Location" $ HTTP.responseHeaders response)
