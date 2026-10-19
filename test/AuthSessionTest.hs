{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Session regressions separate signed bearer validity from persisted ownership,
-- expiry and revocation. Cases use a freshly reset PostgreSQL-backed application.
module AuthSessionTest (authSessionTests) where

import APITest
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Aeson (ToJSON, Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import GHC.Generics (Generic)
import Jizhang.API.App (AppEnv (..))
import Jizhang.OIDC (AuthRuntime (..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (status401, statusCode)
import Servant.Auth.Server (ToJWT, defaultJWTSettings, generateKey, makeJWT)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Deliberately omit session claims to exercise rejection of old UUID-only JWTs.
data LegacyClaims = LegacyClaims {authUserId :: UUID}
  deriving (Generic, ToJSON, ToJWT)

authSessionTests :: TestTree
authSessionTests =
  testGroup
    "Revocable application sessions"
    [ testCase "revoking one account's session leaves another session usable" $ withTestAppServer Nothing id $ \env _ _ conn -> do
        first <- setupUser env "alice"
        second <- setupUser env "bob"
        void $ runClient env (cGetMe $ mkTC first)
        void $ execute_ conn "UPDATE user_sessions SET revoked_at = now() WHERE user__id = (SELECT id FROM users WHERE username = 'alice')"
        runClientExpectStatus env status401 (cGetMe $ mkTC first)
        void $ runClient env (cGetMe $ mkTC second),
      testCase "correctly signed legacy UUID-only tokens cannot authorize application requests" $ do
        signingKey <- generateKey
        let settings = defaultJWTSettings signingKey
        withTestAppServer Nothing (\environment -> environment {appJWTSettings = settings}) $ \env _ _ conn -> do
          void $ setupUser env "alice"
          [Only identifier] <- query_ conn "SELECT id FROM users WHERE username = 'alice'"
          signed <- makeJWT (LegacyClaims identifier) settings Nothing
          case signed of
            Left _ -> assertFailure "Could not prepare legacy signed token"
            Right token -> runClientExpectStatus env status401 (cGetMe $ mkTC $ decodeUtf8 $ LBS.toStrict token),
      testCase "disabled users cannot use existing sessions or obtain a new password session" $ withTestAppServer Nothing id $ \env port manager conn -> do
        token <- setupUser env "alice"
        void $ execute_ conn "UPDATE users SET enabled = false WHERE username = 'alice'"
        runClientExpectStatus env status401 (cGetMe $ mkTC token)
        response <- postJson manager port "/auth/login" $ object ["loginIdentifier" .= ("alice" :: Text), "loginPassword" .= ("password123" :: Text)]
        assertEqual "disabled login" 401 (statusCode $ HTTP.responseStatus response),
      testCase "expired session fails even while its signed bearer token remains unexpired" $ withTestAppServer Nothing id $ \env _ _ conn -> do
        token <- setupUser env "alice"
        void $ execute_ conn "UPDATE user_sessions SET created_at = now() - interval '2 days', authenticated_at = now() - interval '2 days', expires_at = now() - interval '1 day'"
        runClientExpectStatus env status401 (cGetMe $ mkTC token),
      testCase "session cannot authorize a different user after its stored owner changes" $ withTestAppServer Nothing id $ \env _ _ conn -> do
        aliceToken <- setupUser env "alice"
        void $ setupUser env "bob"
        void $ execute_ conn "UPDATE user_sessions SET user__id = (SELECT id FROM users WHERE username = 'bob') WHERE user__id = (SELECT id FROM users WHERE username = 'alice')"
        runClientExpectStatus env status401 (cGetMe $ mkTC aliceToken),
      testCase "registration switch prevents signup without disabling existing local login" $ withTestAppServer Nothing id $ \env port manager conn -> do
        void $ setupUser env "alice"
        void $ execute_ conn "UPDATE auth_settings SET local_registration_enabled = false, revision = revision + 1"
        refused <-
          postJson manager port "/auth/register" $
            object
              ["registerUsername" .= ("bob" :: Text), "registerDisplayName" .= ("Bob" :: Text), "registerEmail" .= ("bob@example.test" :: Text), "registerPassword" .= ("password123" :: Text)]
        assertEqual "signup disabled" 403 (statusCode $ HTTP.responseStatus refused)
        assertEqual "actionable error" "Local registration is disabled" (HTTP.responseBody refused)
        response <- postJson manager port "/auth/login" $ object ["loginIdentifier" .= ("alice" :: Text), "loginPassword" .= ("password123" :: Text)]
        assertEqual "local login remains available" 200 (statusCode $ HTTP.responseStatus response),
      testCase "credential invalidation after password verification prevents session issuance" $ do
        armed <- newIORef False
        verified <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        -- The clock call is a post-password-verification barrier, not simulated time.
        -- It lets invalidation commit before the pending login can issue a session.
        let clock = do
              pause <- readIORef armed
              when pause $ putMVar verified () >> takeMVar release
              getCurrentTime
            configure environment = environment {appAuthRuntime = (appAuthRuntime environment) {authClock = clock}}
        withTestAppServer Nothing configure $ \env port manager conn -> do
          void $ setupUser env "alice"
          before <- query_ conn "SELECT count(*) FROM user_sessions" :: IO [Only Int]
          writeIORef armed True
          let oldLogin = postJson manager port "/auth/login" $ object ["loginIdentifier" .= ("alice" :: Text), "loginPassword" .= ("password123" :: Text)]
          bracket (forkFinally oldLogin $ putMVar finished) killThread $ \_ -> do
            ready <- timeout 10000000 (takeMVar verified)
            assertEqual "login reached post-verification clock barrier" (Just ()) ready
            void $ execute_ conn "UPDATE users SET password_hash = NULL, auth_invalidated_at = now() WHERE username = 'alice'"
            writeIORef armed False
            putMVar release ()
            outcome <- timeout 10000000 (takeMVar finished)
            case outcome of
              Just (Right response) -> assertEqual "stale proof rejected" 409 (statusCode $ HTTP.responseStatus response)
              _ -> assertFailure "Verified login did not finish safely"
            after <- query_ conn "SELECT count(*) FROM user_sessions" :: IO [Only Int]
            assertEqual "no session committed from stale proof" before after
    ]

-- | Keep redirects disabled so login status reflects the application response.
postJson :: HTTP.Manager -> Int -> String -> Value -> IO (HTTP.Response LBS.ByteString)
postJson manager port path value = do
  request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
  HTTP.httpLbs
    request
      { HTTP.method = "POST",
        HTTP.requestHeaders = [("Content-Type", "application/json")],
        HTTP.requestBody = HTTP.RequestBodyLBS (encode value),
        HTTP.redirectCount = 0
      }
    manager
