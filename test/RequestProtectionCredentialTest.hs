{-# LANGUAGE OverloadedStrings #-}

-- | Credential-quota regressions check normalized aliases, account namespaces and
-- rejection without writes through the destructive PostgreSQL HTTP harness.
module RequestProtectionCredentialTest (requestProtectionCredentialTests) where

import APITest (identityRequest, withTestAppServer)
import Control.Monad (forM_, void)
import Data.Aeson (ToJSON, Value (Null), toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection, Only (..), execute_, query, query_)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types.Admin (AdminBootstrap (..))
import qualified Jizhang.API.Types.Admin as Admin
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (tokenDigest)
import Jizhang.RequestProtection
import qualified Network.HTTP.Client as HTTP
import OIDCFlowTest (Harness (..), continuation, decodeResponse, expectStatus, request, withHarnessConfigured)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

requestProtectionCredentialTests :: TestTree
requestProtectionCredentialTests =
  testGroup
    "Protection credential boundaries"
    [ testCase "incorrect and correct local proofs spend one UUID quota across normalized username and email aliases" $
        withCredentialApp (RatePolicy 2 2) Nothing $ \clock port manager conn -> do
          registerLocal port manager
          post port manager "/auth/login" (Auth.LoginRequest " @ALICE " "wrong-password") >>= expectStatus 401
          accepted <- post port manager "/auth/login" (Auth.LoginRequest " ALICE@EXAMPLE.TEST " "password123")
          expectStatus 200 accepted
          session <- decodeResponse accepted
          before <- accountState conn
          forM_ [Auth.LoginRequest "alice" "password123", Auth.LoginRequest "alice@example.test" "wrong-password"] $ \proof ->
            post port manager "/auth/login" proof >>= expectLimited 30
          accountState conn >>= assertEqual "denied login neither edits credentials nor creates or revokes sessions" before
          assertActive port manager session
          writeIORef clock (29 * second)
          post port manager "/auth/login" (Auth.LoginRequest "@alice" "password123") >>= expectLimited 1
          writeIORef clock (30 * second)
          restored <- post port manager "/auth/login" (Auth.LoginRequest "alice@example.test" "password123")
          expectStatus 200 restored
          decodeResponse restored >>= assertActive port manager
          assertActive port manager session
          afterUsers <- userState conn
          assertEqual "refill preserves enabled state, password and invalidation timestamp" (fst before) afterUsers
          counts <- query_ conn "SELECT count(*) FROM user_sessions" :: IO [Only Int]
          assertEqual "only the two admitted correct proofs issued sessions" [Only 2] counts,
      testCase "unknown normalized identifiers have bounded independent quotas and operator identifiers remain exact" $
        withCredentialApp (RatePolicy 1 1) Nothing $ \clock port manager conn -> do
          post port manager "/auth/login" (Auth.LoginRequest " @GHOST " "wrong-password") >>= expectStatus 401
          post port manager "/auth/login" (Auth.LoginRequest "ghost" "wrong-password") >>= expectLimited 60
          post port manager "/auth/login" (Auth.LoginRequest "other-ghost" "wrong-password") >>= expectStatus 401
          post port manager "/auth/login" (Auth.LoginRequest " MISSING@EXAMPLE.TEST " "wrong-password") >>= expectStatus 401
          post port manager "/auth/login" (Auth.LoginRequest "missing@example.test" "wrong-password") >>= expectLimited 60
          post port manager "/admin/auth/login" (Admin.AdminLoginRequest "ghost" "wrong-password") >>= expectStatus 401
          post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "ghost" "wrong-password") >>= expectLimited 60
          post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "Ghost" "wrong-password") >>= expectStatus 401
          post port manager "/admin/auth/login" (Admin.AdminLoginRequest "Ghost" "wrong-password") >>= expectLimited 60
          before <- accountState conn
          assertEqual "unknown proofs create no accounts or sessions" ([], []) before
          writeIORef clock (60 * second)
          post port manager "/auth/login" (Auth.LoginRequest "ghost" "wrong-password") >>= expectStatus 401
          post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "ghost" "wrong-password") >>= expectStatus 401
          accountState conn >>= assertEqual "refill does not turn an unknown proof into an account" before,
      testCase "bearer and cookie operator logins share a credential quota separate from a user with the identical UUID" $
        withCredentialApp (RatePolicy 2 2) (Just $ AdminBootstrap "operator" "operator-password123") $ \clock port manager conn -> do
          registerLocal port manager
          void $ execute_ conn "UPDATE users SET id = (SELECT id FROM admins WHERE username = 'operator') WHERE username = 'alice'"
          identifiers <- query_ conn "SELECT users.id = admins.id FROM users CROSS JOIN admins" :: IO [Only Bool]
          assertEqual "both account namespaces deliberately use the same UUID" [Only True] identifiers
          first <- loginLocal port manager "alice" "password123"
          post port manager "/admin/auth/login" (Admin.AdminLoginRequest "operator" "wrong-password") >>= expectStatus 401
          cookie <- post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "operator" "operator-password123")
          expectStatus 200 cookie
          assertBool "admitted operator cookie login emits cookies" (any ((== "Set-Cookie") . fst) $ HTTP.responseHeaders cookie)
          before <- accountState conn
          adminsBefore <- query_ conn "SELECT row_to_json(a)::text FROM admins AS a ORDER BY id" :: IO [Only Text]
          post port manager "/admin/auth/login" (Admin.AdminLoginRequest "operator" "operator-password123") >>= expectLimited 30
          post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "operator" "wrong-password") >>= expectLimited 30
          accountState conn >>= assertEqual "operator rejection leaves user state alone" before
          secondSession <- loginLocal port manager "alice@example.test" "password123"
          post port manager "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectLimited 30
          assertActive port manager first
          assertActive port manager secondSession
          writeIORef clock (30 * second)
          bearer <- post port manager "/admin/auth/login" (Admin.AdminLoginRequest "operator" "operator-password123")
          expectStatus 200 bearer
          post port manager "/admin/auth/session/login" (Admin.AdminLoginRequest "operator" "operator-password123") >>= expectLimited 30
          adminsAfter <- query_ conn "SELECT row_to_json(a)::text FROM admins AS a ORDER BY id" :: IO [Only Text]
          assertEqual "operator rejection and refill preserve its password and identity" adminsBefore adminsAfter,
      testCase "account password proofs share login quota and rejected changes preserve hashes, freshness and every session" $
        withCredentialApp (RatePolicy 3 3) Nothing $ \clock port manager conn -> do
          registerLocal port manager
          first <- loginLocal port manager "alice" "password123"
          other <- loginLocal port manager "alice@example.test" "password123"
          void $ execute_ conn "UPDATE user_sessions SET authenticated_at = created_at - interval '20 minutes'"
          protected port manager first "POST" "/users/me/reauthenticate" (DTO.ReauthenticateRequest "wrong-password") >>= expectStatus 400
          before <- accountState conn
          protected port manager first "POST" "/users/me/reauthenticate" (DTO.ReauthenticateRequest "password123") >>= expectLimited 20
          protected port manager first "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") "replacement123") >>= expectLimited 20
          post port manager "/auth/login" (Auth.LoginRequest "alice@example.test" "password123") >>= expectLimited 20
          accountState conn >>= assertEqual "429 changes no hash, invalidation, session or reauthentication timestamp" before
          assertActive port manager first
          assertActive port manager other
          writeIORef clock (20 * second)
          protected port manager first "POST" "/users/me/reauthenticate" (DTO.ReauthenticateRequest "password123") >>= expectStatus 204
          refreshed <- accountState conn
          assertEqual "admitted reauthentication preserves the account" (fst before) (fst refreshed)
          assertBool "admitted reauthentication refreshes a stored session" (snd before /= snd refreshed)
          protected port manager first "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") "replacement123") >>= expectLimited 20
          accountState conn >>= assertEqual "password change remains side-effect-free after another proof spent the refill" refreshed
          assertActive port manager first
          assertActive port manager other
          writeIORef clock (40 * second)
          protected port manager first "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") "replacement123") >>= expectStatus 204
          protected port manager first "GET" "/users/me" Null >>= expectStatus 401
          protected port manager other "GET" "/users/me" Null >>= expectStatus 401
          currentUsers <- userState conn
          assertBool "only an admitted password change replaces stored credentials" (fst before /= currentUsers)
          writeIORef clock (60 * second)
          post port manager "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectStatus 401
          post port manager "/auth/login" (Auth.LoginRequest "alice" "replacement123") >>= expectLimited 20
          writeIORef clock (80 * second)
          loginLocal port manager "alice" "replacement123" >>= assertActive port manager,
      testCase "OIDC link-existing shares local quota without spending continuation attempts on 429 and resumes the same proof after refill" $ do
        (protection, clock) <- credentialRuntime (RatePolicy 2 2)
        withHarnessConfigured (\env -> env {appRequestProtection = protection}) $ \h@(Harness _ manager port conn) -> do
          registerLocal port manager
          local <- loginLocal port manager "alice" "password123"
          token <- continuation h
          request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token " ALICE@EXAMPLE.TEST " "wrong-password") >>= expectStatus 400
          attempts <- query conn "SELECT password_attempts FROM oidc_transactions WHERE continuation_hash = ?" (Only $ Binary $ tokenDigest token) :: IO [Only Int]
          assertEqual "an admitted wrong password spends one continuation attempt" [Only 1] attempts
          flowBefore <- continuationState conn token
          assertEqual "one pending continuation is retained" 1 (length flowBefore)
          before <- accountState conn
          forM_ ["password123", "wrong-password", "password123"] $ \password ->
            request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "@alice" password) >>= expectLimited 30
          continuationState conn token >>= assertEqual "429 preserves attempts, phase, continuation hash and proof payload" flowBefore
          accountState conn >>= assertEqual "429 creates no session and changes no credentials" before
          linksBefore <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
          assertEqual "rejected linking writes no external identity" [Only 0] linksBefore
          request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest "invalid-continuation" "alice" "password123") >>= expectStatus 400
          assertActive port manager local
          writeIORef clock (30 * second)
          linked <- request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice@example.test" "password123")
          expectStatus 200 linked
          linkedSession <- decodeResponse linked
          assertEqual "linking preserves the local account UUID" (Auth.loginUser local) (Auth.loginUser linkedSession)
          assertActive port manager linkedSession
          assertActive port manager local
          userState conn >>= assertEqual "refilled linking does not replace local credentials or enabled state" (fst before)
          linksAfter <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
          assertEqual "the same continuation completes exactly one link" [Only 1] linksAfter
          post port manager "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectLimited 30
          request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "password123") >>= expectStatus 400
    ]

second :: Word64
second = 1000000000

-- | Isolate credential quotas with generous outer budgets and a manually advanced
-- nanosecond admission clock; authentication and SQL still use their own clocks.
credentialRuntime :: RatePolicy -> IO (RequestProtection, IORef Word64)
credentialRuntime policy = do
  clock <- newIORef 0
  let generous = RatePolicy 1000000 1000000
  protection <-
    newRequestProtection
      defaultRequestProtectionConfig
        { protectionIpRate = generous,
          protectionAuthIpRate = generous,
          protectionPrincipalRate = generous,
          protectionExpensiveRate = generous,
          protectionCredentialRate = policy
        }
      (readIORef clock)
  pure (protection, clock)

-- | Install the controlled limiter before startup; each invocation resets storage.
withCredentialApp :: RatePolicy -> Maybe AdminBootstrap -> (IORef Word64 -> Int -> HTTP.Manager -> Connection -> IO ()) -> IO ()
withCredentialApp policy bootstrap action = do
  (protection, clock) <- credentialRuntime policy
  withTestAppServer bootstrap (\env -> env {appRequestProtection = protection}) $ \_ port manager conn ->
    action clock port manager conn

post :: (ToJSON a) => Int -> HTTP.Manager -> String -> a -> IO (HTTP.Response LBS.ByteString)
post port manager path body = identityRequest port manager Nothing "POST" path (toJSON body)

protected :: (ToJSON a) => Int -> HTTP.Manager -> Auth.LoginResponse -> ByteString -> String -> a -> IO (HTTP.Response LBS.ByteString)
protected port manager session method path body =
  identityRequest port manager (Just $ Auth.accessToken session) method path (toJSON body)

registerLocal :: Int -> HTTP.Manager -> IO ()
registerLocal port manager =
  post port manager "/auth/register" (Auth.RegisterRequest "alice" "Alice" "alice@example.test" "password123") >>= expectStatus 200

loginLocal :: Int -> HTTP.Manager -> Text -> Text -> IO Auth.LoginResponse
loginLocal port manager identifier password = do
  response <- post port manager "/auth/login" (Auth.LoginRequest identifier password)
  expectStatus 200 response
  decodeResponse response

assertActive :: Int -> HTTP.Manager -> Auth.LoginResponse -> Assertion
assertActive port manager session = protected port manager session "GET" "/users/me" Null >>= expectStatus 200

-- | Check both the fixed rejection body and refill deadline, not merely HTTP 429.
expectLimited :: Int -> HTTP.Response LBS.ByteString -> Assertion
expectLimited retry response = do
  expectStatus 429 response
  assertEqual "fixed credential rejection" "Too many requests; try again later" (HTTP.responseBody response)
  assertEqual "positive refill deadline" (Just $ B8.pack $ show retry) (lookup "Retry-After" $ HTTP.responseHeaders response)
  assertEqual "credential rejection is not cached" (Just "no-store") (lookup "Cache-Control" $ HTTP.responseHeaders response)

userState :: Connection -> IO [Only Text]
userState conn = query_ conn "SELECT row_to_json(u)::text FROM users AS u ORDER BY id"

accountState :: Connection -> IO ([Only Text], [Only Text])
accountState conn = do
  users <- userState conn
  sessions <- query_ conn "SELECT row_to_json(s)::text FROM user_sessions AS s ORDER BY id"
  pure (users, sessions)

continuationState :: Connection -> Text -> IO [Only Text]
continuationState conn token =
  query
    conn
    "SELECT row_to_json(t)::text FROM oidc_transactions AS t WHERE continuation_hash = ?"
    (Only $ Binary $ tokenDigest token)
