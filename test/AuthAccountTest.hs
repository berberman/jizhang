{-# LANGUAGE OverloadedStrings #-}

-- | Account-method regressions cover proof freshness, last-method guards and atomic
-- revocation. The shared OIDC harness resets PostgreSQL for each case.
module AuthAccountTest (authAccountTests) where

import Control.Monad (forM_, void)
import Data.Aeson (ToJSON, Value (Null), encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import Database.PostgreSQL.Simple.Types (Binary (..))
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (pkceChallenge, randomToken, tokenDigest)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.URI (parseURI, uriFragment, uriQuery)
import OIDCFlowTest
import OIDCProvider (ProviderBehavior (..), defaultProviderBehavior, setProviderBehavior)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

authAccountTests :: TestTree
authAccountTests =
  testGroup
    "Account authentication and invitations"
    [ testCase "wrong current password is 400 and leaves every existing session usable" $ withHarness $ \h -> do
        first <- localAccount h
        second <- login h "password123"
        response <- protected h first "POST" "/users/me/reauthenticate" (DTO.ReauthenticateRequest "wrong-password")
        expectStatus 400 response
        assertEqual "actionable non-expiring password error" "Current password is incorrect" (HTTP.responseBody response)
        protected h first "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "wrong-password") "replacement123") >>= expectStatus 400
        assertActive h first
        assertActive h second
        void $ login h "password123",
      testCase "local password change revokes all sessions, never auto-signs-in, and replaces the hash" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        first <- localAccount h
        second <- login h "password123"
        protected h first "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") "replacement123") >>= expectStatus 204
        assertRevoked h first
        assertRevoked h second
        request h "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectStatus 401
        sessions <- query_ conn "SELECT count(*) FROM user_sessions WHERE revoked_at IS NULL" :: IO [Only Int]
        assertEqual "mutation issued no replacement session" [Only 0] sessions
        login h "replacement123" >>= assertActive h,
      testCase "local-only account cannot remove its last method" $ withHarness $ \h -> do
        session <- localAccount h
        response <- protected h session "DELETE" "/users/me/password" Null
        expectStatus 409 response
        assertEqual "last method guard" "Cannot remove the last sign-in method" (HTTP.responseBody response)
        assertActive h session,
      testCase "unlink needs the local password, preserves the account, and revokes both session methods" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        local <- localAccount h
        sso <- linkLocal h
        before <- query_ conn "SELECT id FROM users WHERE username = 'alice'" :: IO [Only UUID]
        protected h sso "POST" "/users/me/identity/unlink" (DTO.IdentityUnlinkRequest "wrong-password") >>= expectStatus 400
        assertActive h local
        assertActive h sso
        protected h sso "POST" "/users/me/identity/unlink" (DTO.IdentityUnlinkRequest "password123") >>= expectStatus 204
        assertRevoked h local
        assertRevoked h sso
        after <- query_ conn "SELECT id FROM users WHERE username = 'alice'" :: IO [Only UUID]
        assertEqual "unlink retains immutable account" before after
        identities <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "identity was removed" [Only 0] identities
        login h "password123" >>= assertActive h,
      testCase "nullable currentPassword is required and password UTF-8 byte boundary is enforced" $ withHarness $ \h -> do
        session <- localAccount h
        protected h session "PUT" "/users/me/password" (object ["newPassword" .= ("replacement123" :: Text)]) >>= expectStatus 400
        protected h session "PUT" "/users/me/password" (DTO.PasswordChangeRequest Nothing "replacement123") >>= expectStatus 400
        let over = T.replicate 24 "界" <> "a"
            boundary = T.replicate 24 "界"
        rejected <- protected h session "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") over)
        expectStatus 400 rejected
        assertEqual "byte limit, not character count" "Password must be at most 72 UTF-8 bytes" (HTTP.responseBody rejected)
        assertActive h session
        protected h session "PUT" "/users/me/password" (DTO.PasswordChangeRequest (Just "password123") boundary) >>= expectStatus 204
        login h boundary >>= assertActive h,
      testCase "password reauthentication changes only generic freshness and cannot authorize password removal" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        void $ localAccount h
        session <- linkLocal h
        void $ execute_ conn "UPDATE user_sessions SET authenticated_at = now() - interval '20 minutes' WHERE revoked_at IS NULL"
        protected h session "POST" "/users/me/reauthenticate" (DTO.ReauthenticateRequest "password123") >>= expectStatus 204
        response <- protected h session "GET" "/users/me/authentication" Null
        expectStatus 200 response
        DTO.AccountAuthentication hasPassword _ method _ explicit enabled _ <- decodeResponse response
        assertBool "password retained" hasPassword
        assertEqual "password proof cannot change session method" DTO.OidcMethod method
        assertEqual "password proof is not explicit provider proof" Nothing explicit
        assertBool "other sign-in method is viable" enabled
        protected h session "DELETE" "/users/me/password" Null >>= expectStatus 403
        assertActive h session,
      testCase "real matching prompt-login proof permits removal and revokes the starting session" $ withHarness $ \h -> do
        void $ localAccount h
        initial <- linkLocal h
        fresh <- explicitOidc h initial
        assertRevoked h initial
        protected h fresh "DELETE" "/users/me/password" Null >>= expectStatus 204
        assertRevoked h fresh
        request h "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectStatus 401,
      testCase "provider verification refreshes a stale provider session when zero max_age is ignored" $ withHarness $ \h@(Harness provider _ _ _) -> do
        void $ localAccount h
        initial <- linkLocal h
        setProviderBehavior provider defaultProviderBehavior {behaviorAuthenticationAgeSeconds = 13065}
        verified <- explicitOidc h initial
        assertRevoked h initial
        protected h verified "DELETE" "/users/me/password" Null >>= expectStatus 204
        assertRevoked h verified,
      testCase "silent SSO cannot set a first password; explicit provider proof can" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        token <- continuation h
        registered <- request h "/auth/oidc/register" (DTO.OidcRegisterRequest token "alice" "Alice" "alice@example.test")
        expectStatus 200 registered
        initial <- decodeResponse registered
        protected h initial "PUT" "/users/me/password" (DTO.PasswordChangeRequest Nothing "replacement123") >>= expectStatus 403
        fresh <- explicitOidc h initial
        protected h fresh "PUT" "/users/me/password" (DTO.PasswordChangeRequest Nothing "replacement123") >>= expectStatus 204
        assertRevoked h fresh
        identities <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "first password preserved identity" [Only 1] identities
        login h "replacement123" >>= assertActive h,
      testCase "reset inspect never consumes; redemption revokes sessions and sibling grants but preserves identity" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        first <- localAccount h
        sso <- linkLocal h
        token <- invitation conn
        sibling <- invitation conn
        forM_ [1, 2 :: Int] $ \_ -> request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus 200
        unused <- query_ conn "SELECT count(*) FROM account_invitations WHERE consumed_at IS NULL AND revoked_at IS NULL" :: IO [Only Int]
        assertEqual "inspection was read-only" [Only 2] unused
        assertActive h first
        assertActive h sso
        response <- request h "/auth/password-reset" (DTO.PasswordResetRequest token "reset-password123")
        expectStatus 204 response
        assertRevoked h first
        assertRevoked h sso
        request h "/auth/password-reset" (DTO.PasswordResetRequest token "another-password123") >>= expectStatus 400
        request h "/auth/password-reset" (DTO.PasswordResetRequest sibling "another-password123") >>= expectStatus 400
        request h "/auth/login" (Auth.LoginRequest "alice" "password123") >>= expectStatus 401
        identities <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "reset never unlinks identity" [Only 1] identities
        sessions <- query_ conn "SELECT count(*) FROM user_sessions WHERE revoked_at IS NULL" :: IO [Only Int]
        assertEqual "reset never auto-logins" [Only 0] sessions
        login h "reset-password123" >>= assertActive h,
      testCase "reset accepts disabled target without reenabling or resurrecting sessions" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        session <- localAccount h
        token <- invitation conn
        void $ execute_ conn "UPDATE users SET enabled = false WHERE username = 'alice'"
        request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus 200
        request h "/auth/password-reset" (DTO.PasswordResetRequest token "reset-password123") >>= expectStatus 204
        enabled <- query_ conn "SELECT enabled FROM users WHERE username = 'alice'" :: IO [Only Bool]
        assertEqual "disabled state preserved" [Only False] enabled
        request h "/auth/login" (Auth.LoginRequest "alice" "reset-password123") >>= expectStatus 401
        void $ execute_ conn "UPDATE users SET enabled = true WHERE username = 'alice'"
        assertRevoked h session
        login h "reset-password123" >>= assertActive h,
      testCase "invalid, expired, revoked and wrong-purpose reset grants fail without changing credentials" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        session <- localAccount h
        expired <- invitation conn
        void $ execute conn "UPDATE account_invitations SET created_at = now() - interval '31 minutes', expires_at = now() - interval '1 minute' WHERE token_hash = ?" (Only $ Binary $ tokenDigest expired)
        revoked <- invitation conn
        void $ execute conn "UPDATE account_invitations SET revoked_at = now() WHERE token_hash = ?" (Only $ Binary $ tokenDigest revoked)
        wrongPurpose <- invitation conn
        void $ execute conn "UPDATE account_invitations SET purpose = 'oidc_link', provider_revision = (SELECT revision FROM auth_settings) WHERE token_hash = ?" (Only $ Binary $ tokenDigest wrongPurpose)
        unknown <- randomToken
        forM_ ["malformed", unknown, expired, revoked, wrongPurpose] $ \token -> do
          response <- request h "/auth/password-reset" (DTO.PasswordResetRequest token "reset-password123")
          expectStatus 400 response
          assertEqual "fixed grant error" "This invitation is invalid or expired" (HTTP.responseBody response)
        token <- invitation conn
        over <- request h "/auth/password-reset" (DTO.PasswordResetRequest token (T.replicate 24 "界" <> "a"))
        expectStatus 400 over
        assertEqual "reset byte limit" "Password must be at most 72 UTF-8 bytes" (HTTP.responseBody over)
        request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus 200
        assertActive h session
        void $ login h "password123",
      testCase "authenticated linking requires confirmation, retains account, and replaces only its bound session" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        starting <- localAccount h
        unrelated <- login h "password123"
        token <- boundLink h starting
        unlinked <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "provider proof alone never links" [Only 0] unlinked
        assertActive h starting
        confirmed <- request h "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest token)
        expectStatus 200 confirmed
        replacement <- decodeResponse confirmed
        assertEqual "explicit link retains target UUID" (Auth.loginUser starting) (Auth.loginUser replacement)
        assertRevoked h starting
        assertActive h replacement
        assertActive h unrelated
        request h "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest token) >>= expectStatus 400
        login h "password123" >>= assertActive h,
      testCase "revoked bound session cannot confirm an otherwise valid proven identity" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        starting <- localAccount h
        unrelated <- login h "password123"
        token <- boundLink h starting
        void $ execute conn "UPDATE user_sessions SET revoked_at = now() WHERE id = (SELECT session__id FROM oidc_transactions WHERE continuation_hash = ?)" (Only $ Binary $ tokenDigest token)
        request h "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest token) >>= expectStatus 409
        effects <- query_ conn "SELECT (SELECT count(*) FROM external_identities), (SELECT count(*) FROM user_sessions WHERE method = 'oidc')" :: IO [(Int, Int)]
        assertEqual "rejected confirmation commits neither identity nor session" [(0, 0)] effects
        assertRevoked h starting
        assertActive h unrelated,
      testCase "SSO invitation survives provider denial and requires one explicit target confirmation" $ withHarness $ \h@(Harness _ _ _ conn) -> do
        original <- localAccount h
        token <- invitationFor conn "oidc_link"
        deniedVerifier <- randomToken
        deniedStart <- request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge deniedVerifier) (Just token))
        expectStatus 200 deniedStart
        DTO.OidcStartResponse _ deniedUrl <- decodeResponse deniedStart
        deniedProvider <- getUrl h (deniedUrl <> "&decision=deny") False
        expectStatus 303 deniedProvider
        deniedCallback <- redirectLocation deniedProvider
        deniedBackend <- getUrl h deniedCallback True
        expectStatus 303 deniedBackend
        deniedFrontend <- redirectLocation deniedBackend
        assertBool "fixed cancellation category" ("error=access_denied" `T.isInfixOf` deniedFrontend)
        request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus 200
        verifier <- randomToken
        started <- request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) (Just token))
        expectStatus 200 started
        begun <- decodeResponse started
        proof <- completeFlow h verifier begun
        continuationToken <- case proof of
          DTO.OidcLinkConfirmationRequired pending target _ -> do
            assertEqual "invitation pins the intended target" (Auth.loginUser original) target
            pure pending
          _ -> assertFailure "Invitation did not require explicit target confirmation" >> fail "unreachable"
        before <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "invitation proof alone does not link" [Only 0] before
        request h "/auth/invitations/inspect" (DTO.InvitationInspectRequest token) >>= expectStatus 200
        confirmed <- request h "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest continuationToken)
        expectStatus 200 confirmed
        linked <- decodeResponse confirmed
        assertEqual "invitation confirmation preserves account UUID" (Auth.loginUser original) (Auth.loginUser linked)
        consumed <- query_ conn "SELECT consumed_at IS NOT NULL FROM account_invitations WHERE purpose = 'oidc_link'" :: IO [Only Bool]
        assertEqual "only successful confirmation consumes invitation" [Only True] consumed
        request h "/auth/oidc/confirm-link" (DTO.OidcContinuationRequest continuationToken) >>= expectStatus 400
        request h "/auth/oidc/start" (DTO.OidcStartRequest (pkceChallenge verifier) (Just token)) >>= expectStatus 400
        login h "password123" >>= assertActive h
    ]

-- | Keep the captured bearer fixed, including after mutations revoke its session.
protected :: (ToJSON a) => Harness -> Auth.LoginResponse -> ByteString -> Text -> a -> IO (HTTP.Response LBS.ByteString)
protected (Harness _ manager port _) session method path body = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
  HTTP.httpLbs
    initial
      { HTTP.method = method,
        HTTP.redirectCount = 0,
        HTTP.requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> TE.encodeUtf8 (Auth.accessToken session))],
        HTTP.requestBody = HTTP.RequestBodyLBS (encode body)
      }
    manager

-- | Reserve the synthetic alice identity before linking or invitation setup.
localAccount :: Harness -> IO Auth.LoginResponse
localAccount h = do
  request h "/auth/register" (Auth.RegisterRequest "alice" "Alice" "alice@example.test" "password123") >>= expectStatus 200
  login h "password123"

login :: Harness -> Text -> IO Auth.LoginResponse
login h password = do
  response <- request h "/auth/login" (Auth.LoginRequest "alice" password)
  expectStatus 200 response
  decodeResponse response

assertActive, assertRevoked :: Harness -> Auth.LoginResponse -> Assertion
assertActive h session = protected h session "GET" "/users/me" Null >>= expectStatus 200
assertRevoked h session = protected h session "GET" "/users/me" Null >>= expectStatus 401

-- | Link the default provider identity using alice's local password proof.
linkLocal :: Harness -> IO Auth.LoginResponse
linkLocal h = do
  token <- continuation h
  response <- request h "/auth/oidc/link-existing" (DTO.OidcLinkExistingRequest token "alice" "password123")
  expectStatus 200 response
  decodeResponse response

invitation :: Connection -> IO Text
invitation conn = invitationFor conn "password_reset"

-- | Seed a thirty-minute grant for alice, storing only its digest and pinning OIDC
-- grants to the current provider revision; the synthetic admin is never logged in.
invitationFor :: Connection -> Text -> IO Text
invitationFor conn purpose = do
  token <- randomToken
  rowId <- UUID.nextRandom
  adminId <- UUID.nextRandom
  void $ execute conn "INSERT INTO admins (id, username, password_hash) VALUES (?, ?, 'synthetic-unused-admin-hash')" (adminId, "invitation-" <> T.pack (show adminId))
  [Only userId] <- query_ conn "SELECT id FROM users WHERE username = 'alice'" :: IO [Only UUID]
  void $
    execute
      conn
      "INSERT INTO account_invitations (id, token_hash, purpose, user__id, admin__id, provider_revision, created_at, expires_at) VALUES (?, ?, ?, ?, ?, CASE WHEN ? = 'oidc_link' THEN (SELECT revision FROM auth_settings) ELSE NULL END, now(), now() + interval '30 minutes')"
      (rowId, Binary $ tokenDigest token, purpose, userId, adminId, purpose)
  pure token

-- | Require a matching explicit provider reauthentication to return a new session.
explicitOidc :: Harness -> Auth.LoginResponse -> IO Auth.LoginResponse
explicitOidc h session = do
  verifier <- randomToken
  begun <- protected h session "POST" "/users/me/oidc/start" (DTO.AccountOidcStartRequest DTO.ReauthenticateIdentity (pkceChallenge verifier))
  expectStatus 200 begun
  started <- decodeResponse begun
  result <- completeFlow h verifier started
  case result of
    DTO.OidcAuthenticated authenticated -> pure authenticated
    _ -> assertFailure "Matching explicit provider proof did not authenticate" >> fail "unreachable"

-- | Retain the confirmation token without committing the authenticated identity link.
boundLink :: Harness -> Auth.LoginResponse -> IO Text
boundLink h session = do
  verifier <- randomToken
  response <- protected h session "POST" "/users/me/oidc/start" (DTO.AccountOidcStartRequest DTO.LinkIdentity (pkceChallenge verifier))
  expectStatus 200 response
  started <- decodeResponse response
  result <- completeFlow h verifier started
  case result of
    DTO.OidcLinkConfirmationRequired token target _ -> do
      assertEqual "authenticated link pins current account" (Auth.loginUser session) target
      pure token
    _ -> assertFailure "Authenticated link skipped explicit confirmation" >> fail "unreachable"

-- | Drive redirects manually and assert prompt=login before redeeming the browser
-- handoff with the original verifier; no frontend server is involved.
completeFlow :: Harness -> Text -> DTO.OidcStartResponse -> IO DTO.OidcExchangeResponse
completeFlow h verifier (DTO.OidcStartResponse flow url) = do
  authorization <- maybe (assertFailure "Invalid authorization URL" >> fail "unreachable") pure (parseURI $ T.unpack url)
  let authorizationQuery = parseSimpleQuery $ TE.encodeUtf8 $ T.pack $ uriQuery authorization
  assertEqual "explicit provider authentication requested" (Just "login") (lookup "prompt" authorizationQuery)
  provider <- getUrl h (url <> "&decision=allow") False
  expectStatus 303 provider
  callback <- redirectLocation provider
  backend <- getUrl h callback True
  expectStatus 303 backend
  frontend <- redirectLocation backend
  let code = do
        uri <- parseURI (T.unpack frontend)
        let query = T.dropWhile (/= '?') (T.pack $ uriFragment uri)
        TE.decodeUtf8 <$> lookup "code" (parseSimpleQuery $ TE.encodeUtf8 query)
  handoff <- maybe (assertFailure "Explicit provider proof omitted handoff" >> fail "unreachable") pure code
  exchanged <- request h "/auth/oidc/exchange" (DTO.OidcExchangeRequest flow handoff verifier)
  expectStatus 200 exchanged
  decodeResponse exchanged

redirectLocation :: HTTP.Response a -> IO Text
redirectLocation response = maybe (assertFailure "Missing redirect" >> fail "unreachable") (pure . TE.decodeUtf8) (lookup "Location" $ HTTP.responseHeaders response)
