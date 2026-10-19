{-# LANGUAGE OverloadedStrings #-}

-- | Operator-side regressions for settings revisions, provisioning and invitation
-- revocation. Each case uses the destructive PostgreSQL-backed HTTP harness.
module AdminAuthenticationTest (adminAuthenticationTests) where

import APITest (setupAdmin, withTestAppServer)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), getCurrentTime, picosecondsToDiffTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types (AdminBootstrap (..))
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security (encryptSecret)
import qualified Jizhang.OIDC as O
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.URI (parseSimpleQuery)
import OIDCFlowTest (Harness (..), continuation, decodeResponse, expectStatus, request)
import OIDCProvider
import Test.Tasty
import Test.Tasty.HUnit

-- | Enable a signed loopback provider and bootstrap a bearer-authenticated operator.
-- The injected clock exercises comparisons against PostgreSQL-truncated timestamps.
withOperator :: (Harness -> Text -> IO ()) -> IO ()
withOperator action = withOidcProvider defaultProviderConfig $ \provider -> do
  runtime <-
    O.newAuthRuntime
      (Just "http://127.0.0.1:8080")
      (Just "http://127.0.0.1:3000/")
      (Just $ TE.decodeUtf8 $ B64.encode $ BS.pack [0 .. 31])
      ["http://127.0.0.1:3000"]
  withTestAppServer
    (Just $ AdminBootstrap "operator" "operator-password")
    (\env -> env {appAuthRuntime = runtime {O.authClock = preciseClock}})
    $ \env port manager conn -> do
      key <- maybe (fail "Missing synthetic key") pure (O.authSecretKey runtime)
      encrypted <- encryptSecret key "auth_settings:1:client_secret" providerClientSecret
      void $
        execute
          conn
          "UPDATE auth_settings SET oidc_enabled=true, issuer=?, client_id=?, client_secret_encrypted=?, sso_registration_enabled=true, revision=revision+1"
          (providerIssuer provider, providerClientId, encrypted)
      token <- setupAdmin env "operator" "operator-password"
      action (Harness provider manager port conn) token

-- PostgreSQL truncates timestamps to microseconds; exercise finer runtime clocks.
preciseClock :: IO UTCTime
preciseClock = do
  now <- getCurrentTime
  pure now {utctDayTime = fromInteger (floor $ utctDayTime now) + picosecondsToDiffTime 123456789}

-- | Send JSON without following redirects, so authorization failures stay observable.
operatorRequest :: Harness -> Maybe Text -> BS.ByteString -> Text -> Value -> IO (HTTP.Response LBS.ByteString)
operatorRequest (Harness _ manager port _) token method path body = do
  initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
  HTTP.httpLbs
    initial
      { HTTP.method = method,
        HTTP.redirectCount = 0,
        HTTP.requestHeaders = [("Content-Type", "application/json")] <> maybe [] (\value -> [("Authorization", "Bearer " <> TE.encodeUtf8 value)]) token,
        HTTP.requestBody = HTTP.RequestBodyLBS (encode body)
      }
    manager

admin :: Harness -> Text -> BS.ByteString -> Text -> Value -> IO (HTTP.Response LBS.ByteString)
admin h token = operatorRequest h (Just token)

readSettings :: Harness -> Text -> IO DTO.AuthenticationSettings
readSettings h token = admin h token "GET" "/admin/authentication" Null >>= decoded 200

decoded :: (FromJSON a) => Int -> HTTP.Response LBS.ByteString -> IO a
decoded status response = expectStatus status response >> decodeResponse response

revisionOf :: DTO.AuthenticationSettings -> Int64
revisionOf (DTO.AuthenticationSettings revision _ _ _ _ _ _ _ _ _ _ _ _ _ _) = revision

-- | Build an unsaved settings draft with both public registration switches disabled.
draft :: Int64 -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Value
draft revision enabled issuer client secret = toJSON $ DTO.AuthenticationSettingsRequest revision False False enabled "Synthetic provider" issuer client secret

-- | Provision through the operator API, then resolve the persisted UUID by username.
createUser :: Harness -> Text -> Text -> Text -> Maybe Text -> IO UUID
createUser h token username mode password = do
  response <-
    admin h token "POST" "/admin/users" $
      object
        [ "createUsername" .= username,
          "createDisplayName" .= username,
          "createEmail" .= (username <> "@example.test"),
          "createMode" .= mode,
          "createPassword" .= password
        ]
  expectStatus 200 response
  let Harness _ _ _ conn = h
  [Only uid] <- query conn "SELECT id FROM users WHERE username=?" (Only username)
  pure uid

userPath :: UUID -> Text
userPath uid = "/admin/users/" <> T.pack (show uid)

-- | Decode a session only on expected success; failed proofs must return no token.
localLogin :: Harness -> Text -> Text -> Int -> IO (Maybe Text)
localLogin h username password status = do
  response <- request h "/auth/login" $ object ["loginIdentifier" .= username, "loginPassword" .= password]
  expectStatus status response
  if status /= 200
    then pure Nothing
    else do
      value <- decodeResponse response
      case value of
        Object fields | Just (String token) <- KM.lookup "accessToken" fields -> pure (Just token)
        _ -> assertFailure "Missing session" >> fail "unreachable"

sessionStatus :: Harness -> Text -> Int -> Assertion
sessionStatus h token status = operatorRequest h (Just token) "GET" "/users/me" Null >>= expectStatus status

-- | Issue a grant through the operator route rather than seeding invitation rows.
issue :: Harness -> Text -> UUID -> Text -> IO DTO.InvitationIssued
issue h token uid purpose = admin h token "POST" (userPath uid <> "/" <> purpose) Null >>= decoded 200

invitationToken :: DTO.InvitationIssued -> Text
invitationToken (DTO.InvitationIssued _ url _) =
  maybe "" TE.decodeUtf8 $
    lookup "token" $
      parseSimpleQuery (TE.encodeUtf8 $ T.dropWhile (/= '?') url)

invitationId :: DTO.InvitationIssued -> UUID
invitationId (DTO.InvitationIssued uid _ _) = uid

adminAuthenticationTests :: TestTree
adminAuthenticationTests =
  testGroup
    "Operator authentication management"
    [ testCase "settings preserve write-only secrets, test unsaved drafts and reject stale saves without writes" $ withOperator $ \h@(Harness provider _ _ conn) token -> do
        before <- readSettings h token
        [Only ciphertext] <- query_ conn "SELECT client_secret_encrypted FROM auth_settings" :: IO [Only Text]
        let value = draft (revisionOf before) True (Just $ providerIssuer provider) (Just providerClientId) Nothing
        tested <- admin h token "POST" "/admin/authentication/test" value
        expectStatus 200 tested
        afterTest <- readSettings h token
        assertBool "Test never saves" (before == afterTest)
        providerTokenExchangeCount provider >>= assertEqual "Test never claims a successful client-secret exchange" 0
        saved <- admin h token "PUT" "/admin/authentication" value >>= decoded 200
        assertEqual "one revision" (revisionOf before + 1) (revisionOf saved)
        [Only preserved] <- query_ conn "SELECT client_secret_encrypted FROM auth_settings" :: IO [Only Text]
        assertBool "null secret preserves encrypted value" (ciphertext == preserved)
        stale <- admin h token "PUT" "/admin/authentication" value
        expectStatus 409 stale
        assertEqual "specific stale revision error" "Authentication settings changed; reload before saving" (HTTP.responseBody stale)
        readSettings h token >>= assertBool "stale save writes nothing" . (saved ==)
        response <- admin h token "GET" "/admin/authentication" Null
        assertBool "never returns secret/ciphertext" (not ("clientSecret\"" `BS.isInfixOf` LBS.toStrict (HTTP.responseBody response))),
      testCase "disabled partial drafts require no discovery and failed enablement rolls back" $ withOperator $ \h token -> do
        settings <- readSettings h token
        let partial = draft (revisionOf settings) False (Just "http://127.0.0.1:1/") Nothing Nothing
        saved <- admin h token "PUT" "/admin/authentication" partial >>= decoded 200
        failed <- admin h token "PUT" "/admin/authentication" (draft (revisionOf saved) True (Just "http://127.0.0.1:1/") (Just providerClientId) Nothing)
        expectStatus 503 failed
        readSettings h token >>= assertBool "failed discovery does not save" . (saved ==)
        forM_ [Just "", Just "   "] $ \secret ->
          admin
            h
            token
            "PUT"
            "/admin/authentication"
            (draft (revisionOf saved) False Nothing Nothing secret)
            >>= expectStatus 400,
      testCase "operator provisioning bypasses signup switches and enforces explicit local versus pending SSO modes" $ withOperator $ \h@(Harness provider _ _ conn) token -> do
        settings <- readSettings h token
        admin h token "PUT" "/admin/authentication" (draft (revisionOf settings) True (Just $ providerIssuer provider) (Just providerClientId) Nothing) >>= expectStatus 200
        void $ createUser h token "alice" "local" (Just "password123")
        void $ createUser h token "bob" "sso" Nothing
        localLogin h "alice" "password123" 200
          >>= maybe
            (fail "Missing local session")
            ( \session -> do
                operatorRequest h (Just session) "GET" "/admin/authentication" Null >>= expectStatus 401
                operatorRequest h (Just session) "POST" "/admin/users/00000000-0000-0000-0000-000000000000/password-reset" Null >>= expectStatus 401
            )
        rows <- query_ conn "SELECT username, enabled, password_hash IS NOT NULL FROM users ORDER BY username" :: IO [(Text, Bool, Bool)]
        assertEqual "pending SSO account has no invented credentials" [("alice", True, True), ("bob", True, False)] rows
        forM_ [("local", Null), ("sso", String "password123"), ("unknown", Null)] $ \(mode, password) ->
          admin h token "POST" "/admin/users" (object ["createUsername" .= ("carol" :: Text), "createDisplayName" .= ("Carol" :: Text), "createEmail" .= ("carol@example.test" :: Text), "createMode" .= (mode :: Text), "createPassword" .= password]) >>= expectStatus 400
        admin h token "POST" "/admin/users" (object ["createUsername" .= ("carol" :: Text), "createDisplayName" .= ("Carol" :: Text), "createEmail" .= ("carol@example.test" :: Text), "createPassword" .= ("password123" :: Text)]) >>= expectStatus 400,
      testCase "profile disable revokes sessions and grants; reenable restores neither and changed email loses verification" $ withOperator $ \h@(Harness _ _ _ conn) token -> do
        uid <- createUser h token "alice" "local" (Just "password123")
        Just session <- localLogin h "alice" "password123" 200
        grant <- issue h token uid "password-reset"
        void $ execute conn "UPDATE users SET email_verified=true WHERE id=?" (Only uid)
        let update enabled = toJSON $ DTO.AdminUpdateUserRequest " @ALICE " "Alice" " Changed@Example.Test " enabled
        admin h token "PUT" (userPath uid) (update False) >>= expectStatus 200
        sessionStatus h session 401
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken grant) >>= expectStatus 400
        localLogin h "alice" "password123" 401 >> pure ()
        admin h token "PUT" (userPath uid) (update True) >>= expectStatus 200
        sessionStatus h session 401
        localLogin h "alice" "password123" 200 >> pure ()
        rows <- query conn "SELECT email, email_verified FROM users WHERE id=?" (Only uid) :: IO [(Text, Bool)]
        assertEqual "normalized changed email is not verified" [("changed@example.test", False)] rows,
      testCase "reset issuance is inert, replacement is one-use and disabled reset never enables or signs in" $ withOperator $ \h@(Harness _ _ _ conn) token -> do
        uid <- createUser h token "alice" "local" (Just "password123")
        Just session <- localLogin h "alice" "password123" 200
        first <- issue h token uid "password-reset"
        sessionStatus h session 200
        second <- issue h token uid "password-reset"
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken first) >>= expectStatus 400
        sessionStatus h session 200
        request h "/auth/password-reset" (toJSON $ DTO.PasswordResetRequest (invitationToken second) "replacement-password") >>= expectStatus 204
        sessionStatus h session 401
        request h "/auth/password-reset" (toJSON $ DTO.PasswordResetRequest (invitationToken second) "another-password") >>= expectStatus 400
        localLogin h "alice" "password123" 401 >> pure ()
        localLogin h "alice" "replacement-password" 200 >> pure ()
        admin h token "PUT" (userPath uid) (toJSON $ DTO.AdminUpdateUserRequest "alice" "Alice" "alice@example.test" False) >>= expectStatus 200
        disabled <- issue h token uid "password-reset"
        request h "/auth/password-reset" (toJSON $ DTO.PasswordResetRequest (invitationToken disabled) "disabled-password") >>= expectStatus 204
        localLogin h "alice" "disabled-password" 401 >> pure ()
        [Only enabled] <- query conn "SELECT enabled FROM users WHERE id=?" (Only uid)
        assertEqual "reset leaves disabled" False enabled,
      testCase "invitation revocation checks target ownership and provider revisions invalidate only SSO invitations" $ withOperator $ \h@(Harness provider _ _ _) token -> do
        alice <- createUser h token "alice" "local" (Just "password123")
        bob <- createUser h token "bob" "sso" Nothing
        reset <- issue h token alice "password-reset"
        link <- issue h token bob "identity-invitation"
        admin h token "DELETE" (userPath alice <> "/invitations/" <> T.pack (show $ invitationId link)) Null >>= expectStatus 404
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken link) >>= expectStatus 200
        settings <- readSettings h token
        admin h token "PUT" "/admin/authentication" (draft (revisionOf settings) True (Just $ providerIssuer provider) (Just providerClientId) Nothing) >>= expectStatus 200
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken link) >>= expectStatus 400
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken reset) >>= expectStatus 200
        admin h token "DELETE" (userPath alice <> "/invitations/" <> T.pack (show $ invitationId reset)) Null >>= expectStatus 204
        request h "/auth/invitations/inspect" (toJSON $ DTO.InvitationInspectRequest $ invitationToken reset) >>= expectStatus 400,
      testCase "last-method guard is exact; disabled removal and revoke-all preserve later allowed login" $ withOperator $ \h token -> do
        uid <- createUser h token "alice" "local" (Just "password123")
        refusal <- admin h token "DELETE" (userPath uid <> "/password") Null
        expectStatus 409 refusal
        assertEqual "last method" "Cannot remove the last sign-in method" (HTTP.responseBody refusal)
        Just session <- localLogin h "alice" "password123" 200
        admin h token "POST" (userPath uid <> "/revoke-sessions") Null >>= expectStatus 204
        sessionStatus h session 401
        localLogin h "alice" "password123" 200 >> pure ()
        admin h token "PUT" (userPath uid) (toJSON $ DTO.AdminUpdateUserRequest "alice" "Alice" "alice@example.test" False) >>= expectStatus 200
        admin h token "DELETE" (userPath uid <> "/password") Null >>= expectStatus 204
        admin h token "PUT" (userPath uid) (toJSON $ DTO.AdminUpdateUserRequest "alice" "Alice" "alice@example.test" True) >>= expectStatus 200
        localLogin h "alice" "password123" 401 >> pure (),
      testCase "linked identity prevents retarget and clear; disabling revokes only OIDC sessions and preserves local access" $ withOperator $ \h@(Harness provider _ _ _) token -> do
        uid <- createUser h token "local" "local" (Just "password123")
        Just local <- localLogin h "local" "password123" 200
        proof <- continuation h
        registered <- request h "/auth/oidc/register" (toJSON $ DTO.OidcRegisterRequest proof "alice" "Alice" "alice@example.test") >>= decoded 200
        sso <- case registered of
          Object fields | Just (String value) <- KM.lookup "accessToken" fields -> pure value
          _ -> fail "Missing SSO session"
        operatorRequest h (Just sso) "GET" "/admin/authentication" Null >>= expectStatus 401
        settings <- readSettings h token
        forM_
          [ admin h token "PUT" "/admin/authentication" (draft (revisionOf settings) True (Just $ providerIssuer provider) (Just "different-client") Nothing),
            admin h token "DELETE" ("/admin/authentication/provider?revision=" <> T.pack (show $ revisionOf settings)) Null
          ]
          $ \action -> do
            response <- action
            expectStatus 409 response
            assertEqual "identity retarget guard" "Unlink existing SSO identities before changing issuer or client ID" (HTTP.responseBody response)
        disabled <- admin h token "PUT" "/admin/authentication" (draft (revisionOf settings) False (Just $ providerIssuer provider) (Just providerClientId) Nothing)
        expectStatus 200 disabled
        sessionStatus h sso 401
        sessionStatus h local 200
        admin h token "DELETE" (userPath uid <> "/password") Null >>= expectStatus 409
    ]
