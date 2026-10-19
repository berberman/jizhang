{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Manage sign-in methods for the current account. Mutations compare account,
-- session, identity and provider snapshots inside serializable transactions.
module Jizhang.API.AccountAuthentication
  ( AccountAuthenticationAPI,
    accountAuthenticationServer,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (validatePassword)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Beam.Postgres (Pg)
import Jizhang.API.Logout (LogoutAPI, logoutServer)
import Jizhang.API.OIDC (startAccountOidc)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceUserCredentialLimit)
import Jizhang.API.Types (AuthUser)
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.API.User.Account (validatePasswordText, hashPasswordText)
import Jizhang.Auth.Context (checkAccountSnapshot, currentAccount, identitySummary, providerSettingsFromRow, requireFreshOidc)
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.OIDC as O
import Servant

-- | Self-service authentication state, reauthentication, method changes and logout.
type AccountAuthenticationAPI = CoreAccountAuthenticationAPI :<|> LogoutAPI

type CoreAccountAuthenticationAPI =
  "users"
    :> "me"
    :> ( "authentication" :> Get '[JSON] DTO.AccountAuthentication
           :<|> "reauthenticate" :> ReqBody '[JSON] DTO.ReauthenticateRequest :> Verb 'POST 204 '[JSON] NoContent
           :<|> "oidc" :> "start" :> ReqBody '[JSON] DTO.AccountOidcStartRequest :> Post '[JSON] DTO.OidcStartResponse
           :<|> "password" :> ReqBody '[JSON] DTO.PasswordChangeRequest :> Verb 'PUT 204 '[JSON] NoContent
           :<|> "password" :> Verb 'DELETE 204 '[JSON] NoContent
           :<|> "identity" :> "unlink" :> ReqBody '[JSON] DTO.IdentityUnlinkRequest :> Verb 'POST 204 '[JSON] NoContent
       )

accountAuthenticationServer :: AuthUser -> MyServer AccountAuthenticationAPI
accountAuthenticationServer auth =
  ( authentication auth
      :<|> reauthenticate auth
      :<|> startAccountOidc auth
      :<|> changePassword auth
      :<|> removePassword auth
      :<|> unlinkIdentity auth
  )
    :<|> logoutServer auth

authentication :: AuthUser -> MyHandler DTO.AccountAuthentication
authentication auth = do
  (user, session, identity) <- currentAccount auth
  settings <- runDBSensitive A.getAuthSettings
  runtime <- asks appAuthRuntime
  let providerSettings = providerSettingsFromRow runtime settings
      enabled = S._settingsOidcEnabled settings && isRight providerSettings
      summary = identitySummary (S._settingsProviderName settings)
  canLogout <- case providerSettings of
    -- A logout hint is usable only for the identity/provider that authenticated this session.
    Right provider
      | enabled,
        S._sessionMethod session == "oidc",
        isJust (S._sessionIdTokenEncrypted session),
        Just linked <- identity,
        S._settingsIssuer settings == Just (S._identityIssuer linked),
        S._sessionOidcIssuer session == Just (S._identityIssuer linked),
        S._sessionOidcSubject session == Just (S._identitySubject linked) -> do
          discovered <- liftIO $ O.cachedProvider runtime provider
          pure $ either (const False) (isJust . O.providerEndSessionEndpoint) discovered
    _ -> pure False
  pure $
    DTO.AccountAuthentication
      (isJust $ S._passwordHash user)
      (summary <$> identity)
      (if S._sessionMethod session == "oidc" then DTO.OidcMethod else DTO.PasswordMethod)
      (S._sessionAuthenticatedAt session)
      (S._sessionOidcReauthenticatedAt session)
      enabled
      canLogout

reauthenticate :: AuthUser -> DTO.ReauthenticateRequest -> MyHandler NoContent
reauthenticate auth (DTO.ReauthenticateRequest password) = do
  (user, session, identity) <- currentAccount auth
  enforceUserCredentialLimit (Just (S._userId user)) (S._username user)
  settings <- runDBSensitive A.getAuthSettings
  requirePassword user (Just password)
  now <- currentTime
  runDBSensitiveTransaction $ do
    checkSnapshot now user session identity settings
    -- Password proof refreshes local authentication, not the separate provider reauthentication time.
    A.saveUserSession session (session {S._sessionAuthenticatedAt = now})
  pure NoContent

-- | Existing passwords require password proof; passwordless accounts require fresh
-- provider proof. Committing the new password revokes prior authentication state.
changePassword :: AuthUser -> DTO.PasswordChangeRequest -> MyHandler NoContent
changePassword auth (DTO.PasswordChangeRequest currentPassword newPassword) = do
  (user, session, identity) <- currentAccount auth
  enforceUserCredentialLimit (Just (S._userId user)) (S._username user)
  settings <- runDBSensitive A.getAuthSettings
  case S._passwordHash user of
    Just _ -> requirePassword user currentPassword
    Nothing -> do
      when (isJust currentPassword) incorrectPassword
      requireViableOidc settings identity
      now <- currentTime
      maybe reauthenticationRequired (requireFreshOidc now session) identity
  validatePasswordText newPassword
  hash <- hashPasswordText newPassword
  now <- currentTime
  -- Hashing and password verification happen outside the serializable lock.
  -- Repeat time-sensitive proof after hashing, then compare every snapshot in Pg.
  when (S._passwordHash user == Nothing) $
    maybe reauthenticationRequired (requireFreshOidc now session) identity
  runDBSensitiveTransaction $ do
    checkSnapshot now user session identity settings
    A.saveUserAuthentication user (user {S._passwordHash = Just hash})
    -- Commit the password and revocation together, leaving no old session valid under new credentials.
    A.revokeUserAuthentication (S._userId user) now
    A.cleanupExpiredAuth now
  pure NoContent

-- | Require a usable linked provider and fresh OIDC proof before removing a password.
removePassword :: AuthUser -> MyHandler NoContent
removePassword auth = do
  (user, session, identity) <- currentAccount auth
  settings <- runDBSensitive A.getAuthSettings
  -- A linked row alone is not a fallback if its provider is disabled or its issuer no longer matches.
  requireViableOidc settings identity
  now <- currentTime
  maybe reauthenticationRequired (requireFreshOidc now session) identity
  runDBSensitiveTransaction $ do
    checkSnapshot now user session identity settings
    A.saveUserAuthentication user (user {S._passwordHash = Nothing})
    A.revokeUserAuthentication (S._userId user) now
    A.cleanupExpiredAuth now
  pure NoContent

-- | Password proof protects unlinking, and a local password must remain afterward.
unlinkIdentity :: AuthUser -> DTO.IdentityUnlinkRequest -> MyHandler NoContent
unlinkIdentity auth (DTO.IdentityUnlinkRequest password) = do
  (user, session, identity) <- currentAccount auth
  settings <- runDBSensitive A.getAuthSettings
  unless (isJust $ S._passwordHash user) cannotRemoveLastMethod
  -- Unlinking must leave a password usable without relying on the identity being removed.
  requirePassword user (Just password)
  linked <- maybe (throwError $ err409 {errBody = "This account has no SSO identity"}) pure identity
  now <- currentTime
  runDBSensitiveTransaction $ do
    checkSnapshot now user session identity settings
    A.deleteExternalIdentity linked
    A.revokeUserAuthentication (S._userId user) now
    A.cleanupExpiredAuth now
  pure NoContent

-- | Reject changed snapshots and provider logout events that invalidate this proof.
checkSnapshot :: UTCTime -> S.User -> S.UserSession -> Maybe S.ExternalIdentity -> S.AuthSettings -> Pg ()
checkSnapshot now user session identity settings = do
  checkAccountSnapshot now user session identity
  currentSettings <- A.getAuthSettings
  unless (currentSettings == settings) A.abortAuthTransaction
  when (S._sessionMethod session == "oidc") $ case S._sessionOidcIssuer session of
    -- Logout may arrive after proof was checked; include matching events in the mutation's snapshot.
    Just issuer -> do
      events <- A.getLogoutEventsSince issuer (S._sessionCreatedAt session) now
      let matches event = case S._logoutEventSid event of
            Just sid ->
              S._sessionOidcSid session == Just sid
                && maybe True (\subject -> S._sessionOidcSubject session == Just subject) (S._logoutEventSub event)
            Nothing ->
              isJust (S._logoutEventSub event)
                && S._logoutEventSub event == S._sessionOidcSubject session
      when (any matches events) A.abortAuthTransaction
    Nothing -> A.abortAuthTransaction

requireViableOidc :: S.AuthSettings -> Maybe S.ExternalIdentity -> MyHandler ()
requireViableOidc settings identity = do
  runtime <- asks appAuthRuntime
  unless
    ( S._settingsOidcEnabled settings
        && isRight (providerSettingsFromRow runtime settings)
        && maybe False (\row -> S._settingsIssuer settings == Just (S._identityIssuer row)) identity
    )
    cannotRemoveLastMethod

requirePassword :: S.User -> Maybe Text -> MyHandler ()
requirePassword user password = unless valid incorrectPassword
  where
    valid = case (S._passwordHash user, password) of
      (Just hash, Just plaintext) ->
        BS.length (encodeUtf8 plaintext) <= 72
          && validatePassword (encodeUtf8 hash) (encodeUtf8 plaintext)
      _ -> False

incorrectPassword :: MyHandler a
incorrectPassword = throwError $ err400 {errBody = "Current password is incorrect"}

cannotRemoveLastMethod :: MyHandler a
cannotRemoveLastMethod = throwError $ err409 {errBody = "Cannot remove the last sign-in method"}

reauthenticationRequired :: MyHandler a
reauthenticationRequired = throwError $ err403 {errBody = "Reauthentication required"}

currentTime :: MyHandler UTCTime
currentTime = asks appAuthRuntime >>= liftIO . O.authClock
