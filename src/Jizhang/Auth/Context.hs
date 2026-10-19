{-# LANGUAGE OverloadedStrings #-}

-- | Database-backed account checks for sensitive authentication actions.
-- JWT claims identify rows; current account, session and identity state decide access.
module Jizhang.Auth.Context where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Data.Time
import Database.Beam.Postgres (Pg)
import Jizhang.API.App
import Jizhang.API.Types (AuthUser (..))
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Security
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U
import qualified Jizhang.OIDC as O
import Servant

-- | Require an enabled, configured provider and decrypt its row-bound credential.
providerSettingsFromRow :: O.AuthRuntime -> S.AuthSettings -> Either Text O.ProviderSettings
providerSettingsFromRow runtime row = do
  O.runtimeReady runtime
  unless (S._settingsOidcEnabled row) (Left "SSO is disabled")
  issuer <- maybe (Left "Configure the OIDC issuer") Right (S._settingsIssuer row)
  client <- maybe (Left "Configure the OIDC client ID") Right (S._settingsClientId row)
  encrypted <- maybe (Left "Configure the OIDC client secret") Right (S._settingsClientSecretEncrypted row)
  key <- maybe (Left "Configure OIDC_SECRET_KEY") Right (O.authSecretKey runtime)
  secret <- decryptSecret key "auth_settings:1:client_secret" encrypted
  pure (O.ProviderSettings (S._settingsRevision row) issuer client secret)

-- | Hide configuration/decryption details behind a generic availability response.
loadProviderSettings :: MyHandler (S.AuthSettings, O.ProviderSettings)
loadProviderSettings = do
  runtime <- asks appAuthRuntime
  row <- runDBSensitive A.getAuthSettings
  settings <- either (const (throwError $ err503 {errBody = "SSO is unavailable; contact an administrator"})) pure (providerSettingsFromRow runtime row)
  pure (row, settings)

-- | Reload the account and session rather than treating signed claims as live authority.
currentAccount :: AuthUser -> MyHandler (S.User, S.UserSession, Maybe S.ExternalIdentity)
currentAccount auth = do
  runtime <- asks appAuthRuntime
  now <- liftIO (O.authClock runtime)
  result <- runDBSensitive $ do
    user <- U.getUserById (authUserId auth)
    session <- A.getUserSession (authSessionId auth)
    identity <- A.getUserIdentity (authUserId auth)
    pure (user, session, identity)
  case result of
    (Just user, Just session, identity) | S._enabled user, activeSession now user session -> pure (user, session, identity)
    _ -> throwError err401

-- | Account-wide invalidation excludes sessions created at or before its timestamp.
activeSession :: UTCTime -> S.User -> S.UserSession -> Bool
activeSession now user session =
  S._sessionUser session == S.UserId (S._userId user)
    && S._sessionRevokedAt session == Nothing
    && S._sessionExpiresAt session > now
    -- Equality is revoked too: timestamps from the same database tick must not let
    -- a pre-reset session survive merely because creation and invalidation coincide.
    && maybe True (S._sessionCreatedAt session >) (S._authInvalidatedAt user)

-- | Abort if the caller's snapshot changed before a transactional account mutation.
checkAccountSnapshot :: UTCTime -> S.User -> S.UserSession -> Maybe S.ExternalIdentity -> Pg ()
checkAccountSnapshot now user session identity = do
  current <- U.getUserById (S._userId user)
  currentSession <- A.getUserSession (S._sessionId session)
  currentIdentity <- A.getUserIdentity (S._userId user)
  -- Reauthentication may perform slow proof checks before entering this transaction.
  -- Comparing the entire snapshot prevents those proofs from authorizing changed credentials.
  unless
    ( current == Just user
        && S._enabled user
        && currentSession == Just session
        && activeSession now user session
        && currentIdentity == identity
    )
    A.abortAuthTransaction

-- | Require authentication within ten minutes, rejecting future timestamps too.
requireFreshAuthentication :: UTCTime -> S.UserSession -> MyHandler ()
requireFreshAuthentication now session = unless (fresh now (S._sessionAuthenticatedAt session)) reauthenticationRequired

-- | Require recent provider reauthentication for this exact issuer/subject pair.
requireFreshOidc :: UTCTime -> S.UserSession -> S.ExternalIdentity -> MyHandler ()
requireFreshOidc now session identity =
  -- A recent local-password login cannot stand in for fresh proof of this provider
  -- identity; both the authentication method and the exact issuer/subject must match.
  unless
    ( S._sessionMethod session == "oidc"
        && S._sessionOidcIssuer session == Just (S._identityIssuer identity)
        && S._sessionOidcSubject session == Just (S._identitySubject identity)
        && maybe False (fresh now) (S._sessionOidcReauthenticatedAt session)
    )
    reauthenticationRequired

fresh :: UTCTime -> UTCTime -> Bool
fresh now value = value <= now && value >= addUTCTime (-600) now

reauthenticationRequired :: MyHandler a
reauthenticationRequired = throwError $ err403 {errBody = "Reauthentication required"}

-- | Provider email and display claims are not account identity keys.
matchesIdentity :: S.ExternalIdentity -> O.VerifiedOidcIdentity -> Bool
matchesIdentity row identity = S._identityIssuer row == O.verifiedIssuer identity && S._identitySubject row == O.verifiedSubject identity

-- | Expose profile claims as metadata alongside the stable issuer/subject identity.
identitySummary :: Text -> S.ExternalIdentity -> DTO.IdentitySummary
identitySummary providerName row =
  DTO.IdentitySummary
    (S._identityIssuer row)
    (S._identitySubject row)
    providerName
    (S._identityClaimedEmail row)
    (S._identityClaimedEmailVerified row)
    (S._identityLinkedAt row)
    (S._identityLastLoginAt row)

-- | Describe an uncommitted identity candidate without implying it is linked.
identityCandidate :: Text -> O.VerifiedOidcIdentity -> DTO.IdentityCandidate
identityCandidate providerName identity =
  DTO.IdentityCandidate
    (O.verifiedIssuer identity)
    (O.verifiedSubject identity)
    providerName
    (O.profileEmail profile)
    (O.profileEmailVerified profile)
  where
    profile = O.verifiedProfile identity
