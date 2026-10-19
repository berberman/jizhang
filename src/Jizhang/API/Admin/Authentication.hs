{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Operator-managed authentication policy and invitation issuance. Provider changes
-- use revision checks; environment-owned policy rejects database-managed edits.
module Jizhang.API.Admin.Authentication
  ( AdminAuthenticationAPI,
    adminAuthenticationServer,
    applyEnvironmentAuthentication,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (AdminPrincipal))
import Jizhang.API.Types (AuthAdmin (authAdminId), UserId (..))
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Context (providerSettingsFromRow)
import Jizhang.Auth.Security
import Jizhang.Config (EnvironmentAuthentication (..))
import qualified Jizhang.Database.AdminAuthentication as D
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U
import qualified Jizhang.OIDC as O
import Servant
import qualified Web.OIDC.Client as OIDC

-- | Full operator paths manage policy, test discovery and issue/revoke user grants.
-- The enclosing API supplies operator authentication rather than group membership.
type AdminAuthenticationAPI =
  "admin" :> "authentication" :> Get '[JSON] DTO.AuthenticationSettings
    :<|> "admin" :> "authentication" :> ReqBody '[JSON] DTO.AuthenticationSettingsRequest :> Put '[JSON] DTO.AuthenticationSettings
    :<|> "admin" :> "authentication" :> "provider" :> QueryParam' '[Required, Strict] "revision" Int64 :> Verb 'DELETE 204 '[JSON] NoContent
    :<|> "admin" :> "authentication" :> "test" :> ReqBody '[JSON] DTO.AuthenticationSettingsRequest :> Post '[JSON] DTO.AuthenticationTestResult
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "password-reset" :> Post '[JSON] DTO.InvitationIssued
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "identity-invitation" :> Post '[JSON] DTO.InvitationIssued
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "invitations" :> Capture "invitationId" UUID :> Verb 'DELETE 204 '[JSON] NoContent

adminAuthenticationServer :: AuthAdmin -> ServerT AdminAuthenticationAPI MyHandler
adminAuthenticationServer admin =
  getSettings
    :<|> (\request -> enforceExpensiveLimit (AdminPrincipal $ authAdminId admin) >> putSettings request)
    :<|> clearProvider
    :<|> (\request -> enforceExpensiveLimit (AdminPrincipal $ authAdminId admin) >> testSettings request)
    :<|> issueInvitation admin "password_reset"
    :<|> issueInvitation admin "oidc_link"
    :<|> revokeInvitation

badRequest :: Text -> MyHandler a
badRequest message = throwError $ err400 {errBody = BL.fromStrict (TE.encodeUtf8 message)}

conflict :: Text -> MyHandler a
conflict message = throwError $ err409 {errBody = BL.fromStrict (TE.encodeUtf8 message)}

unavailable :: Text -> MyHandler a
unavailable message = throwError $ err503 {errBody = BL.fromStrict (TE.encodeUtf8 message)}

settingsChanged, linkedIdentities :: Text
settingsChanged = "Authentication settings changed; reload before saving"
linkedIdentities = "Unlink existing SSO identities before changing issuer or client ID"

getSettings :: MyHandler DTO.AuthenticationSettings
getSettings = do
  managed <- asks (isJust . appEnvironmentAuthentication)
  runtime <- asks appAuthRuntime
  (row, count) <- runDBSensitive $ do
    row <- A.getAuthSettings
    count <- D.getSsoOnlyUserCount row
    pure (row, count)
  pure (settingsResponse managed runtime row count)

-- | Report secret presence and runtime readiness without exposing stored ciphertext.
settingsResponse :: Bool -> O.AuthRuntime -> S.AuthSettings -> Int -> DTO.AuthenticationSettings
settingsResponse managed runtime row count =
  DTO.AuthenticationSettings
    (S._settingsRevision row)
    managed
    (S._settingsLocalRegistrationEnabled row)
    (S._settingsSsoRegistrationEnabled row)
    (S._settingsOidcEnabled row)
    (S._settingsProviderName row)
    (S._settingsIssuer row)
    (S._settingsClientId row)
    (isJust (S._settingsClientSecretEncrypted row))
    (either (const Nothing) Just (O.callbackUrl runtime))
    (either (const Nothing) Just (O.backchannelLogoutUrl runtime))
    (either (const Nothing) Just (O.postLogoutRedirectUrl runtime))
    (either (const False) (const True) readiness)
    (either Just (const Nothing) readiness)
    count
  where
    readiness = do
      O.runtimeReady runtime
      case S._settingsClientSecretEncrypted row of
        Nothing -> pure ()
        Just encrypted -> do
          key <- maybe (Left "Configure OIDC_SECRET_KEY") Right (O.authSecretKey runtime)
          _ <- decryptSecret key "auth_settings:1:client_secret" encrypted
          pure ()

-- Encryption and discovery deliberately happen before entering the write transaction.
prepareDraft :: S.AuthSettings -> DTO.AuthenticationSettingsRequest -> MyHandler S.AuthSettings
prepareDraft current (DTO.AuthenticationSettingsRequest revision local sso enabled name issuer client secret) = do
  unless (revision == S._settingsRevision current) (conflict settingsChanged)
  unless (T.length name >= 1 && T.length name <= 100 && not (T.any isControl name)) $
    badRequest "Provider name must be 1–100 non-control characters"
  mapM_ (either badRequest (const (pure ())) . O.validateBaseUrl False) issuer
  mapM_ (\value -> when (T.null (T.strip value) || T.any isControl value) (badRequest "Client ID must not be blank or contain control characters")) client
  runtime <- asks appAuthRuntime
  encrypted <- case secret of
    -- Omitted secrets mean "keep" for API drafts; environment ownership overrides that below.
    Nothing -> pure (S._settingsClientSecretEncrypted current)
    Just value -> do
      when (T.null (T.strip value)) (badRequest "Client secret must not be blank")
      key <- maybe (unavailable "Configure OIDC_SECRET_KEY before saving a client secret") pure (O.authSecretKey runtime)
      -- The fixed row/purpose binding prevents ciphertext from another secret field being substituted.
      Just <$> liftIO (encryptSecret key "auth_settings:1:client_secret" value)
  pure
    current
      { S._settingsLocalRegistrationEnabled = local,
        S._settingsSsoRegistrationEnabled = sso,
        S._settingsOidcEnabled = enabled,
        S._settingsProviderName = name,
        S._settingsIssuer = issuer,
        S._settingsClientId = client,
        S._settingsClientSecretEncrypted = encrypted
      }

providerForDraft :: O.AuthRuntime -> S.AuthSettings -> MyHandler O.ProviderSettings
providerForDraft runtime row =
  either badRequest pure $
    providerSettingsFromRow runtime (row {S._settingsOidcEnabled = True})

validateConnection :: O.AuthRuntime -> S.AuthSettings -> MyHandler O.VerifiedProvider
validateConnection runtime row = do
  settings <- providerForDraft runtime row
  result <- liftIO (O.discoverProvider runtime settings)
  case result of
    Right provider -> pure provider
    Left O.ProviderUnavailable -> unavailable "OIDC provider discovery or JWKS is unavailable"
    Left _ -> badRequest "OIDC provider discovery or JWKS validation failed"

ensureDatabaseManaged :: MyHandler ()
ensureDatabaseManaged = do
  managed <- asks (isJust . appEnvironmentAuthentication)
  when managed (conflict "Authentication settings are managed by environment variables")

-- Environment ownership is authoritative, including clearing omitted secrets.
-- Compare plaintext before preparing the draft so unchanged secrets retain their
-- ciphertext and an identical startup does not revoke live authentication state.
applyEnvironmentAuthentication :: EnvironmentAuthentication -> MyHandler ()
applyEnvironmentAuthentication configuration = do
  current <- runDBSensitive A.getAuthSettings
  runtime <- asks appAuthRuntime
  when (envOidcEnabled configuration) $
    either unavailable pure (O.runtimeReady runtime)
  changedSecret <- case (envClientSecret configuration, S._settingsClientSecretEncrypted current) of
    (Just desired, Just encrypted) -> do
      key <- maybe (unavailable "Configure OIDC_SECRET_KEY") pure (O.authSecretKey runtime)
      existing <- either unavailable pure (decryptSecret key "auth_settings:1:client_secret" encrypted)
      pure $ if secureEqual (TE.encodeUtf8 desired) (TE.encodeUtf8 existing) then Nothing else Just desired
    (desired, Nothing) -> pure desired
    (Nothing, Just _) -> pure Nothing
  prepared <-
    prepareDraft current $
      DTO.AuthenticationSettingsRequest
        (S._settingsRevision current)
        (envLocalRegistrationEnabled configuration)
        (envSsoRegistrationEnabled configuration)
        (envOidcEnabled configuration)
        (envProviderName configuration)
        (envIssuer configuration)
        (envClientId configuration)
        changedSecret
  let draft = case envClientSecret configuration of
        Nothing -> prepared {S._settingsClientSecretEncrypted = Nothing}
        Just _ -> prepared
  when (S._settingsOidcEnabled draft) $ do
    _ <- providerForDraft runtime draft
    pure ()
  unless (draft == current) (commitSettings current draft)

-- | Reject environment-owned edits and compare the submitted provider revision on save.
putSettings :: DTO.AuthenticationSettingsRequest -> MyHandler DTO.AuthenticationSettings
putSettings request = do
  ensureDatabaseManaged
  current <- runDBSensitive A.getAuthSettings
  draft <- prepareDraft current request
  commitSettings current draft
  getSettings

-- Encryption and required discovery precede the sensitive write transaction.
-- Explicit admin saves intentionally commit even when the draft is unchanged.
commitSettings :: S.AuthSettings -> S.AuthSettings -> MyHandler ()
commitSettings current draft = do
  runtime <- asks appAuthRuntime
  let connectionChanged =
        S._settingsIssuer current /= S._settingsIssuer draft
          || S._settingsClientId current /= S._settingsClientId draft
      proofRequired =
        not (S._settingsOidcEnabled current)
          || connectionChanged
          || S._settingsClientSecretEncrypted current /= S._settingsClientSecretEncrypted draft
  when connectionChanged $ do
    -- Issuer/client changes would reinterpret existing identity links under a different login authority.
    identities <- runDBSensitive D.getAllIdentities
    unless (null identities) (conflict linkedIdentities)
  when (S._settingsOidcEnabled draft) $ do
    _ <- providerForDraft runtime draft
    when proofRequired $ do
      _ <- validateConnection runtime draft
      pure ()
  now <- liftIO (O.authClock runtime)
  result <- runDBSensitiveTransaction $ do
    latest <- A.getAuthSettings
    identities <- D.getAllIdentities
    -- Discovery ran outside the transaction; recheck both policy and links before accepting that proof.
    if latest /= current
      then pure (Left settingsChanged)
      else
        if connectionChanged && not (null identities)
          then pure (Left linkedIdentities)
          else do
            A.saveAuthSettings (S._settingsRevision current) draft
            -- Policy and grant invalidation must commit together; no old flow may use the new revision.
            A.invalidateProviderGrants now (not (S._settingsOidcEnabled draft))
            pure (Right ())
  either conflict pure result
  -- Drop cached discovery only after the new persisted policy has committed successfully.
  liftIO (O.invalidateProviderCache runtime)

-- | Clearing a provider requires the current revision and no remaining linked identities.
clearProvider :: Int64 -> MyHandler NoContent
clearProvider revision = do
  ensureDatabaseManaged
  runtime <- asks appAuthRuntime
  now <- liftIO (O.authClock runtime)
  result <- runDBSensitiveTransaction $ do
    current <- A.getAuthSettings
    identities <- D.getAllIdentities
    if S._settingsRevision current /= revision
      then pure (Left settingsChanged)
      else
        if not (null identities)
          then pure (Left linkedIdentities)
          else do
            A.saveAuthSettings
              revision
              ( current
                  { S._settingsOidcEnabled = False,
                    S._settingsIssuer = Nothing,
                    S._settingsClientId = Nothing,
                    S._settingsClientSecretEncrypted = Nothing
                  }
              )
            A.invalidateProviderGrants now True
            pure (Right ())
  either conflict pure result
  liftIO (O.invalidateProviderCache runtime)
  pure NoContent

-- | Discover and validate an unsaved draft without changing the persisted policy.
testSettings :: DTO.AuthenticationSettingsRequest -> MyHandler DTO.AuthenticationTestResult
testSettings request = do
  ensureDatabaseManaged
  current <- runDBSensitive A.getAuthSettings
  draft <- prepareDraft current request
  runtime <- asks appAuthRuntime
  provider <- validateConnection runtime draft
  let configuration = O.providerConfiguration provider
  pure $
    DTO.AuthenticationTestResult
      (OIDC.issuer configuration)
      (OIDC.authorizationEndpoint configuration)
      (OIDC.tokenEndpoint configuration)
      (OIDC.jwksUri configuration)
      (OIDC.userinfoEndpoint configuration)
      (O.providerEndSessionEndpoint provider)
      True
      (O.providerBackchannelLogoutSupported provider)

-- | Issue a 30-minute grant and revoke older grants for the same account/purpose;
-- only the returned URL contains the bearer token, while storage keeps its digest.
issueInvitation :: AuthAdmin -> Text -> UserId -> MyHandler DTO.InvitationIssued
issueInvitation admin purpose (UserId uid) = do
  runtime <- asks appAuthRuntime
  either unavailable pure (O.runtimeReady runtime)
  frontend <- maybe (unavailable "Configure FRONTEND_URL before generating invitations") pure (O.authFrontendUrl runtime)
  (user, settings, identity) <- runDBSensitive $ (,,) <$> U.getUserById uid <*> A.getAuthSettings <*> A.getUserIdentity uid
  target <- maybe (throwError err404) pure user
  when (purpose == "oidc_link") $ do
    unless (S._enabled target) (conflict "Enable the user before generating an SSO linking invitation")
    when (isJust identity) (conflict "This account already has an SSO identity")
    _ <- either unavailable pure (providerSettingsFromRow runtime settings)
    pure ()
  token <- liftIO randomToken
  invitationId <- liftIO UUID.nextRandom
  now <- liftIO (O.authClock runtime)
  let expires = addUTCTime 1800 now
      row =
        S.AccountInvitation
          invitationId
          (tokenDigest token)
          purpose
          (S.UserId uid)
          (S.AdminId (authAdminId admin))
          (if purpose == "oidc_link" then Just (S._settingsRevision settings) else Nothing)
          now
          expires
          Nothing
          Nothing
  runDBSensitiveTransaction $ do
    currentUser <- U.getUserById uid
    currentSettings <- A.getAuthSettings
    currentIdentity <- A.getUserIdentity uid
    unless
      ( currentUser == Just target
          && ( purpose /= "oidc_link"
                 || (currentSettings == settings && currentIdentity == identity)
             )
      )
      A.abortAuthTransaction
    A.cleanupExpiredAuth now
    -- Replacement is atomic: concurrent issuance must not leave two live grants for one purpose.
    A.revokeInvitationsByPurpose uid purpose now
    A.insertAccountInvitation row
  -- Publish the bearer URL only after its digest and replacement revocations have committed.
  pure (DTO.InvitationIssued invitationId (frontend <> "#/auth/invitation?token=" <> token) expires)

-- | Scope revocation to the target account and fail flows bound to the unused grant.
revokeInvitation :: UserId -> UUID -> MyHandler NoContent
revokeInvitation (UserId uid) invitationId = do
  runtime <- asks appAuthRuntime
  now <- liftIO (O.authClock runtime)
  found <- runDBSensitiveTransaction $ do
    invitation <- A.getAccountInvitation invitationId
    case invitation of
      Just row
        | S._invitationUser row == S.UserId uid,
          S._invitationConsumedAt row == Nothing,
          S._invitationRevokedAt row == Nothing -> do
            A.saveAccountInvitation row (row {S._invitationRevokedAt = Just now})
            -- An already-started flow must fail too, not merely future uses of the invitation URL.
            A.failInvitationTransactions invitationId
            A.cleanupExpiredAuth now
            pure True
      _ -> pure False
  unless found (throwError err404)
  pure NoContent
