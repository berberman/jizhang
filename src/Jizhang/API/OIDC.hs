{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Browser-bound OIDC sign-in and account-linking routes. Persisted flows bind
-- provider revision, purpose and optional account/session/invitation snapshots.
module Jizhang.API.OIDC (OIDCAPI, oidcServer, startAccountOidc) where

import Control.Monad (unless, void, when)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (validatePassword)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Beam.Postgres (Pg)
import Jizhang.API.App (AppEnv (..), MyHandler, MyServer, runDBSensitive, runDBSensitiveTransaction)
import Jizhang.API.Internal.Error (textToLBS)
import Jizhang.API.Invitation (InvitationAPI, invitationServer)
import Jizhang.API.Logout (OIDCLogoutAPI, oidcLogoutServer)
import Jizhang.API.RequestProtection (enforceUserCredentialLimit)
import Jizhang.API.Types (AuthUser (..), LoginResponse)
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.API.User.Account (normalizeDisplayName, normalizeEmail, normalizeUsername)
import Jizhang.API.User.Projection (schemaUserToApiUser)
import Jizhang.Auth.Context
import Jizhang.Auth.Security
import Jizhang.Auth.Session
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U
import qualified Jizhang.OIDC as O
import Network.HTTP.Types.URI (renderQuery)
import Servant

-- | Public flow endpoints compose with invitation redemption and provider logout.
type OIDCAPI = CoreOIDCAPI :<|> InvitationAPI :<|> OIDCLogoutAPI

type CoreOIDCAPI =
  "auth"
    :> ( "options" :> Get '[JSON] DTO.AuthOptions
           :<|> "oidc"
             :> ( "start" :> ReqBody '[JSON] DTO.OidcStartRequest :> Post '[JSON] DTO.OidcStartResponse
                    :<|> "callback" :> QueryParams "state" Text :> QueryParams "code" Text :> QueryParams "error" Text :> QueryParams "iss" Text :> Get '[JSON] NoContent
                    :<|> "exchange" :> ReqBody '[JSON] DTO.OidcExchangeRequest :> Post '[JSON] DTO.OidcExchangeResponse
                    :<|> "register" :> ReqBody '[JSON] DTO.OidcRegisterRequest :> Post '[JSON] LoginResponse
                    :<|> "link-existing" :> ReqBody '[JSON] DTO.OidcLinkExistingRequest :> Post '[JSON] LoginResponse
                    :<|> "confirm-link" :> ReqBody '[JSON] DTO.OidcContinuationRequest :> Post '[JSON] LoginResponse
                    :<|> "cancel" :> ReqBody '[JSON] DTO.OidcContinuationRequest :> PostNoContent
                )
       )

oidcServer :: MyServer OIDCAPI
oidcServer = (options :<|> (start :<|> callback :<|> exchange :<|> register :<|> linkExisting :<|> confirmLink :<|> cancel)) :<|> invitationServer :<|> oidcLogoutServer

invalid :: MyHandler a
invalid = throwError $ err400 {errBody = "This sign-in request is invalid or expired"}

clock :: MyHandler UTCTime
clock = asks appAuthRuntime >>= liftIO . O.authClock

secretKey :: MyHandler SecretKey
secretKey = asks (O.authSecretKey . appAuthRuntime) >>= maybe (throwError err503) pure

aad :: UUID -> Text -> ByteString
aad ident purpose = TE.encodeUtf8 ("oidc_transactions:" <> UUID.toText ident <> ":" <> purpose)

-- | Bearer proofs must be canonical unpadded encodings of exactly 32 bytes.
validToken :: Text -> Bool
validToken value =
  T.length value == 43 && case B64.decodeUnpadded (TE.encodeUtf8 value) of
    Right bytes -> BS.length bytes == 32 && TE.decodeUtf8 (B64.encodeUnpadded bytes) == value
    _ -> False

options :: MyHandler DTO.AuthOptions
options = do
  runtime <- asks appAuthRuntime
  row <- runDBSensitive A.getAuthSettings
  pure $
    DTO.AuthOptions
      (S._settingsLocalRegistrationEnabled row)
      (S._settingsSsoRegistrationEnabled row)
      (either (const False) (const True) (providerSettingsFromRow runtime row))
      (S._settingsProviderName row)

start :: DTO.OidcStartRequest -> MyHandler DTO.OidcStartResponse
start (DTO.OidcStartRequest challenge invitationToken) = do
  (settings, _) <- loadProviderSettings
  now <- clock
  invitation <- traverse (loadInvitation now settings) invitationToken
  target <- traverse (\row -> runDBSensitive (U.getUserById (S.unUserId (S._invitationUser row))) >>= maybe invalid pure) invitation
  when (maybe False (not . S._enabled) target) invalid
  createFlow settings challenge (if isJust invitation then "invitation" else "login") target Nothing invitation

loadInvitation :: UTCTime -> S.AuthSettings -> Text -> MyHandler S.AccountInvitation
loadInvitation now settings token = do
  unless (validToken token) invalid
  result <- runDBSensitive $ A.getAccountInvitationByTokenHash (tokenDigest token)
  case result of
    Just row | validInvitation now settings row -> pure row
    _ -> invalid

validInvitation :: UTCTime -> S.AuthSettings -> S.AccountInvitation -> Bool
validInvitation now settings row =
  S._invitationPurpose row == "oidc_link"
    && S._invitationExpiresAt row > now
    && isNothing (S._invitationConsumedAt row)
    && isNothing (S._invitationRevokedAt row)
    && S._invitationProviderRevision row == Just (S._settingsRevision settings)

-- | Linking needs fresh account authentication; reauthentication requires an identity.
startAccountOidc :: AuthUser -> DTO.AccountOidcStartRequest -> MyHandler DTO.OidcStartResponse
startAccountOidc auth (DTO.AccountOidcStartRequest purpose challenge) = do
  (user, session, identity) <- currentAccount auth
  now <- clock
  case purpose of
    DTO.LinkIdentity -> requireFreshAuthentication now session
    DTO.ReauthenticateIdentity -> unless (isJust identity) (throwError $ err409 {errBody = "This account has no SSO identity"})
  (settings, _) <- loadProviderSettings
  createFlow settings challenge (case purpose of DTO.LinkIdentity -> "link"; DTO.ReauthenticateIdentity -> "reauth") (Just user) (Just session) Nothing

-- | Prepare discovery and encrypted PKCE state before the serializable insert;
-- commit only if account, session, invitation and provider snapshots still hold.
createFlow :: S.AuthSettings -> Text -> Text -> Maybe S.User -> Maybe S.UserSession -> Maybe S.AccountInvitation -> MyHandler DTO.OidcStartResponse
createFlow settings challenge purpose user session invitation = do
  unless (validToken challenge) (throwError $ err400 {errBody = "Invalid browser challenge"})
  -- This browser challenge protects the frontend handoff, independently of provider PKCE.
  -- Only its digest is stored; the originating browser must retain the verifier.
  challengeDigest <- either (const invalid) pure (B64.decodeUnpadded (TE.encodeUtf8 challenge))
  runtime <- asks appAuthRuntime
  providerSettings <- either (const (throwError err503)) pure (providerSettingsFromRow runtime settings)
  provider <- liftIO (O.cachedProvider runtime providerSettings) >>= either (const (throwError $ err503 {errBody = "SSO provider is unavailable"})) pure
  redirect <- either (const (throwError err503)) pure (O.callbackUrl runtime)
  key <- secretKey
  ident <- liftIO UUID.nextRandom
  -- State correlates the callback; nonce binds the signed ID token; PKCE binds code redemption.
  -- Unlike the hashes, the PKCE verifier must be recoverable for the later token request.
  state <- liftIO randomToken
  nonce <- liftIO randomToken
  verifier <- liftIO randomToken
  encrypted <- liftIO $ encryptSecret key (aad ident "pkce_verifier") verifier
  now <- clock
  let row =
        S.OidcTransaction
          ident
          (tokenDigest state)
          (S._settingsRevision settings)
          purpose
          (S.UserId (S._userId <$> user))
          (S.UserSessionId (S._sessionId <$> session))
          (S.AccountInvitationId (S._invitationId <$> invitation))
          challengeDigest
          (tokenDigest nonce)
          encrypted
          now
          (addUTCTime 600 now)
          "waiting"
          Nothing
          Nothing
          Nothing
          0
  -- Discovery and encryption ran outside the transaction. A concurrent reset or provider
  -- edit must invalidate this snapshot before we publish an authorization redirect.
  runDBSensitiveTransaction $ do
    checkSettings settings
    case (user, session) of
      (Just u, Just s) -> do
        identity <- A.getUserIdentity (S._userId u)
        checkAccountSnapshot now u s identity
        when (purpose == "link") $ unless (fresh now (S._sessionAuthenticatedAt s)) A.abortAuthTransaction
      (Just u, Nothing) -> checkUser now row u
      _ -> pure ()
    mapM_ (checkInvitation now settings) invitation
    A.insertOidcTransaction row
    A.cleanupExpiredAuth now
  pure $ DTO.OidcStartResponse ident (O.authorizationUrl provider providerSettings redirect state nonce (pkceChallenge verifier) (purpose /= "login"))

checkSettings :: S.AuthSettings -> Pg ()
checkSettings expected = do
  current <- A.getAuthSettings
  unless (current == expected && S._settingsOidcEnabled current) A.abortAuthTransaction

checkInvitation :: UTCTime -> S.AuthSettings -> S.AccountInvitation -> Pg ()
checkInvitation now settings expected = do
  current <- A.getAccountInvitation (S._invitationId expected)
  unless (current == Just expected && validInvitation now settings expected) A.abortAuthTransaction

checkUser :: UTCTime -> S.OidcTransaction -> S.User -> Pg ()
checkUser _ flow expected = do
  current <- U.getUserById (S._userId expected)
  unless
    ( current == Just expected
        && S._enabled expected
        && maybe True (S._transactionCreatedAt flow >) (S._authInvalidatedAt expected)
    )
    A.abortAuthTransaction

-- | Recheck expiry, provider revision, account/session viability and logout fences
-- before a verified provider identity can advance or complete a persisted flow.
checkFlow :: UTCTime -> S.AuthSettings -> S.OidcTransaction -> Maybe O.VerifiedOidcIdentity -> Pg ()
checkFlow now settings expected identity = do
  checkSettings settings
  -- A phase-only check would miss replaced proofs or password-attempt counters.
  -- Every transition must still own the complete flow snapshot it originally read.
  current <- A.getOidcTransaction (S._transactionId expected)
  unless
    ( current == Just expected
        && S._transactionExpiresAt expected > now
        && S._transactionProviderRevision expected == S._settingsRevision settings
    )
    A.abortAuthTransaction
  case S.unUserId (S._transactionUser expected) of
    Nothing -> pure ()
    Just uid -> do
      user <- U.getUserById uid >>= maybe A.abortAuthTransaction pure
      checkUser now expected user
      case S.unUserSessionId (S._transactionSession expected) of
        Nothing -> pure ()
        Just sid -> do
          session <- A.getUserSession sid >>= maybe A.abortAuthTransaction pure
          unless (activeSession now user session) A.abortAuthTransaction
          when (S._transactionPurpose expected == "link") $ unless (fresh now (S._sessionAuthenticatedAt session)) A.abortAuthTransaction
  case S.unAccountInvitationId (S._transactionInvitation expected) of
    Nothing -> pure ()
    Just iid -> A.getAccountInvitation iid >>= maybe A.abortAuthTransaction (checkInvitation now settings)
  -- Provider success is not local authorization: bound accounts, sessions and invitations
  -- may have been invalidated while the browser was away at the provider.
  mapM_
    ( \proof -> do
        checkLogoutFence now expected proof
        linked <- A.getExternalIdentity (O.verifiedIssuer proof) (O.verifiedSubject proof)
        case linked of
          Nothing -> pure ()
          Just existing -> U.getUserById (S.unUserId (S._identityUser existing)) >>= maybe A.abortAuthTransaction (checkUser now expected)
        when (S._transactionPurpose expected /= "login") $
          unless
            ( maybe
                False
                ( \time ->
                    time >= addUTCTime (-60) (S._transactionCreatedAt expected)
                      && time <= addUTCTime 60 now
                )
                (O.verifiedReauthenticatedAt proof)
            )
            A.abortAuthTransaction
    )
    identity

checkLogoutFence :: UTCTime -> S.OidcTransaction -> O.VerifiedOidcIdentity -> Pg ()
checkLogoutFence now flow identity = do
  -- Logout may arrive before this flow has created any session to revoke.
  -- Retained events fence in-flight identities as well as already-issued sessions.
  events <- A.getLogoutEventsSince (O.verifiedIssuer identity) (S._transactionCreatedAt flow) now
  let matches event = case S._logoutEventSid event of
        Just sid -> O.verifiedSid identity == Just sid && maybe True (== O.verifiedSubject identity) (S._logoutEventSub event)
        Nothing -> S._logoutEventSub event == Just (O.verifiedSubject identity)
  when (any matches events) A.abortAuthTransaction

-- | Claim a waiting flow before external token exchange; only a validated provider
-- response becomes an encrypted, short-lived handoff for the configured frontend.
callback :: [Text] -> [Text] -> [Text] -> [Text] -> MyHandler NoContent
callback states codes errors issuers = do
  state <- case states of [value] | not (T.null value) -> pure value; _ -> invalid
  flow <- runDBSensitive (A.getOidcTransactionByStateHash (tokenDigest state)) >>= maybe invalid pure
  let failWith category = do
        runDBSensitiveTransaction $ do
          current <- A.getOidcTransaction (S._transactionId flow)
          -- A delayed callback error must not overwrite a resolved or consumed handoff.
          case current of
            Just row | S._transactionPhase row `elem` ["waiting", "exchanging"] -> A.saveOidcTransaction row (failed row)
            _ -> pure ()
        callbackRedirect flow [("error", category)]
  unless
    ( length codes <= 1
        && length errors <= 1
        && length issuers <= 1
        && all (not . T.null) (codes <> errors <> issuers)
        && (null codes /= null errors)
    )
    (failWith "invalid_response")
  now <- clock
  unless (S._transactionPhase flow == "waiting") $ case S._transactionPhase flow of
    "failed" -> failWith "configuration_changed"
    _ -> invalid
  when (S._transactionExpiresAt flow <= now) (failWith "expired")
  runtime <- asks appAuthRuntime
  settings <- runDBSensitive A.getAuthSettings
  providerSettings <- either (const (failWith "configuration_changed")) pure (providerSettingsFromRow runtime settings)
  unless (S._transactionProviderRevision flow == S._settingsRevision settings) (failWith "configuration_changed")
  -- The optional response issuer is an extra mix-up check, not a substitute for
  -- validating the issuer inside the signed ID token returned by the token endpoint.
  unless (null issuers || issuers == [O.settingsIssuer providerSettings]) (failWith "invalid_response")
  unless (null errors) (failWith (if errors == ["access_denied"] then "access_denied" else "invalid_response"))
  code <- case codes of
    [authorizationCode] -> pure authorizationCode
    _ -> failWith "invalid_response"
  key <- secretKey
  verifier <- either (const (failWith "configuration_changed")) pure (decryptSecret key (aad (S._transactionId flow) "pkce_verifier") (S._transactionPkceVerifierEncrypted flow))
  -- Claim before HTTP so a competing callback cannot redeem the same code.
  -- The provider request runs after commit, without holding the database connection.
  let claimed = flow {S._transactionPhase = "exchanging"}
  (runDBSensitiveTransaction $ checkFlow now settings flow Nothing >> A.saveOidcTransaction flow claimed)
    `catchError` (\failure -> if errHTTPCode failure == 503 then throwError failure else failWith "configuration_changed")
  redirect <- either (const (failWith "configuration_changed")) pure (O.callbackUrl runtime)
  result <-
    liftIO $
      O.exchangeAuthorizationCode
        runtime
        providerSettings
        redirect
        code
        verifier
        (S._transactionNonceHash flow)
        (if S._transactionPurpose flow == "login" then Nothing else Just (S._transactionCreatedAt flow))
  identity <- either (failWith . O.protocolErrorCode) pure result
  handoff <- liftIO randomToken
  encrypted <- liftIO $ O.encryptVerifiedIdentity key (aad (S._transactionId flow) "result_payload") identity
  completed <- clock
  let resolved =
        claimed
          { S._transactionPhase = "resolved",
            S._transactionHandoffHash = Just (tokenDigest handoff),
            S._transactionResultPayloadEncrypted = Just encrypted,
            -- Bound both the handoff lifetime and the total time since flow creation.
            S._transactionExpiresAt = min (addUTCTime 1260 (S._transactionCreatedAt flow)) (addUTCTime 60 completed)
          }
  -- Account changes or logout during provider I/O can invalidate an otherwise valid token.
  -- Recheck those fences before making the encrypted identity available to the browser.
  (runDBSensitiveTransaction $ checkFlow completed settings claimed (Just identity) >> A.saveOidcTransaction claimed resolved)
    `catchError` (\failure -> if errHTTPCode failure == 503 then throwError failure else failWith "configuration_changed")
  callbackRedirect flow [("code", handoff)]

failed :: S.OidcTransaction -> S.OidcTransaction
failed row = row {S._transactionPhase = "failed", S._transactionResultPayloadEncrypted = Nothing}

callbackRedirect :: S.OidcTransaction -> [(Text, Text)] -> MyHandler a
callbackRedirect row fields = do
  frontend <- asks (O.authFrontendUrl . appAuthRuntime) >>= maybe (throwError err503) pure
  let query = TE.decodeUtf8 $ renderQuery True [(TE.encodeUtf8 k, Just (TE.encodeUtf8 v)) | (k, v) <- ("flow", UUID.toText (S._transactionId row)) : fields]
      -- The fragment keeps the handoff proof out of requests to the frontend HTTP server.
      destination = frontend <> "#/auth/oidc/callback" <> query
  throwError $ err303 {errHeaders = [("Location", TE.encodeUtf8 destination)], errBody = ""}

loadPayload :: S.OidcTransaction -> MyHandler O.VerifiedOidcIdentity
loadPayload row = do
  key <- secretKey
  encrypted <- maybe invalid pure (S._transactionResultPayloadEncrypted row)
  either (const invalid) pure (O.decryptVerifiedIdentity key (aad (S._transactionId row) "result_payload") encrypted)

loadContinuation :: Text -> MyHandler (S.AuthSettings, S.OidcTransaction, O.VerifiedOidcIdentity)
loadContinuation token = do
  unless (validToken token) invalid
  row <- runDBSensitive (A.getOidcTransactionByContinuationHash (tokenDigest token)) >>= maybe invalid pure
  now <- clock
  -- Loading a continuation does not reserve it; the eventual mutation must call
  -- checkFlow in the same transaction that consumes this unchanged row.
  unless (S._transactionPhase row == "continued" && S._transactionExpiresAt row > now) invalid
  (settings, _) <- loadProviderSettings
  unless (S._transactionProviderRevision row == S._settingsRevision settings) invalid
  identity <- loadPayload row
  pure (settings, row, identity)

-- | Require both callback handoff and browser verifier before issuing a session or
-- replacing the resolved flow with a one-use registration/link continuation.
exchange :: DTO.OidcExchangeRequest -> MyHandler DTO.OidcExchangeResponse
exchange (DTO.OidcExchangeRequest ident code verifier) = do
  unless (validToken code && validToken verifier) invalid
  row <- runDBSensitive (A.getOidcTransaction ident) >>= maybe invalid pure
  now <- clock
  unless
    ( S._transactionPhase row == "resolved"
        && S._transactionExpiresAt row > now
        -- The callback handoff alone is insufficient without the starting browser's proof.
        && maybe False (secureEqual (tokenDigest code)) (S._transactionHandoffHash row)
        && secureEqual (tokenDigest verifier) (S._transactionBrowserChallenge row)
    )
    invalid
  (settings, _) <- loadProviderSettings
  identity <- loadPayload row
  -- Resolve authority by issuer/subject, never by a matching provider email claim.
  linked <- runDBSensitive $ A.getExternalIdentity (O.verifiedIssuer identity) (O.verifiedSubject identity)
  case (S._transactionPurpose row, linked) of
    ("login", Just existing) -> DTO.OidcAuthenticated <$> issue settings row identity (S.unUserId (S._identityUser existing)) False
    ("reauth", Just existing)
      | Just (S.unUserId (S._identityUser existing)) == S.unUserId (S._transactionUser row) ->
          DTO.OidcAuthenticated <$> issue settings row identity (S.unUserId (S._identityUser existing)) False
    ("reauth", _) -> invalid
    _ -> do
      continuation <- liftIO randomToken
      let continued =
            row
              { S._transactionPhase = "continued",
                S._transactionContinuationHash = Just (tokenDigest continuation),
                S._transactionExpiresAt = min (addUTCTime 1260 (S._transactionCreatedAt row)) (addUTCTime 600 now)
              }
      response <- case S._transactionPurpose row of
        "login" -> DTO.OidcRegistrationRequired continuation <$> profileHints identity <*> pure (S._settingsSsoRegistrationEnabled settings)
        _ -> do
          uid <- maybe invalid pure (S.unUserId (S._transactionUser row))
          user <- runDBSensitive (U.getUserById uid) >>= maybe invalid pure
          ensureLinkAvailable user identity
          pure $
            DTO.OidcLinkConfirmationRequired
              continuation
              (schemaUserToApiUser user)
              (candidate settings identity)
      -- Changing phase retires the handoff even though its hash remains in the row.
      -- Only the new continuation can authorize registration or explicit link confirmation.
      runDBSensitiveTransaction $ do
        checkFlow now settings row (Just identity)
        A.saveOidcTransaction row continued
        A.cleanupExpiredAuth now
      pure response

candidate :: S.AuthSettings -> O.VerifiedOidcIdentity -> DTO.IdentityCandidate
candidate settings = identityCandidate (S._settingsProviderName settings)

-- | Invalid provider profile hints are omitted, not accepted as account attributes.
profileHints :: O.VerifiedOidcIdentity -> MyHandler DTO.OidcProfileHints
profileHints identity = do
  let p = O.verifiedProfile identity
      hint :: (Text -> MyHandler Text) -> Maybe Text -> MyHandler (Maybe Text)
      hint normalize value = traverse normalize value `catchError` (const (pure Nothing))
  username <- hint normalizeUsername (O.profileUsername p)
  name <- hint normalizeDisplayName (O.profileDisplayName p)
  email <- hint normalizeEmail (O.profileEmail p)
  pure $ DTO.OidcProfileHints username name email (isJust email && O.profileEmailVerified p)

ensureLinkAvailable :: S.User -> O.VerifiedOidcIdentity -> MyHandler ()
ensureLinkAvailable user identity = do
  (existing, own) <- runDBSensitive $ (,) <$> A.getExternalIdentity (O.verifiedIssuer identity) (O.verifiedSubject identity) <*> A.getUserIdentity (S._userId user)
  when (maybe False ((/= S.UserId (S._userId user)) . S._identityUser) existing) $ throwError $ err409 {errBody = "SSO identity is already linked"}
  when (maybe False (not . (`matchesIdentity` identity)) own) $ throwError $ err409 {errBody = "This account already has an SSO identity"}

-- | Atomically consume the flow and any invitation with identity/session updates;
-- account-bound linking also revokes the session that initiated the flow.
issue :: S.AuthSettings -> S.OidcTransaction -> O.VerifiedOidcIdentity -> UUID -> Bool -> MyHandler LoginResponse
issue settings row identity uid allowLink = do
  user <- runDBSensitive (U.getUserById uid) >>= maybe invalid pure
  unless (S._enabled user) invalid
  ensureLinkAvailable user identity
  own <- runDBSensitive (A.getUserIdentity uid)
  unless (allowLink || maybe False (`matchesIdentity` identity) own) invalid
  (session, response) <- prepareUserSession user (OidcSession identity)
  now <- clock
  -- Session/JWT preparation has no published effect. Consumption and insertion must
  -- commit together before returning it, or a retry could complete the same flow twice.
  runDBSensitiveTransaction $ do
    checkFlow now settings row (Just identity)
    checkUser now row user
    currentIdentity <- A.getUserIdentity uid
    existing <- A.getExternalIdentity (O.verifiedIssuer identity) (O.verifiedSubject identity)
    unless
      ( currentIdentity == own
          && maybe True (`matchesIdentity` identity) currentIdentity
          && maybe True ((== S.UserId uid) . S._identityUser) existing
      )
      A.abortAuthTransaction
    unless (allowLink || maybe False (`matchesIdentity` identity) currentIdentity) A.abortAuthTransaction
    -- Every authorization check precedes the first mutation.
    case own of
      Nothing -> A.insertExternalIdentity (identityRow now uid identity)
      Just previous ->
        A.saveExternalIdentity
          previous
          ( previous
              { S._identityLastLoginAt = Just now,
                S._identityClaimedEmail = O.profileEmail (O.verifiedProfile identity),
                S._identityClaimedEmailVerified = O.profileEmailVerified (O.verifiedProfile identity)
              }
          )
    -- Invitation redemption and identity attachment roll back with session creation.
    consumeBoundInvitation now row
    A.saveOidcTransaction row (consumed row)
    case S.unUserSessionId (S._transactionSession row) of
      Nothing -> pure ()
      Just sid -> do
        -- Rotate the initiating session rather than silently upgrading its old bearer token.
        starting <- A.getUserSession sid >>= maybe A.abortAuthTransaction pure
        void $ A.revokeSession starting now
    A.insertUserSession session
    A.cleanupExpiredAuth now
  pure response

identityRow :: UTCTime -> UUID -> O.VerifiedOidcIdentity -> S.ExternalIdentity
identityRow now uid identity =
  S.ExternalIdentity
    (O.verifiedIssuer identity)
    (O.verifiedSubject identity)
    (S.UserId uid)
    now
    (Just now)
    (O.profileEmail (O.verifiedProfile identity))
    (O.profileEmailVerified (O.verifiedProfile identity))

consumed :: S.OidcTransaction -> S.OidcTransaction
consumed row =
  -- Keep the correlation row until cleanup, but clear completion proofs and provider payload.
  row
    { S._transactionPhase = "consumed",
      S._transactionHandoffHash = Nothing,
      S._transactionContinuationHash = Nothing,
      S._transactionResultPayloadEncrypted = Nothing
    }

consumeBoundInvitation :: UTCTime -> S.OidcTransaction -> Pg ()
consumeBoundInvitation now row = case S.unAccountInvitationId (S._transactionInvitation row) of
  Nothing -> pure ()
  Just iid -> do
    invitation <- A.getAccountInvitation iid >>= maybe A.abortAuthTransaction pure
    A.saveAccountInvitation invitation (invitation {S._invitationConsumedAt = Just now})

-- | Create account, identity and session together only for an allowed signup flow;
-- email verification transfers only when the normalized provider email matches.
register :: DTO.OidcRegisterRequest -> MyHandler LoginResponse
register (DTO.OidcRegisterRequest token rawUsername rawName rawEmail) = do
  (settings, row, identity) <- loadContinuation token
  unless (S._transactionPurpose row == "login") invalid
  unless (S._settingsSsoRegistrationEnabled settings) $ throwError $ err403 {errBody = "SSO registration is disabled"}
  username <- normalizeUsername rawUsername
  name <- normalizeDisplayName rawName
  email <- normalizeEmail rawEmail
  providerEmail <- traverse normalizeEmail (O.profileEmail (O.verifiedProfile identity)) `catchError` (const (pure Nothing))
  uid <- liftIO UUID.nextRandom
  -- A user-edited email inherits verification only if it matches the normalized provider claim.
  let user = S.User uid username name email (providerEmail == Just email && O.profileEmailVerified (O.verifiedProfile identity)) Nothing True Nothing
  (session, response) <- prepareUserSession user (OidcSession identity)
  now <- clock
  outcome <- runDBSensitiveTransaction $ do
    checkFlow now settings row (Just identity)
    unless (S._settingsSsoRegistrationEnabled settings) A.abortAuthTransaction
    existing <- A.getExternalIdentity (O.verifiedIssuer identity) (O.verifiedSubject identity)
    usernameExists <- U.checkUserExists username
    emailExists <- U.checkEmailExists email
    if isJust existing
      then pure (Just "SSO identity is already linked")
      else
        -- Name/email conflicts leave the continuation unconsumed so signup input can be corrected.
        if usernameExists
          then pure (Just "Username is already in use")
          else
            if emailExists
              then pure (Just "Email is already in use")
              else do
                U.insertUserRow user
                A.insertExternalIdentity (identityRow now uid identity)
                A.saveOidcTransaction row (consumed row)
                A.insertUserSession session
                A.cleanupExpiredAuth now
                pure Nothing
  maybe (pure response) (\message -> throwError $ err409 {errBody = textToLBS message}) outcome

-- | Link only after local password proof; five failed attempts fail this continuation.
linkExisting :: DTO.OidcLinkExistingRequest -> MyHandler LoginResponse
linkExisting (DTO.OidcLinkExistingRequest token rawIdentifier password) = do
  (settings, row, identity) <- loadContinuation token
  unless (S._transactionPurpose row == "login") invalid
  let raw = T.strip rawIdentifier
      isEmail = T.any (== '@') (T.drop 1 raw)
  identifier <- (Just <$> (if isEmail then normalizeEmail raw else normalizeUsername raw)) `catchError` (const (pure Nothing))
  target <- case identifier of
    Nothing -> pure Nothing
    Just value -> runDBSensitive $ if isEmail then U.getUserByEmail value else U.getUserByUsername value
  case identifier of
    Nothing -> pure ()
    Just value -> enforceUserCredentialLimit (S._userId <$> target) value
  case target of
    Just user
      | S._enabled user,
        BS.length (TE.encodeUtf8 password) <= 72,
        Just hash <- S._passwordHash user,
        validatePassword (TE.encodeUtf8 hash) (TE.encodeUtf8 password) -> do
          -- Do not use issue here: its fresh user lookup would discard the account snapshot
          -- whose password hash was actually verified, allowing a concurrent reset to be missed.
          ensureLinkAvailable user identity
          issueVerifiedPassword settings row identity user
    _ -> do
      now <- clock
      runDBSensitiveTransaction $ do
        checkFlow now settings row (Just identity)
        let attempts = S._transactionPasswordAttempts row + 1
            next = row {S._transactionPasswordAttempts = attempts}
        A.saveOidcTransaction row (if attempts >= 5 then failed next else next)
      -- Throw only after commit; throwing inside would roll back the failed-attempt counter.
      throwError $ err400 {errBody = "Invalid username/email or password"}

-- | Preserve the password-verified account snapshot across session preparation and
-- compare it again inside the transaction before attaching the provider identity.
issueVerifiedPassword :: S.AuthSettings -> S.OidcTransaction -> O.VerifiedOidcIdentity -> S.User -> MyHandler LoginResponse
issueVerifiedPassword settings row identity user = do
  own <- runDBSensitive (A.getUserIdentity (S._userId user))
  (session, response) <- prepareUserSession user (OidcSession identity)
  now <- clock
  runDBSensitiveTransaction $ do
    checkFlow now settings row (Just identity)
    checkUser now row user
    current <- A.getUserIdentity (S._userId user)
    external <- A.getExternalIdentity (O.verifiedIssuer identity) (O.verifiedSubject identity)
    unless
      ( current == own
          && maybe True (`matchesIdentity` identity) current
          && maybe True ((== S.UserId (S._userId user)) . S._identityUser) external
      )
      A.abortAuthTransaction
    case own of
      Nothing -> A.insertExternalIdentity (identityRow now (S._userId user) identity)
      Just previous ->
        A.saveExternalIdentity
          previous
          ( previous
              { S._identityLastLoginAt = Just now,
                S._identityClaimedEmail = O.profileEmail (O.verifiedProfile identity),
                S._identityClaimedEmailVerified = O.profileEmailVerified (O.verifiedProfile identity)
              }
          )
    A.saveOidcTransaction row (consumed row)
    A.insertUserSession session
    A.cleanupExpiredAuth now
  pure response

confirmLink :: DTO.OidcContinuationRequest -> MyHandler LoginResponse
confirmLink (DTO.OidcContinuationRequest token) = do
  (settings, row, identity) <- loadContinuation token
  unless (S._transactionPurpose row `elem` ["link", "invitation"]) invalid
  uid <- maybe invalid pure (S.unUserId (S._transactionUser row))
  issue settings row identity uid True

cancel :: DTO.OidcContinuationRequest -> MyHandler NoContent
cancel (DTO.OidcContinuationRequest token) = do
  (settings, row, identity) <- loadContinuation token
  now <- clock
  -- Terminal phase blocks reuse even if a copy of the continuation proof still exists.
  runDBSensitiveTransaction $ do
    checkFlow now settings row (Just identity)
    A.saveOidcTransaction row (failed row)
    A.cleanupExpiredAuth now
  pure NoContent
