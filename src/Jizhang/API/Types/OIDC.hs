{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wire contracts for application-user authentication and operator management.
-- Import this module qualified: several contracts intentionally share wire fields.
-- Secret-bearing DTOs deliberately have no Show instance.
-- JSON keys keep their field spelling. Every key is required, including nullable
-- keys, and unknown keys are rejected; codecs do not enforce authentication policy.
-- Issuer/subject and claimed email are private identity data, unlike User.User.
-- UUIDs and UTC timestamps use Aeson's string encodings; Text fields are not
-- normalized by decoding, including URLs, profile text and sensitive credentials.
module Jizhang.API.Types.OIDC
  ( AccountOidcPurpose (..),
    SessionMethod (..),
    InvitationPurpose (..),
    OidcError (..),
    AdminCreateMode (..),
    AuthOptions (..),
    OidcStartRequest (..),
    OidcStartResponse (..),
    OidcExchangeRequest (..),
    OidcProfileHints (..),
    IdentityCandidate (..),
    IdentitySummary (..),
    OidcRegisterRequest (..),
    OidcLinkExistingRequest (..),
    OidcContinuationRequest (..),
    InvitationInspectRequest (..),
    InvitationInspection (..),
    PasswordResetRequest (..),
    AccountAuthentication (..),
    ReauthenticateRequest (..),
    AccountOidcStartRequest (..),
    PasswordChangeRequest (..),
    IdentityUnlinkRequest (..),
    ProviderLogoutResponse (..),
    AuthenticationSettings (..),
    AuthenticationSettingsRequest (..),
    AuthenticationTestResult (..),
    InvitationIssued (..),
    PendingInvitation (..),
    AdminUserSummary (..),
    AdminUserDetails (..),
    AdminUpdateUserRequest (..),
    OidcExchangeResponse (..),
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Data (Typeable)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.User as User

-- | Reject keys outside the allowed set. Each caller's explicit (.:) lookups
-- require its keys, including nullable keys that generic decoding could omit.
strictObject :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
strictObject label keys parse = withObject label $ \o ->
  if any (`notElem` keys) (KM.keys o)
    then fail ("Unknown field in " <> label)
    else parse o

-- | Swagger description of the exact string literals accepted by an enum codec.
enumSchema :: Text -> [Text] -> Swagger.NamedSchema
enumSchema label values =
  Swagger.NamedSchema (Just label) $
    mempty
      { Swagger._schemaParamSchema =
          mempty
            { Swagger._paramSchemaType = Just Swagger.SwaggerString,
              Swagger._paramSchemaEnum = Just (map String values)
            }
      }

-- | Account flow intent encoded as "link" or "reauth"; no login intent is allowed.
data AccountOidcPurpose = LinkIdentity | ReauthenticateIdentity
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON AccountOidcPurpose where
  toJSON LinkIdentity = String "link"
  toJSON ReauthenticateIdentity = String "reauth"

instance FromJSON AccountOidcPurpose where
  parseJSON = withText "AccountOidcPurpose" $ \value -> case value of
    "link" -> pure LinkIdentity
    "reauth" -> pure ReauthenticateIdentity
    _ -> fail "Invalid AccountOidcPurpose"

instance ToSchema AccountOidcPurpose where
  declareNamedSchema _ = pure $ enumSchema "AccountOidcPurpose" ["link", "reauth"]

-- | Authentication method recorded for a session: "password" or "oidc".
data SessionMethod = PasswordMethod | OidcMethod
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON SessionMethod where
  toJSON PasswordMethod = String "password"
  toJSON OidcMethod = String "oidc"

instance FromJSON SessionMethod where
  parseJSON = withText "SessionMethod" $ \value -> case value of
    "password" -> pure PasswordMethod
    "oidc" -> pure OidcMethod
    _ -> fail "Invalid SessionMethod"

instance ToSchema SessionMethod where
  declareNamedSchema _ = pure $ enumSchema "SessionMethod" ["password", "oidc"]

-- | Invitation capability encoded as "password_reset" or "oidc_link".
data InvitationPurpose = PasswordResetInvitation | OidcLinkInvitation
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON InvitationPurpose where
  toJSON PasswordResetInvitation = String "password_reset"
  toJSON OidcLinkInvitation = String "oidc_link"

instance FromJSON InvitationPurpose where
  parseJSON = withText "InvitationPurpose" $ \value -> case value of
    "password_reset" -> pure PasswordResetInvitation
    "oidc_link" -> pure OidcLinkInvitation
    _ -> fail "Invalid InvitationPurpose"

instance ToSchema InvitationPurpose where
  declareNamedSchema _ = pure $ enumSchema "InvitationPurpose" ["password_reset", "oidc_link"]

-- | Public protocol-failure category, not raw provider error text. Wire values
-- are access_denied, expired, invalid_response, provider_unavailable and configuration_changed.
data OidcError = AccessDenied | Expired | InvalidResponse | ProviderUnavailable | ConfigurationChanged
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON OidcError where
  toJSON AccessDenied = String "access_denied"
  toJSON Expired = String "expired"
  toJSON InvalidResponse = String "invalid_response"
  toJSON ProviderUnavailable = String "provider_unavailable"
  toJSON ConfigurationChanged = String "configuration_changed"

instance FromJSON OidcError where
  parseJSON = withText "OidcError" $ \value -> case value of
    "access_denied" -> pure AccessDenied
    "expired" -> pure Expired
    "invalid_response" -> pure InvalidResponse
    "provider_unavailable" -> pure ProviderUnavailable
    "configuration_changed" -> pure ConfigurationChanged
    _ -> fail "Invalid OidcError"

instance ToSchema OidcError where
  declareNamedSchema _ = pure $ enumSchema "OidcError" ["access_denied", "expired", "invalid_response", "provider_unavailable", "configuration_changed"]

-- | Operator account-creation choice encoded as "local" or "sso".
data AdminCreateMode = LocalAccount | SsoAccount
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON AdminCreateMode where
  toJSON LocalAccount = String "local"
  toJSON SsoAccount = String "sso"

instance FromJSON AdminCreateMode where
  parseJSON = withText "AdminCreateMode" $ \value -> case value of
    "local" -> pure LocalAccount
    "sso" -> pure SsoAccount
    _ -> fail "Invalid AdminCreateMode"

instance ToSchema AdminCreateMode where
  declareNamedSchema _ = pure $ enumSchema "AdminCreateMode" ["local", "sso"]

-- | Public authentication choices and provider label, without provider secrets,
-- issuer/subject identifiers or private account information.
data AuthOptions = AuthOptions
  { -- | Whether local registration is advertised as enabled.
    localRegistrationEnabled :: !(Bool),
    -- | Whether provider-backed registration is advertised as enabled.
    ssoRegistrationEnabled :: !(Bool),
    -- | Whether SSO is advertised as available.
    ssoEnabled :: !(Bool),
    -- | Human-readable provider label, not its protocol issuer identifier.
    providerName :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AuthOptions where
  parseJSON = strictObject "AuthOptions" ["localRegistrationEnabled", "ssoRegistrationEnabled", "ssoEnabled", "providerName"] $ \o ->
    AuthOptions
      <$> o .: "localRegistrationEnabled"
      <*> o .: "ssoRegistrationEnabled"
      <*> o .: "ssoEnabled"
      <*> o .: "providerName"

-- | Begin a browser-bound login flow. invitationToken is a required nullable
-- key: send null for ordinary login, not an omitted field.
data OidcStartRequest = OidcStartRequest
  { -- | Browser verifier digest; the handler validates its token spelling.
    browserChallenge :: !(Text),
    -- | Sensitive invitation capability when the flow is invitation-backed.
    invitationToken :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcStartRequest where
  parseJSON = strictObject "OidcStartRequest" ["browserChallenge", "invitationToken"] $ \o ->
    OidcStartRequest
      <$> o .: "browserChallenge"
      <*> o .: "invitationToken"

-- | Redirect destination and flow UUID for a newly started browser flow. Parsing
-- does not check that the URL is trusted or that the flow is live.
data OidcStartResponse = OidcStartResponse
  { flowId :: !(UUID),
    -- | Provider authorization redirect, not an application access token.
    authorizationUrl :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcStartResponse where
  parseJSON = strictObject "OidcStartResponse" ["flowId", "authorizationUrl"] $ \o ->
    OidcStartResponse
      <$> o .: "flowId"
      <*> o .: "authorizationUrl"

-- | Exchange an application's callback handoff with browser proof. These are
-- required strings; liveness, proof binding and one-use checks are handler duties.
data OidcExchangeRequest = OidcExchangeRequest
  { -- | UUID returned when the browser flow started.
    flowId :: !(UUID),
    -- | Application handoff code, not the provider authorization code.
    code :: !(Text),
    -- | Sensitive browser secret corresponding to the start request's challenge.
    browserVerifier :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcExchangeRequest where
  parseJSON = strictObject "OidcExchangeRequest" ["flowId", "code", "browserVerifier"] $ \o ->
    OidcExchangeRequest
      <$> o .: "flowId"
      <*> o .: "code"
      <*> o .: "browserVerifier"

-- | Nullable provider-supplied suggestions for a registration form. emailVerified
-- describes the provider claim; it does not make the email an account-linking key.
data OidcProfileHints = OidcProfileHints
  { -- | Suggested public handle, or explicit null if unavailable.
    username :: !(Maybe Text),
    -- | Suggested public name, or explicit null if unavailable.
    displayName :: !(Maybe Text),
    -- | Suggested contact address, not proof of an existing local account.
    email :: !(Maybe Text),
    emailVerified :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcProfileHints where
  parseJSON = strictObject "OidcProfileHints" ["username", "displayName", "email", "emailVerified"] $ \o ->
    OidcProfileHints
      <$> o .: "username"
      <*> o .: "displayName"
      <*> o .: "email"
      <*> o .: "emailVerified"

-- | Private provider identity presented for explicit linking. issuer and subject
-- identify the external account; claimedEmail must not replace that identity pair.
data IdentityCandidate = IdentityCandidate
  { -- | Provider issuer identifier, paired with subject for identity matching.
    issuer :: !(Text),
    -- | Provider-local subject; not a public application username.
    subject :: !(Text),
    -- | Display label for the provider, not an identity key.
    providerName :: !(Text),
    -- | Nullable provider claim, distinct from the local profile email.
    claimedEmail :: !(Maybe Text),
    -- | Provider's verification claim, not local email-verification state.
    claimedEmailVerified :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON IdentityCandidate where
  parseJSON = strictObject "IdentityCandidate" ["issuer", "subject", "providerName", "claimedEmail", "claimedEmailVerified"] $ \o ->
    IdentityCandidate
      <$> o .: "issuer"
      <*> o .: "subject"
      <*> o .: "providerName"
      <*> o .: "claimedEmail"
      <*> o .: "claimedEmailVerified"

-- | Private linked-identity metadata for account/operator views, never the public
-- ledger User projection. Nullable claimedEmail and lastLoginAt remain required keys.
data IdentitySummary = IdentitySummary
  { -- | Issuer half of the linked external identity's stable key.
    issuer :: !(Text),
    -- | Subject half of that key, not an application username.
    subject :: !(Text),
    -- | Human-readable provider label, not part of the identity key.
    providerName :: !(Text),
    -- | Provider-claimed address, distinct from private local profile email.
    claimedEmail :: !(Maybe Text),
    -- | Provider verification claim, not the local profile's emailVerified flag.
    claimedEmailVerified :: !(Bool),
    -- | Time this external identity was attached to the application account.
    linkedAt :: !(UTCTime),
    -- | Last recorded identity login, or null if none is recorded.
    lastLoginAt :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON IdentitySummary where
  parseJSON = strictObject "IdentitySummary" ["issuer", "subject", "providerName", "claimedEmail", "claimedEmailVerified", "linkedAt", "lastLoginAt"] $ \o ->
    IdentitySummary
      <$> o .: "issuer"
      <*> o .: "subject"
      <*> o .: "providerName"
      <*> o .: "claimedEmail"
      <*> o .: "claimedEmailVerified"
      <*> o .: "linkedAt"
      <*> o .: "lastLoginAt"

-- | Complete provider-backed registration using a sensitive continuation token
-- and chosen local profile strings. The codec does not validate profile policy.
data OidcRegisterRequest = OidcRegisterRequest
  { -- | Sensitive capability for the pending registration decision.
    continuationToken :: !(Text),
    -- | Chosen public handle; it need not reuse the provider's suggestion.
    username :: !(Text),
    -- | Chosen public name shown in group and ledger responses.
    displayName :: !(Text),
    -- | Chosen private local email, not an external identity lookup key.
    email :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcRegisterRequest where
  parseJSON = strictObject "OidcRegisterRequest" ["continuationToken", "username", "displayName", "email"] $ \o ->
    OidcRegisterRequest
      <$> o .: "continuationToken"
      <*> o .: "username"
      <*> o .: "displayName"
      <*> o .: "email"

-- | Prove an existing local account with its password while continuing an OIDC
-- flow. Neither matching email claims nor successful decoding authenticates it.
data OidcLinkExistingRequest = OidcLinkExistingRequest
  { -- | Sensitive capability for the unfinished provider-authentication decision.
    continuationToken :: !(Text),
    -- | Local account identifier, not the provider's subject identifier.
    loginIdentifier :: !(Text),
    -- | Plaintext local credential, never a provider password.
    loginPassword :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcLinkExistingRequest where
  parseJSON = strictObject "OidcLinkExistingRequest" ["continuationToken", "loginIdentifier", "loginPassword"] $ \o ->
    OidcLinkExistingRequest
      <$> o .: "continuationToken"
      <*> o .: "loginIdentifier"
      <*> o .: "loginPassword"

-- | Continue a pending OIDC decision with its sensitive continuationToken string;
-- this object carries neither a login session nor additional identity proof.
data OidcContinuationRequest = OidcContinuationRequest
  { continuationToken :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON OidcContinuationRequest where
  parseJSON = strictObject "OidcContinuationRequest" ["continuationToken"] $ \o ->
    OidcContinuationRequest <$> o .: "continuationToken"

-- | Inspect a sensitive invitation token without including new credentials.
-- Decoding checks object shape only, not token purpose, expiry or consumption.
data InvitationInspectRequest = InvitationInspectRequest
  { token :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON InvitationInspectRequest where
  parseJSON = strictObject "InvitationInspectRequest" ["token"] $ \o ->
    InvitationInspectRequest <$> o .: "token"

-- | Valid invitation's purpose, public target account and expiry. The inspection
-- response does not echo the raw invitation token or expose provider identity data.
data InvitationInspection = InvitationInspection
  { purpose :: !(InvitationPurpose),
    -- | Public target identity, not a private profile or provider identity.
    target :: !(User.User),
    expiresAt :: !(UTCTime)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON InvitationInspection where
  parseJSON = strictObject "InvitationInspection" ["purpose", "target", "expiresAt"] $ \o ->
    InvitationInspection
      <$> o .: "purpose"
      <*> o .: "target"
      <*> o .: "expiresAt"

-- | Sensitive reset capability and replacement plaintext password. Required
-- strings alone do not establish token validity or satisfy password policy.
data PasswordResetRequest = PasswordResetRequest
  { token :: !(Text),
    newPassword :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON PasswordResetRequest where
  parseJSON = strictObject "PasswordResetRequest" ["token", "newPassword"] $ \o ->
    PasswordResetRequest
      <$> o .: "token"
      <*> o .: "newPassword"

-- | Private authentication state of the current account/session. Missing identity
-- or OIDC reauthentication timestamps use explicit null, never omitted keys.
data AccountAuthentication = AccountAuthentication
  { -- | Whether a local credential exists; no password hash is returned.
    hasPassword :: !(Bool),
    -- | Linked provider identity metadata, or null when no identity is attached.
    identity :: !(Maybe IdentitySummary),
    -- | Method by which this session was authenticated.
    sessionMethod :: !(SessionMethod),
    -- | Session authentication time, refreshed by password reauthentication.
    authenticatedAt :: !(UTCTime),
    -- | Latest OIDC reauthentication proof time, or null if not recorded.
    oidcReauthenticatedAt :: !(Maybe UTCTime),
    -- | Whether the handler considers the configured provider enabled and viable.
    providerEnabled :: !(Bool),
    -- | Whether this session currently supports provider sign-out.
    canSignOutProvider :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AccountAuthentication where
  parseJSON = strictObject "AccountAuthentication" ["hasPassword", "identity", "sessionMethod", "authenticatedAt", "oidcReauthenticatedAt", "providerEnabled", "canSignOutProvider"] $ \o ->
    AccountAuthentication
      <$> o .: "hasPassword"
      <*> o .: "identity"
      <*> o .: "sessionMethod"
      <*> o .: "authenticatedAt"
      <*> o .: "oidcReauthenticatedAt"
      <*> o .: "providerEnabled"
      <*> o .: "canSignOutProvider"

-- | Plaintext local password proof for refreshing account authentication state.
-- The decoder checks a required string, not whether it matches the account.
data ReauthenticateRequest = ReauthenticateRequest
  { password :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ReauthenticateRequest where
  parseJSON = strictObject "ReauthenticateRequest" ["password"] $ \o ->
    ReauthenticateRequest <$> o .: "password"

-- | Start account linking or provider reauthentication using a browser challenge.
-- Purpose and proof are checked by handlers after decoding this closed object.
data AccountOidcStartRequest = AccountOidcStartRequest
  { purpose :: !(AccountOidcPurpose),
    browserChallenge :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AccountOidcStartRequest where
  parseJSON = strictObject "AccountOidcStartRequest" ["purpose", "browserChallenge"] $ \o ->
    AccountOidcStartRequest
      <$> o .: "purpose"
      <*> o .: "browserChallenge"

-- | Set or replace a local password. currentPassword is a required nullable key;
-- existing passwords need it, while first-password authorization uses OIDC proof.
data PasswordChangeRequest = PasswordChangeRequest
  { -- | Plaintext old credential, or explicit null when setting the first password.
    currentPassword :: !(Maybe Text),
    -- | Sensitive replacement credential; the decoder does not check its strength.
    newPassword :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON PasswordChangeRequest where
  parseJSON = strictObject "PasswordChangeRequest" ["currentPassword", "newPassword"] $ \o ->
    PasswordChangeRequest
      <$> o .: "currentPassword"
      <*> o .: "newPassword"

-- | Required plaintext local password proof for unlinking an external identity.
-- The codec does not check that another viable login method remains.
data IdentityUnlinkRequest = IdentityUnlinkRequest
  { currentPassword :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON IdentityUnlinkRequest where
  parseJSON = strictObject "IdentityUnlinkRequest" ["currentPassword"] $ \o ->
    IdentityUnlinkRequest <$> o .: "currentPassword"

-- | Provider sign-out redirect. It is not a bearer token and decoding does not
-- validate its destination or terminate either application or provider sessions.
data ProviderLogoutResponse = ProviderLogoutResponse
  { logoutUrl :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ProviderLogoutResponse where
  parseJSON = strictObject "ProviderLogoutResponse" ["logoutUrl"] $ \o ->
    ProviderLogoutResponse <$> o .: "logoutUrl"

-- | Operator settings projection: reports secret presence without returning the
-- secret. Nullable provider configuration, derived URLs and runtimeError use null.
data AuthenticationSettings = AuthenticationSettings
  { -- | Saved revision used to detect stale settings updates.
    revision :: !(Int64),
    -- | Whether environment configuration owns settings rather than operator edits.
    environmentManaged :: !(Bool),
    -- | Whether the saved settings permit local account registration.
    localRegistrationEnabled :: !(Bool),
    -- | Whether the saved settings permit provider-backed account registration.
    ssoRegistrationEnabled :: !(Bool),
    -- | Saved provider enablement flag, separate from runtimeReady.
    oidcEnabled :: !(Bool),
    -- | Human-readable provider label; not the issuer identity key.
    providerName :: !(Text),
    -- | Configured protocol issuer, or null if absent.
    issuer :: !(Maybe Text),
    -- | Configured client identifier, or null if absent.
    clientId :: !(Maybe Text),
    -- | Presence indicator only; plaintext or encrypted secret bytes are not exposed.
    hasClientSecret :: !(Bool),
    -- | Derived callback URL, or null when unavailable.
    callbackUrl :: !(Maybe Text),
    -- | This application's derived backchannel-logout URL, or null when unavailable.
    backchannelLogoutUrl :: !(Maybe Text),
    -- | Derived post-logout redirect, or null when unavailable.
    postLogoutRedirectUrl :: !(Maybe Text),
    -- | Local runtime-readiness result, not a promise of provider availability.
    runtimeReady :: !(Bool),
    -- | Nullable readiness explanation, not a raw client-secret field.
    runtimeError :: !(Maybe Text),
    -- | Count of accounts with no local password and an external identity.
    ssoOnlyUserCount :: !(Int)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AuthenticationSettings where
  parseJSON = strictObject "AuthenticationSettings" ["revision", "environmentManaged", "localRegistrationEnabled", "ssoRegistrationEnabled", "oidcEnabled", "providerName", "issuer", "clientId", "hasClientSecret", "callbackUrl", "backchannelLogoutUrl", "postLogoutRedirectUrl", "runtimeReady", "runtimeError", "ssoOnlyUserCount"] $ \o ->
    AuthenticationSettings
      <$> o .: "revision"
      <*> o .: "environmentManaged"
      <*> o .: "localRegistrationEnabled"
      <*> o .: "ssoRegistrationEnabled"
      <*> o .: "oidcEnabled"
      <*> o .: "providerName"
      <*> o .: "issuer"
      <*> o .: "clientId"
      <*> o .: "hasClientSecret"
      <*> o .: "callbackUrl"
      <*> o .: "backchannelLogoutUrl"
      <*> o .: "postLogoutRedirectUrl"
      <*> o .: "runtimeReady"
      <*> o .: "runtimeError"
      <*> o .: "ssoOnlyUserCount"

-- | Settings draft with an optimistic revision. Every key is required; issuer,
-- clientId and clientSecret may be null. Semantic configuration checks are external.
data AuthenticationSettingsRequest = AuthenticationSettingsRequest
  { -- | Revision the caller edited; the handler rejects a stale snapshot.
    revision :: !(Int64),
    -- | Requested local-registration policy.
    localRegistrationEnabled :: !(Bool),
    -- | Requested provider-backed registration policy.
    ssoRegistrationEnabled :: !(Bool),
    -- | Requested provider enablement, not a provider readiness result.
    oidcEnabled :: !(Bool),
    -- | Human-readable label for the configured provider.
    providerName :: !(Text),
    -- | Replacement issuer configuration; null clears this field.
    issuer :: !(Maybe Text),
    -- | Replacement client identifier; null clears this field.
    clientId :: !(Maybe Text),
    -- | Sensitive replacement secret; null preserves the stored secret in updates.
    clientSecret :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AuthenticationSettingsRequest where
  parseJSON = strictObject "AuthenticationSettingsRequest" ["revision", "localRegistrationEnabled", "ssoRegistrationEnabled", "oidcEnabled", "providerName", "issuer", "clientId", "clientSecret"] $ \o ->
    AuthenticationSettingsRequest
      <$> o .: "revision"
      <*> o .: "localRegistrationEnabled"
      <*> o .: "ssoRegistrationEnabled"
      <*> o .: "oidcEnabled"
      <*> o .: "providerName"
      <*> o .: "issuer"
      <*> o .: "clientId"
      <*> o .: "clientSecret"

-- | Provider discovery test output, without tokens or secrets. Nullable optional
-- endpoints use explicit null; the codec itself does not discover or verify them.
data AuthenticationTestResult = AuthenticationTestResult
  { -- | Discovered provider issuer identifier.
    issuer :: !(Text),
    -- | Browser authorization endpoint.
    authorizationEndpoint :: !(Text),
    -- | Server-side code/token exchange endpoint.
    tokenEndpoint :: !(Text),
    -- | Provider signing-key set location.
    jwksUri :: !(Text),
    -- | Optional user-info endpoint from discovery metadata.
    userinfoEndpoint :: !(Maybe Text),
    -- | Optional provider sign-out endpoint from discovery metadata.
    endSessionEndpoint :: !(Maybe Text),
    -- | Whether required PKCE support was reported by discovery.
    pkceSupported :: !(Bool),
    -- | Whether provider backchannel logout support was reported.
    backchannelLogoutSupported :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AuthenticationTestResult where
  parseJSON = strictObject "AuthenticationTestResult" ["issuer", "authorizationEndpoint", "tokenEndpoint", "jwksUri", "userinfoEndpoint", "endSessionEndpoint", "pkceSupported", "backchannelLogoutSupported"] $ \o ->
    AuthenticationTestResult
      <$> o .: "issuer"
      <*> o .: "authorizationEndpoint"
      <*> o .: "tokenEndpoint"
      <*> o .: "jwksUri"
      <*> o .: "userinfoEndpoint"
      <*> o .: "endSessionEndpoint"
      <*> o .: "pkceSupported"
      <*> o .: "backchannelLogoutSupported"

-- | Newly issued invitation and its sensitive capability URL. Unlike a pending
-- invitation listing, this response exposes the link needed to redeem it.
data InvitationIssued = InvitationIssued
  { -- | Management UUID, distinct from the secret token embedded in the URL.
    invitationId :: !(UUID),
    -- | Sensitive redemption URL; do not treat it as public account metadata.
    url :: !(Text),
    -- | Server expiry timestamp for the invitation capability.
    expiresAt :: !(UTCTime)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON InvitationIssued where
  parseJSON = strictObject "InvitationIssued" ["invitationId", "url", "expiresAt"] $ \o ->
    InvitationIssued
      <$> o .: "invitationId"
      <*> o .: "url"
      <*> o .: "expiresAt"

-- | Invitation management metadata without its raw token or redemption URL.
-- The UUID identifies an invitation for management, not a redemption credential.
data PendingInvitation = PendingInvitation
  { invitationId :: !(UUID),
    purpose :: !(InvitationPurpose),
    expiresAt :: !(UTCTime)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON PendingInvitation where
  parseJSON = strictObject "PendingInvitation" ["invitationId", "purpose", "expiresAt"] $ \o ->
    PendingInvitation
      <$> o .: "invitationId"
      <*> o .: "purpose"
      <*> o .: "expiresAt"

-- | Operator-only account summary containing private email via UserProfile,
-- plus capability flags without password hashes or issuer/subject details.
data AdminUserSummary = AdminUserSummary
  { -- | Public user plus private email and local verification state.
    profile :: !(User.UserProfile),
    -- | Account enabled state, distinct from available authentication methods.
    enabled :: !(Bool),
    -- | Presence flag only, not password or hash material.
    hasPassword :: !(Bool),
    -- | Presence flag only, without exposing issuer and subject.
    hasIdentity :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminUserSummary where
  parseJSON = strictObject "AdminUserSummary" ["profile", "enabled", "hasPassword", "hasIdentity"] $ \o ->
    AdminUserSummary
      <$> o .: "profile"
      <*> o .: "enabled"
      <*> o .: "hasPassword"
      <*> o .: "hasIdentity"

-- | Operator account detail including private linked identity and pending
-- invitation metadata. A missing identity is explicit null, not an omitted key.
data AdminUserDetails = AdminUserDetails
  { profile :: !(User.UserProfile),
    enabled :: !(Bool),
    -- | Credential presence only; no password hash is exposed.
    hasPassword :: !(Bool),
    -- | Private identity metadata, or explicit null when no identity is linked.
    identity :: !(Maybe IdentitySummary),
    -- | Pending invitation metadata without sensitive redemption URLs.
    invitations :: !([PendingInvitation])
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminUserDetails where
  parseJSON = strictObject "AdminUserDetails" ["profile", "enabled", "hasPassword", "identity", "invitations"] $ \o ->
    AdminUserDetails
      <$> o .: "profile"
      <*> o .: "enabled"
      <*> o .: "hasPassword"
      <*> o .: "identity"
      <*> o .: "invitations"

-- | Replace public names, private email and account enabled state. Every key is
-- required; successful decoding does not establish profile policy or authorization.
data AdminUpdateUserRequest = AdminUpdateUserRequest
  { username :: !(Text),
    displayName :: !(Text),
    -- | Private local email; editing it does not change a provider identity key.
    email :: !(Text),
    -- | Requested account enabled state, not a password/identity availability flag.
    enabled :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminUpdateUserRequest where
  parseJSON = strictObject "AdminUpdateUserRequest" ["username", "displayName", "email", "enabled"] $ \o ->
    AdminUpdateUserRequest
      <$> o .: "username"
      <*> o .: "displayName"
      <*> o .: "email"
      <*> o .: "enabled"

-- | Flat status-discriminated response: authenticated carries only session;
-- registration_required carries continuationToken/profile/canRegister, while
-- link_confirmation_required carries continuationToken/target/identity.
-- Branch codecs reject every other key, so unfinished decisions carry no session.
data OidcExchangeResponse
  = OidcAuthenticated {session :: !Auth.LoginResponse}
    -- A completed login includes its sensitive bearer token and public user.
  | OidcRegistrationRequired
      { -- | Sensitive capability for a pending registration decision, not a session.
        continuationToken :: !Text,
        -- | Provider suggestions; these are not an existing local account profile.
        profile :: !OidcProfileHints,
        -- | Whether registration is permitted, even though a decision is pending.
        canRegister :: !Bool
      }
  | OidcLinkConfirmationRequired
      { continuationToken :: !Text,
        target :: !User.User,
        -- | Private external identity to be attached after explicit confirmation.
        identity :: !IdentityCandidate
      }
  deriving stock (Eq, Ord, Generic, Typeable)

instance ToJSON OidcExchangeResponse where
  toJSON (OidcAuthenticated loginSession) =
    object
      ["status" .= ("authenticated" :: Text), "session" .= loginSession]
  toJSON (OidcRegistrationRequired continuation hints allowed) =
    object
      [ "status" .= ("registration_required" :: Text),
        "continuationToken" .= continuation,
        "profile" .= hints,
        "canRegister" .= allowed
      ]
  toJSON (OidcLinkConfirmationRequired continuation account candidate) =
    object
      [ "status" .= ("link_confirmation_required" :: Text),
        "continuationToken" .= continuation,
        "target" .= account,
        "identity" .= candidate
      ]

instance FromJSON OidcExchangeResponse where
  parseJSON value =
    withObject
      "OidcExchangeResponse"
      ( \o -> do
          status <- o .: "status" :: Parser Text
          case status of
            "authenticated" ->
              strictObject
                "OidcAuthenticated"
                ["status", "session"]
                (\fields -> OidcAuthenticated <$> fields .: "session")
                value
            "registration_required" ->
              strictObject
                "OidcRegistrationRequired"
                ["status", "continuationToken", "profile", "canRegister"]
                ( \fields ->
                    OidcRegistrationRequired
                      <$> fields .: "continuationToken"
                      <*> fields .: "profile"
                      <*> fields .: "canRegister"
                )
                value
            "link_confirmation_required" ->
              strictObject
                "OidcLinkConfirmationRequired"
                ["status", "continuationToken", "target", "identity"]
                ( \fields ->
                    OidcLinkConfirmationRequired
                      <$> fields .: "continuationToken"
                      <*> fields .: "target"
                      <*> fields .: "identity"
                )
                value
            _ -> fail "Invalid OIDC exchange status"
      )
      value

instance ToSchema OidcExchangeResponse where
  declareNamedSchema _ = do
    sessionSchema <- Swagger.declareSchemaRef (Proxy :: Proxy Auth.LoginResponse)
    hintsSchema <- Swagger.declareSchemaRef (Proxy :: Proxy OidcProfileHints)
    userSchema <- Swagger.declareSchemaRef (Proxy :: Proxy User.User)
    identitySchema <- Swagger.declareSchemaRef (Proxy :: Proxy IdentityCandidate)
    textSchema <- Swagger.declareSchemaRef (Proxy :: Proxy Text)
    boolSchema <- Swagger.declareSchemaRef (Proxy :: Proxy Bool)
    let Swagger.NamedSchema _ statusSchema =
          enumSchema
            "OidcExchangeStatus"
            ["authenticated", "registration_required", "link_confirmation_required"]
    pure $
      Swagger.NamedSchema (Just "OidcExchangeResponse") $
        mempty
          { Swagger._schemaParamSchema = mempty {Swagger._paramSchemaType = Just Swagger.SwaggerObject},
            Swagger._schemaRequired = ["status"],
            Swagger._schemaDescription =
              Just
                "Discriminated by status. authenticated requires only session; registration_required requires continuationToken, profile and canRegister; link_confirmation_required requires continuationToken, target and identity. No other keys are accepted.",
            Swagger._schemaProperties =
              [ ("status", Swagger.Inline statusSchema),
                ("session", sessionSchema),
                ("continuationToken", textSchema),
                ("profile", hintsSchema),
                ("canRegister", boolSchema),
                ("target", userSchema),
                ("identity", identitySchema)
              ]
          }
