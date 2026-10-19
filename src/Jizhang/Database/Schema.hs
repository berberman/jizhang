{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Beam row shapes and SQL-name mappings, not schema creation or validation.
-- Identity specializes rows to values; Nullable foreign keys represent absent links.
module Jizhang.Database.Schema where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Beam

type Username = Text

-- | Account credentials and an invalidation fence shared by all login methods.
-- A missing password hash permits an account with no local-password credential.
data UserT f = User
  { _userId :: C f UUID,
    _username :: C f Username,
    _displayName :: C f Text,
    _email :: C f Text,
    _emailVerified :: C f Bool,
    _passwordHash :: C f (Maybe Text),
    _enabled :: C f Bool,
    _authInvalidatedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Eq User

deriving instance Ord User

instance Table UserT where
  data PrimaryKey UserT f = UserId {unUserId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = UserId . _userId

type UserKey = PrimaryKey UserT Identity

deriving instance Eq UserKey

deriving instance Show UserKey

deriving instance Ord UserKey

-- | Singleton provider policy (ID 1 by persistence convention). Revision binds
-- in-flight grants to the configuration under which callers issued them.
data AuthSettingsT f = AuthSettings
  { _settingsId :: C f Int16,
    _settingsRevision :: C f Int64,
    _settingsLocalRegistrationEnabled :: C f Bool,
    _settingsSsoRegistrationEnabled :: C f Bool,
    _settingsOidcEnabled :: C f Bool,
    _settingsProviderName :: C f Text,
    _settingsIssuer :: C f (Maybe Text),
    _settingsClientId :: C f (Maybe Text),
    _settingsClientSecretEncrypted :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

type AuthSettings = AuthSettingsT Identity

deriving instance Eq AuthSettings

instance Table AuthSettingsT where
  data PrimaryKey AuthSettingsT f = AuthSettingsId {unAuthSettingsId :: C f Int16} deriving (Generic, Beamable)
  primaryKey = AuthSettingsId . _settingsId

-- | Persisted session evidence. Presence alone does not imply validity: callers
-- check expiry, revocation, account state and authentication fences.
data UserSessionT f = UserSession
  { _sessionId :: C f UUID,
    _sessionUser :: PrimaryKey UserT f,
    _sessionMethod :: C f Text,
    _sessionCreatedAt :: C f UTCTime,
    _sessionAuthenticatedAt :: C f UTCTime,
    _sessionExpiresAt :: C f UTCTime,
    _sessionOidcReauthenticatedAt :: C f (Maybe UTCTime),
    _sessionRevokedAt :: C f (Maybe UTCTime),
    _sessionOidcIssuer :: C f (Maybe Text),
    _sessionOidcSubject :: C f (Maybe Text),
    _sessionOidcSid :: C f (Maybe Text),
    _sessionIdTokenEncrypted :: C f (Maybe Text)
  }
  deriving (Generic, Beamable)

type UserSession = UserSessionT Identity

deriving instance Eq UserSession

instance Table UserSessionT where
  data PrimaryKey UserSessionT f = UserSessionId {unUserSessionId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = UserSessionId . _sessionId

-- | Single-use account grant linked to its issuing operator. Only the token hash
-- is stored here; purpose, expiry and consumption are checked by auth operations.
data AccountInvitationT f = AccountInvitation
  { _invitationId :: C f UUID,
    _invitationTokenHash :: C f ByteString,
    _invitationPurpose :: C f Text,
    _invitationUser :: PrimaryKey UserT f,
    _invitationAdmin :: PrimaryKey AdminT f,
    _invitationProviderRevision :: C f (Maybe Int64),
    _invitationCreatedAt :: C f UTCTime,
    _invitationExpiresAt :: C f UTCTime,
    _invitationConsumedAt :: C f (Maybe UTCTime),
    _invitationRevokedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type AccountInvitation = AccountInvitationT Identity

deriving instance Eq AccountInvitation

instance Table AccountInvitationT where
  data PrimaryKey AccountInvitationT f = AccountInvitationId {unAccountInvitationId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = AccountInvitationId . _invitationId

-- | Durable OIDC flow state across browser redirects and provider requests.
-- Hashing/encryption and legal phase transitions are caller responsibilities.
data OidcTransactionT f = OidcTransaction
  { _transactionId :: C f UUID,
    _transactionStateHash :: C f ByteString,
    _transactionProviderRevision :: C f Int64,
    _transactionPurpose :: C f Text,
    _transactionUser :: PrimaryKey UserT (Nullable f),
    _transactionSession :: PrimaryKey UserSessionT (Nullable f),
    _transactionInvitation :: PrimaryKey AccountInvitationT (Nullable f),
    _transactionBrowserChallenge :: C f ByteString,
    _transactionNonceHash :: C f ByteString,
    _transactionPkceVerifierEncrypted :: C f Text,
    _transactionCreatedAt :: C f UTCTime,
    _transactionExpiresAt :: C f UTCTime,
    _transactionPhase :: C f Text,
    _transactionHandoffHash :: C f (Maybe ByteString),
    _transactionContinuationHash :: C f (Maybe ByteString),
    _transactionResultPayloadEncrypted :: C f (Maybe Text),
    _transactionPasswordAttempts :: C f Int32
  }
  deriving (Generic, Beamable)

type OidcTransaction = OidcTransactionT Identity

deriving instance Eq OidcTransaction

instance Table OidcTransactionT where
  data PrimaryKey OidcTransactionT f = OidcTransactionId {unOidcTransactionId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = OidcTransactionId . _transactionId

-- | Replay identity is the issuer/JTI pair; retained sid/sub evidence also fences
-- login results that were in flight when a logout notification arrived.
data OidcLogoutEventT f = OidcLogoutEvent
  { _logoutEventIssuer :: C f Text,
    _logoutEventJti :: C f Text,
    _logoutEventSid :: C f (Maybe Text),
    _logoutEventSub :: C f (Maybe Text),
    _logoutEventReceivedAt :: C f UTCTime,
    _logoutEventExpiresAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type OidcLogoutEvent = OidcLogoutEventT Identity

deriving instance Eq OidcLogoutEvent

instance Table OidcLogoutEventT where
  data PrimaryKey OidcLogoutEventT f = OidcLogoutEventId (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = OidcLogoutEventId <$> _logoutEventIssuer <*> _logoutEventJti

-- | Separate one-use handoff and provider-return proofs with independent expiry
-- and consumption timestamps; encrypted token material is supplied by callers.
data OidcLogoutStateT f = OidcLogoutState
  { _logoutStateId :: C f UUID,
    _logoutStateHandoffHash :: C f ByteString,
    _logoutStateStateHash :: C f ByteString,
    _logoutStateIdTokenEncrypted :: C f Text,
    _logoutStateEndpoint :: C f Text,
    _logoutStateClientId :: C f Text,
    _logoutStateHandoffExpiresAt :: C f UTCTime,
    _logoutStateStateExpiresAt :: C f UTCTime,
    _logoutStateHandoffConsumedAt :: C f (Maybe UTCTime),
    _logoutStateStateConsumedAt :: C f (Maybe UTCTime)
  }
  deriving (Generic, Beamable)

type OidcLogoutState = OidcLogoutStateT Identity

deriving instance Eq OidcLogoutState

instance Table OidcLogoutStateT where
  data PrimaryKey OidcLogoutStateT f = OidcLogoutStateId {unOidcLogoutStateId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = OidcLogoutStateId . _logoutStateId

deriving instance Eq (PrimaryKey UserSessionT (Nullable Identity))

deriving instance Eq (PrimaryKey AccountInvitationT (Nullable Identity))

-- | Provider identity is keyed by issuer and subject, not email claims.
-- Claimed email fields retain provider evidence without redefining the account key.
data ExternalIdentityT f = ExternalIdentity
  { _identityIssuer :: C f Text,
    _identitySubject :: C f Text,
    _identityUser :: PrimaryKey UserT f,
    _identityLinkedAt :: C f UTCTime,
    _identityLastLoginAt :: C f (Maybe UTCTime),
    _identityClaimedEmail :: C f (Maybe Text),
    _identityClaimedEmailVerified :: C f Bool
  }
  deriving (Generic, Beamable)

deriving instance Eq ExternalIdentity

type ExternalIdentity = ExternalIdentityT Identity

instance Table ExternalIdentityT where
  data PrimaryKey ExternalIdentityT f = ExternalIdentityId (C f Text) (C f Text) deriving (Generic, Beamable)
  primaryKey = ExternalIdentityId <$> _identityIssuer <*> _identitySubject

data AdminT f = Admin
  { _adminId :: C f UUID,
    _adminUsername :: C f Text,
    _adminPasswordHash :: C f Text
  }
  deriving (Generic, Beamable)

type Admin = AdminT Identity

deriving instance Eq Admin

deriving instance Ord Admin

instance Table AdminT where
  data PrimaryKey AdminT f = AdminId {unAdminId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = AdminId . _adminId

type AdminKey = PrimaryKey AdminT Identity

deriving instance Eq AdminKey

deriving instance Show AdminKey

deriving instance Ord AdminKey

-- | Currency policy can become permanently locked by financial writes.
-- The owner foreign key does not itself establish active group membership.
data GroupT f = Group
  { _groupId :: C f UUID,
    _groupName :: C f Text,
    _groupOwner :: PrimaryKey UserT f,
    _currencyMode :: C f Text,
    _defaultCurrency :: C f Text,
    _currencyLocked :: C f Bool
  }
  deriving (Generic, Beamable)

instance Table GroupT where
  data PrimaryKey GroupT f = GroupId {unGroupId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = GroupId . _groupId

type Group = GroupT Identity

deriving instance Eq Group

deriving instance Show Group

deriving instance Ord Group

type GroupKey = PrimaryKey GroupT Identity

deriving instance Eq GroupKey

deriving instance Show GroupKey

deriving instance Ord GroupKey

-- | Stable user/group key with soft removal. Inactive rows remain available for
-- historical resolution even though access queries filter them out.
data GroupMemberT f = GroupMember
  { _gmUser :: PrimaryKey UserT f,
    _gmGroup :: PrimaryKey GroupT f,
    _gmActive :: C f Bool
  }
  deriving (Generic, Beamable)

type GroupMember = GroupMemberT Identity

instance Table GroupMemberT where
  data PrimaryKey GroupMemberT f = GroupMemberId {gmUnUser :: PrimaryKey UserT f, gmUnGroup :: PrimaryKey GroupT f} deriving (Generic, Beamable)
  primaryKey = GroupMemberId <$> _gmUser <*> _gmGroup

type GroupMemberKey = PrimaryKey GroupMemberT Identity

deriving instance Eq GroupMemberKey

deriving instance Show GroupMemberKey

deriving instance Ord GroupMemberKey

deriving instance Eq GroupMember

deriving instance Show GroupMember

deriving instance Ord GroupMember

data ReceiptT f = Receipt
  { _receiptId :: C f UUID,
    _receiptGroup :: PrimaryKey GroupT f,
    _receiptUploadedBy :: PrimaryKey UserT f,
    _receiptNote :: C f Text,
    _receiptCreatedAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type Receipt = ReceiptT Identity

deriving instance Eq Receipt

deriving instance Show Receipt

deriving instance Ord Receipt

instance Table ReceiptT where
  data PrimaryKey ReceiptT f = ReceiptId {unReceiptId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = ReceiptId . _receiptId

type ReceiptKey = PrimaryKey ReceiptT Identity

deriving instance Eq ReceiptKey

deriving instance Show ReceiptKey

deriving instance Ord ReceiptKey

-- | Provider observation retained independently from records. A record also
-- stores its chosen conversion fields so reads need not fetch a current rate.
data ExchangeQuoteT f = ExchangeQuote
  { _quoteId :: C f UUID,
    _quoteGroup :: PrimaryKey GroupT f,
    _quoteSourceCurrency :: C f Text,
    _quoteTargetCurrency :: C f Text,
    _quoteRate :: C f Scientific,
    _quoteProvider :: C f Text,
    _quoteRateDate :: C f Day,
    _quoteFetchedAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type ExchangeQuote = ExchangeQuoteT Identity

deriving instance Eq ExchangeQuote

deriving instance Show ExchangeQuote

deriving instance Ord ExchangeQuote

instance Table ExchangeQuoteT where
  data PrimaryKey ExchangeQuoteT f = ExchangeQuoteId {unExchangeQuoteId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = ExchangeQuoteId . _quoteId

-- | Ledger header with original and settlement amounts. Text discriminators and
-- optional columns encode split records versus transfers; types alone allow invalid combinations.
data RecordT f = Record
  { _recordId :: C f UUID,
    _recordGroup :: PrimaryKey GroupT f,
    _recordKind :: C f Text,
    _title :: C f Text,
    _recordNote :: C f (Maybe Text),
    _amountMinor :: C f Int64,
    _currency :: C f Text,
    _settlementCurrency :: C f Text,
    _settlementAmountMinor :: C f Int64,
    _splitMode :: C f (Maybe Text),
    _exchangeRate :: C f (Maybe Scientific),
    _rateSource :: C f (Maybe Text),
    _rateDate :: C f (Maybe Day),
    _rateFetchedAt :: C f (Maybe UTCTime),
    _exchangeQuoteId :: C f (Maybe UUID),
    _paidBy :: PrimaryKey UserT f,
    -- | Recipient of a member-to-member transfer; absent for split records
    _transferTo :: PrimaryKey UserT (Nullable f),
    _date :: C f Day,
    _createdAt :: C f UTCTime,
    -- | Optional receipt this record belongs to
    _recordReceipt :: PrimaryKey ReceiptT (Nullable f)
  }
  deriving (Generic, Beamable)

type Record = RecordT Identity

instance Table RecordT where
  data PrimaryKey RecordT f = RecordId {unRecordId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = RecordId . _recordId

type RecordKey = PrimaryKey RecordT Identity

deriving instance Eq RecordKey

deriving instance Show RecordKey

deriving instance Ord RecordKey

deriving instance Eq (PrimaryKey UserT (Nullable Identity))

deriving instance Show (PrimaryKey UserT (Nullable Identity))

deriving instance Ord (PrimaryKey UserT (Nullable Identity))

deriving instance Eq (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Show (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Ord (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Eq Record

deriving instance Show Record

deriving instance Ord Record

-- | Saved allocation per record/user, including both currency amounts and the
-- original weight/percentage/exact input. Historical reads must not reallocate it.
data RecordSplitT f = RecordSplit
  { _rsRecord :: PrimaryKey RecordT f,
    _rsUser :: PrimaryKey UserT f,
    _splitValue :: C f Int64,
    _splitAmountMinor :: C f Int64,
    _splitSettlementAmountMinor :: C f Int64
  }
  deriving (Generic, Beamable)

type RecordSplit = RecordSplitT Identity

type RecordSplitKey = PrimaryKey RecordSplitT Identity

deriving instance Eq (PrimaryKey RecordSplitT Identity)

deriving instance Show (PrimaryKey RecordSplitT Identity)

deriving instance Ord (PrimaryKey RecordSplitT Identity)

deriving instance Eq RecordSplit

deriving instance Show RecordSplit

deriving instance Ord RecordSplit

instance Table RecordSplitT where
  data PrimaryKey RecordSplitT f = RecordSplitId {rsUnRecordId :: PrimaryKey RecordT f, rsUnUserId :: PrimaryKey UserT f} deriving (Generic, Beamable)
  primaryKey :: RecordSplitT column -> PrimaryKey RecordSplitT column
  primaryKey = RecordSplitId <$> _rsRecord <*> _rsUser

-- | Table registry used by Beam queries. PrimaryKey instances describe joins;
-- database constraints and deletion behavior come from the installed SQL schema.
data JizhangDb f = JizhangDb
  { _users :: f (TableEntity UserT),
    _externalIdentities :: f (TableEntity ExternalIdentityT),
    _authSettings :: f (TableEntity AuthSettingsT),
    _userSessions :: f (TableEntity UserSessionT),
    _accountInvitations :: f (TableEntity AccountInvitationT),
    _oidcTransactions :: f (TableEntity OidcTransactionT),
    _oidcLogoutEvents :: f (TableEntity OidcLogoutEventT),
    _oidcLogoutStates :: f (TableEntity OidcLogoutStateT),
    _admins :: f (TableEntity AdminT),
    _groups :: f (TableEntity GroupT),
    _groupMembers :: f (TableEntity GroupMemberT),
    _receipts :: f (TableEntity ReceiptT),
    _exchangeQuotes :: f (TableEntity ExchangeQuoteT),
    _records :: f (TableEntity RecordT),
    _recordSplits :: f (TableEntity RecordSplitT)
  }
  deriving (Generic, Database be)

-- | Override Beam's inferred identifiers to match the packaged SQL, including
-- nested foreign-key names such as user__id. Unlisted fields keep Beam defaults.
jizhangDb :: DatabaseSettings be JizhangDb
jizhangDb =
  defaultDbSettings
    `withDbModification` JizhangDb
      { _users =
          setEntityName "users"
            <> modifyTableFields tableModification {_displayName = "display_name", _emailVerified = "email_verified", _passwordHash = "password_hash", _enabled = "enabled", _authInvalidatedAt = "auth_invalidated_at"},
        _externalIdentities =
          setEntityName "external_identities"
            <> modifyTableFields tableModification {_identityIssuer = "issuer", _identitySubject = "subject", _identityUser = UserId "user__id", _identityLinkedAt = "linked_at", _identityLastLoginAt = "last_login_at", _identityClaimedEmail = "claimed_email", _identityClaimedEmailVerified = "claimed_email_verified"},
        _authSettings =
          setEntityName "auth_settings"
            <> modifyTableFields tableModification {_settingsId = "id", _settingsRevision = "revision", _settingsLocalRegistrationEnabled = "local_registration_enabled", _settingsSsoRegistrationEnabled = "sso_registration_enabled", _settingsOidcEnabled = "oidc_enabled", _settingsProviderName = "provider_name", _settingsIssuer = "issuer", _settingsClientId = "client_id", _settingsClientSecretEncrypted = "client_secret_encrypted"},
        _userSessions =
          setEntityName "user_sessions"
            <> modifyTableFields tableModification {_sessionId = "id", _sessionUser = UserId "user__id", _sessionMethod = "method", _sessionCreatedAt = "created_at", _sessionAuthenticatedAt = "authenticated_at", _sessionExpiresAt = "expires_at", _sessionOidcReauthenticatedAt = "oidc_reauthenticated_at", _sessionRevokedAt = "revoked_at", _sessionOidcIssuer = "oidc_issuer", _sessionOidcSubject = "oidc_subject", _sessionOidcSid = "oidc_sid", _sessionIdTokenEncrypted = "id_token_encrypted"},
        _accountInvitations =
          setEntityName "account_invitations"
            <> modifyTableFields tableModification {_invitationId = "id", _invitationTokenHash = "token_hash", _invitationPurpose = "purpose", _invitationUser = UserId "user__id", _invitationAdmin = AdminId "admin__id", _invitationProviderRevision = "provider_revision", _invitationCreatedAt = "created_at", _invitationExpiresAt = "expires_at", _invitationConsumedAt = "consumed_at", _invitationRevokedAt = "revoked_at"},
        _oidcTransactions =
          setEntityName "oidc_transactions"
            <> modifyTableFields tableModification {_transactionId = "id", _transactionStateHash = "state_hash", _transactionProviderRevision = "provider_revision", _transactionPurpose = "purpose", _transactionUser = UserId "user__id", _transactionSession = UserSessionId "session__id", _transactionInvitation = AccountInvitationId "invitation__id", _transactionBrowserChallenge = "browser_challenge", _transactionNonceHash = "nonce_hash", _transactionPkceVerifierEncrypted = "pkce_verifier_encrypted", _transactionCreatedAt = "created_at", _transactionExpiresAt = "expires_at", _transactionPhase = "phase", _transactionHandoffHash = "handoff_hash", _transactionContinuationHash = "continuation_hash", _transactionResultPayloadEncrypted = "result_payload_encrypted", _transactionPasswordAttempts = "password_attempts"},
        _oidcLogoutEvents =
          setEntityName "oidc_logout_events"
            <> modifyTableFields tableModification {_logoutEventIssuer = "issuer", _logoutEventJti = "jti", _logoutEventSid = "sid", _logoutEventSub = "sub", _logoutEventReceivedAt = "received_at", _logoutEventExpiresAt = "expires_at"},
        _oidcLogoutStates =
          setEntityName "oidc_logout_states"
            <> modifyTableFields tableModification {_logoutStateId = "id", _logoutStateHandoffHash = "handoff_hash", _logoutStateStateHash = "state_hash", _logoutStateIdTokenEncrypted = "id_token_encrypted", _logoutStateEndpoint = "endpoint", _logoutStateClientId = "client_id", _logoutStateHandoffExpiresAt = "handoff_expires_at", _logoutStateStateExpiresAt = "state_expires_at", _logoutStateHandoffConsumedAt = "handoff_consumed_at", _logoutStateStateConsumedAt = "state_consumed_at"},
        _admins =
          setEntityName "admins"
            <> modifyTableFields tableModification {_adminPasswordHash = "password_hash"},
        _groups =
          setEntityName "groups"
            <> modifyTableFields
              tableModification
                { _groupOwner = UserId "owner__id",
                  _currencyMode = "currency_mode",
                  _defaultCurrency = "default_currency",
                  _currencyLocked = "currency_locked"
                },
        _groupMembers =
          setEntityName "group_members"
            <> modifyTableFields tableModification {_gmActive = "active"},
        _receipts =
          setEntityName "receipts"
            <> modifyTableFields
              tableModification
                { _receiptUploadedBy = UserId "uploaded_by__id",
                  _receiptCreatedAt = "created_at"
                },
        _exchangeQuotes =
          setEntityName "exchange_quotes"
            <> modifyTableFields
              tableModification
                { _quoteId = "id",
                  _quoteGroup = GroupId "group__id",
                  _quoteSourceCurrency = "source_currency",
                  _quoteTargetCurrency = "target_currency",
                  _quoteRate = "rate",
                  _quoteProvider = "provider",
                  _quoteRateDate = "rate_date",
                  _quoteFetchedAt = "fetched_at"
                },
        _records =
          setEntityName "records"
            <> modifyTableFields
              tableModification
                { _recordKind = "record_kind",
                  _recordNote = "note",
                  _amountMinor = "amount_minor",
                  _currency = "currency",
                  _settlementCurrency = "settlement_currency",
                  _settlementAmountMinor = "settlement_amount_minor",
                  _splitMode = "split_mode",
                  _exchangeRate = "exchange_rate",
                  _rateSource = "rate_source",
                  _rateDate = "rate_date",
                  _rateFetchedAt = "rate_fetched_at",
                  _exchangeQuoteId = "exchange_quote__id",
                  _createdAt = "created_at"
                },
        _recordSplits =
          setEntityName "record_splits"
            <> modifyTableFields
              tableModification
                { _splitValue = "split_value",
                  _splitAmountMinor = "amount_minor",
                  _splitSettlementAmountMinor = "settlement_amount_minor"
                }
      }
