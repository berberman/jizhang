{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Separate public ledger identities from private account profiles and JWT data.
-- JSON uses literal field names; these codecs do not validate profile text.
-- Generic response decoders ignore unknown keys; profile updates reject them.
module Jizhang.API.Types.User where

import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Auth.JWT (FromJWT, ToJWT)

-- | UUID identity, encoded as a UUID string in JSON and URL parameters.
newtype UserId = UserId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Public identity embedded in group and ledger responses. No email, provider
-- subject, credentials or session identifiers belong in this projection.
data User = User
  { -- | Stable identity; unlike names, this is suitable for joins and references.
    userId :: !UserId,
    -- | Public account handle, not an email address.
    username :: !Text,
    -- | Public human-readable name; it need not be unique.
    displayName :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Private profile projection: email and its verification flag are additional
-- account data, not part of 'User'. All three JSON fields are required and non-null.
data UserProfile = UserProfile
  { user :: !User,
    -- | Private contact address, separate from the public username.
    email :: !Text,
    -- | Local profile verification state, distinct from provider email claims.
    emailVerified :: !Bool
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Replace both public names. Unknown keys, missing keys and null are rejected;
-- text normalization and account-name policy are handled outside this decoder.
data UpdateProfileRequest = UpdateProfileRequest
  { username :: !Text,
    displayName :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON UpdateProfileRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Application JWT payload tying a user UUID to a particular session UUID.
-- This is authentication data, not the public identity returned in ledger objects.
data AuthUser = AuthUser
  { -- | Account identity used when resolving the authenticated principal.
    authUserId :: !UUID,
    -- | Session identity used for session-specific authentication checks.
    authSessionId :: !UUID
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToJWT, FromJWT)
