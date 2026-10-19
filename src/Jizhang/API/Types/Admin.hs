{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Operator credentials and privileged ledger requests. JSON uses literal field
-- names; generic decoders here accept extra keys unless a custom decoder rejects them.
-- Several credential-bearing types derive Show, so whole-value logging is unsafe.
module Jizhang.API.Types.Admin where

import Control.Lens ((^.))
import qualified Crypto.JWT as Jose
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (String), defaultOptions, fromJSON, genericParseJSON, rejectUnknownFields, withObject, withText, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.Data (Typeable)
import qualified Data.Map.Strict as M
import Data.Swagger (ToSchema (..))
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Money
import Jizhang.API.Types.Record (ExpenseRecordRequest)
import Jizhang.API.Types.User (UserId)
import Servant.Auth.JWT (FromJWT (..), ToJWT)

-- | Bootstrap operator credentials with required prefixed JSON keys. Plaintext
-- passwords remain sensitive in both the JSON encoding and derived Show output.
data AdminBootstrap = AdminBootstrap
  { adminBootstrapUsername :: !Text,
    adminBootstrapPassword :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Operator login using required username/password strings. The codec performs
-- no credential validation and, unlike user login requests, accepts extra keys.
data AdminLoginRequest = AdminLoginRequest
  { username :: !Text,
    password :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Operator bearer token and operator handle; this is not an application-user
-- LoginResponse and must not be exposed as a public ledger identity.
data AdminLoginResponse = AdminLoginResponse
  { accessToken :: !Text,
    adminUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Operator UUID and summaryUsername, without credentials or user-profile data.
data AdminSummary = AdminSummary
  { adminId :: !UUID,
    summaryUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Account creation branch encoded as the string "local" or "sso".
data AdminCreateUserMode = AdminCreateLocal | AdminCreateSso
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON AdminCreateUserMode where
  toJSON AdminCreateLocal = String "local"
  toJSON AdminCreateSso = String "sso"

instance FromJSON AdminCreateUserMode where
  parseJSON = withText "AdminCreateUserMode" $ \value -> case value of
    "local" -> pure AdminCreateLocal
    "sso" -> pure AdminCreateSso
    _ -> fail "Invalid createMode"

instance ToSchema AdminCreateUserMode where
  declareNamedSchema _ =
    pure $
      Swagger.NamedSchema (Just "AdminCreateUserMode") $
        mempty
          { Swagger._schemaParamSchema =
              mempty
                { Swagger._paramSchemaType = Just Swagger.SwaggerString,
                  Swagger._paramSchemaEnum = Just [String "local", String "sso"]
                }
          }

-- | Required account fields with unknown keys rejected. createPassword must be
-- present but may be null; the codec does not enforce its relationship to createMode.
data AdminCreateUserRequest = AdminCreateUserRequest
  { -- | Requested public account handle.
    createUsername :: !Text,
    -- | Requested public name attached to ledger activity.
    createDisplayName :: !Text,
    -- | Private account email, not part of the public User projection.
    createEmail :: !Text,
    -- | Requested password-backed or SSO account branch.
    createMode :: !AdminCreateUserMode,
    -- | Sensitive plaintext password, or explicit null; omission is rejected.
    createPassword :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminCreateUserRequest where
  parseJSON = withObject "AdminCreateUserRequest" $ \o ->
    if any (`notElem` ["createUsername", "createDisplayName", "createEmail", "createMode", "createPassword"]) (KM.keys o)
      then fail "Unknown user field"
      else
        AdminCreateUserRequest
          <$> o .: "createUsername"
          <*> o .: "createDisplayName"
          <*> o .: "createEmail"
          <*> o .: "createMode"
          <*> o .: "createPassword"

-- | Required createAdminUsername/createAdminPassword strings. Extra JSON keys
-- are ignored by the generic decoder; credential policy is enforced elsewhere.
data AdminCreateAdminRequest = AdminCreateAdminRequest
  { createAdminUsername :: !Text,
    createAdminPassword :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Create a group for a specified owner. Missing currency fields default to
-- "single" and "USD"; explicit null and unknown keys are rejected.
data AdminCreateGroupRequest = AdminCreateGroupRequest
  { createGroupName :: !Text,
    -- | Owner UUID chosen by the operator, not inferred from the operator token.
    ownerUserId :: !UserId,
    currencyMode :: !CurrencyMode,
    defaultCurrency :: !CurrencyCode
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminCreateGroupRequest where
  parseJSON = withObject "AdminCreateGroupRequest" $ \o -> do
    if any (`notElem` ["createGroupName", "ownerUserId", "currencyMode", "defaultCurrency"]) (KM.keys o)
      then fail "Unknown group field"
      else
        AdminCreateGroupRequest
          <$> o .: "createGroupName"
          <*> o .: "ownerUserId"
          <*> (if KM.member "currencyMode" o then o .: "currencyMode" else pure SingleCurrency)
          <*> (if KM.member "defaultCurrency" o then o .: "defaultCurrency" else pure (CurrencyCode "USD"))

-- | Upload expense records on behalf of an application user. All keys are
-- required; unknown keys are rejected, while receipt-wide validation is deferred.
data AdminCreateReceiptRequest = AdminCreateReceiptRequest
  { -- | Application-user uploader UUID, distinct from the authenticated operator.
    uploadedByUserId :: !UserId,
    -- | Non-null receipt note; the decoder permits empty text.
    note :: !Text,
    -- | Expense-only submissions using bounded exact money codecs.
    records :: ![ExpenseRecordRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON AdminCreateReceiptRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Operator JWT data, not a public user or application session. Decoding claims
-- requires exp and the servant-auth dat payload; expiry verification is external.
data AuthAdmin = AuthAdmin
  { authAdminId :: !UUID,
    authAdminUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToJWT)

-- | Require claim presence before decoding the payload. This method alone does
-- not verify signatures or compare the expiration timestamp with the current time.
instance FromJWT AuthAdmin where
  decodeJWT claims = case claims ^. Jose.claimExp of
    Nothing -> Left "Missing 'exp' claim"
    Just _ ->
      -- Preserve servant-auth's 'dat' payload format through jose's legacy accessor.
      case M.lookup "dat" (claims ^. Jose.unregisteredClaims) of
        Nothing -> Left "Missing 'dat' claim"
        Just value -> case fromJSON value of
          Error message -> Left $ T.pack message
          Success admin -> Right admin
