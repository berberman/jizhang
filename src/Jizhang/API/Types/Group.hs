{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Group JSON contracts with literal field names and public-only user values.
-- Decoders check object shape, not ownership, membership or currency catalog rules.
module Jizhang.API.Types.Group where

import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields, withObject, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Money
import Jizhang.API.Types.User
import Servant (FromHttpApiData, ToHttpApiData)

-- | Group UUID encoded as a JSON string and a URL UUID parameter.
newtype GroupId = GroupId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Complete group projection; every field is required and non-null. Unknown
-- fields are rejected, but member uniqueness and owner membership are not checked.
data Group = Group
  { groupId :: !GroupId,
    -- | Public display label; decoding does not trim or reject blank text.
    groupName :: !Text,
    -- | Owner's public identity, without private account or provider details.
    owner :: !User,
    -- | Public member identities; this is not a list of private profiles.
    members :: ![User],
    -- | Single- or multi-currency accounting policy.
    currencyMode :: !CurrencyMode,
    -- | Configured default currency, including display precision.
    defaultCurrency :: !CurrencyInfo,
    -- | Whether the group projection reports its currency configuration locked.
    currencyLocked :: !Bool
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Group where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | groupName is required; omitted currencyMode/defaultCurrency default to
-- "single"/"USD". Explicit null is rejected, as are unknown keys.
data CreateGroupRequest = CreateGroupRequest
  { groupName :: !Text,
    currencyMode :: !CurrencyMode,
    defaultCurrency :: !CurrencyCode
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON CreateGroupRequest where
  parseJSON = withObject "CreateGroupRequest" $ \o -> do
    if any (`notElem` ["groupName", "currencyMode", "defaultCurrency"]) (KM.keys o)
      then fail "Unknown group field"
      else
        CreateGroupRequest
          <$> o .: "groupName"
          <*> (if KM.member "currencyMode" o then o .: "currencyMode" else pure SingleCurrency)
          <*> (if KM.member "defaultCurrency" o then o .: "defaultCurrency" else pure (CurrencyCode "USD"))

-- | Full replacement of name and currency configuration, not a partial patch.
-- All three keys are required; unlike creation there are no currency defaults.
data UpdateGroupRequest = UpdateGroupRequest
  { groupName :: !Text,
    currencyMode :: !CurrencyMode,
    defaultCurrency :: !CurrencyCode
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON UpdateGroupRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}
