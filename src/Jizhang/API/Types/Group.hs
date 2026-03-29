{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.Group where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.User
import Servant (FromHttpApiData, ToHttpApiData)

-- | Group IDs (UUID)
newtype GroupId = GroupId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Group with owner and members for API responses
data Group = Group
  { groupId :: !GroupId,
    groupName :: !Text,
    owner :: !User,
    members :: ![User]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
