{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.Record where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Int (Int16)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.User
import Servant (FromHttpApiData, ToHttpApiData)

-- | Record IDs (UUID)
newtype RecordId = RecordId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Record splits for API responses
data RecordSplit = RecordSplit
  { user :: !User,
    share :: !Int16,
    splitAmount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecordSplitRequest = RecordSplitRequest
  { username :: !Username,
    share :: !Int16
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExpenseRecordRequest = ExpenseRecordRequest
  { title :: !Text,
    amount :: !Double,
    byUsername :: !Username,
    date :: !Day,
    splits :: ![RecordSplitRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferRecordRequest = TransferRecordRequest
  { amount :: !Double,
    byUsername :: !Username,
    toUsername :: !Username,
    date :: !Day
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Record for API responses, includes all splits
data Record
  = ExpenseRecord
      { recordId :: !RecordId,
        title :: !Text,
        amount :: !Double,
        paidBy :: !User,
        date :: !Day,
        createdAt :: !UTCTime,
        groupId :: !GroupId,
        splits :: ![RecordSplit]
      }
  | TransferRecord
      { recordId :: !RecordId,
        title :: !Text,
        amount :: !Double,
        paidBy :: !User,
        transferTo :: !User,
        date :: !Day,
        createdAt :: !UTCTime,
        groupId :: !GroupId
      }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
