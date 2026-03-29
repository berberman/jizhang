{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.Receipt where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Record
import Jizhang.API.Types.User
import Servant (FromHttpApiData, ToHttpApiData)

-- | Receipt IDs (UUID)
newtype ReceiptId = ReceiptId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | A receipt is a group of records uploaded together with an optional note,
-- associated with a group and uploader
data Receipt = Receipt
  { receiptId :: !ReceiptId,
    groupId :: !GroupId,
    uploadedBy :: !User,
    note :: !Text,
    records :: ![Record],
    createdAt :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateReceiptRequest = CreateReceiptRequest
  { note :: !Text,
    records :: ![ExpenseRecordRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateReceiptRequest = UpdateReceiptRequest
  { note :: !Text,
    records :: ![ExpenseRecordRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
