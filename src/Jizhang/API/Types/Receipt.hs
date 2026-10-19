{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Receipt envelopes group ledger records under one uploader and note. JSON
-- uses literal field names; notes are required strings rather than nullable text.
module Jizhang.API.Types.Receipt where

import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
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

-- | Receipt UUID encoded directly as a JSON or URL UUID string.
newtype ReceiptId = ReceiptId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | A stored upload with public uploader identity and fully projected records.
-- The note may be empty, but the codec requires a non-null note field.
-- Generic response decoding ignores extra keys and does not validate record grouping.
data Receipt = Receipt
  { receiptId :: !ReceiptId,
    -- | Group whose ledger contains the receipt.
    groupId :: !GroupId,
    -- | Public uploader identity, without private account metadata.
    uploadedBy :: !User,
    note :: !Text,
    -- | Full ledger records, rather than the expense-only submission bodies.
    records :: ![Record],
    -- | Server upload time, separate from each record's accounting date.
    createdAt :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Required note and expense-record array. Unknown fields are rejected;
-- the decoder does not require a nonempty array or validate receipt-wide rules.
data CreateReceiptRequest = CreateReceiptRequest
  { note :: !Text,
    records :: ![ExpenseRecordRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON CreateReceiptRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Replacement note and expense-record array, not a partial patch. Nested money
-- codecs still apply; receipt-level consistency is not checked by this decoder.
data UpdateReceiptRequest = UpdateReceiptRequest
  { note :: !Text,
    records :: ![ExpenseRecordRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON UpdateReceiptRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}
