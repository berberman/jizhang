{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Receipt envelopes group ledger records under one uploader, title and note.
module Jizhang.API.Types.Receipt where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Null), defaultOptions, genericToJSON, omitNothingFields, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Data (Typeable)
import Data.Swagger (ToSchema (..))
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Record (ExpenseRecordRequest, Record)
import Jizhang.API.Types.ReceiptAI (ReceiptSourceInput, ReceiptSourceMeta, ReceiptSourceChange (..))
import Jizhang.API.Types.User
import Jizhang.API.Types.ReceiptSummary (ReceiptId, normalizeReceiptTitle)

-- | A stored upload with public uploader identity and fully projected records.
-- The note may be empty, but the codec requires a non-null note field.
-- Responses require source, including null for receipts without an attachment.
data Receipt = Receipt
  { receiptId :: !ReceiptId,
    -- | Group whose ledger contains the receipt.
    groupId :: !GroupId,
    -- | Public uploader identity, without private account metadata.
    uploadedBy :: !User,
    title :: !Text,
    note :: !Text,
    -- | Full ledger records, rather than the expense-only submission bodies.
    records :: ![Record],
    -- | Server upload time, separate from each record's accounting date.
    createdAt :: !UTCTime,
    source :: !(Maybe ReceiptSourceMeta)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON)

instance ToSchema Receipt where
  declareNamedSchema proxy = do
    Swagger.NamedSchema name schema <- Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    pure $ Swagger.NamedSchema name schema
      { Swagger._schemaRequired = "source" : filter (/= "source") (Swagger._schemaRequired schema) }

instance FromJSON Receipt where
  parseJSON = withObject "Receipt" $ \o ->
    Receipt <$> o .: "receiptId" <*> o .: "groupId" <*> o .: "uploadedBy"
      <*> o .: "title" <*> o .: "note" <*> o .: "records" <*> o .: "createdAt" <*> o .: "source"

-- | Required title, note and expense-record array. Unknown fields are rejected;
-- an empty child array and empty note are permitted.
data CreateReceiptRequest = CreateReceiptRequest
  { title :: !Text,
    note :: !Text,
    records :: ![ExpenseRecordRequest],
    source :: !(Maybe ReceiptSourceInput)
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToSchema)

instance ToJSON CreateReceiptRequest where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON CreateReceiptRequest where
  parseJSON = withObject "CreateReceiptRequest" $ \o -> do
    if any (`notElem` ["title", "note", "records", "source"]) (KM.keys o)
      then fail "Unknown receipt field"
      else do
        receiptTitle <- o .: "title" >>= either (fail . T.unpack) pure . normalizeReceiptTitle
        CreateReceiptRequest receiptTitle <$> o .: "note" <*> o .: "records" <*> o .:? "source"

-- | Replace metadata and children; omitted source preserves the attachment, null
-- removes it, and an object replaces it in the same transaction.
data UpdateReceiptRequest = UpdateReceiptRequest
  { title :: !Text,
    note :: !Text,
    records :: ![ExpenseRecordRequest],
    source :: !ReceiptSourceChange
  }
  deriving stock (Eq, Ord, Generic, Typeable)

instance ToSchema UpdateReceiptRequest where
  declareNamedSchema proxy = do
    Swagger.NamedSchema name schema <- Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    pure $ Swagger.NamedSchema name schema
      { Swagger._schemaRequired = filter (/= "source") (Swagger._schemaRequired schema),
        Swagger._schemaDescription = Just "Full title/note/records replacement. Omit source to preserve it; null removes it; a ReceiptSourceInput object replaces it."
      }

instance FromJSON UpdateReceiptRequest where
  parseJSON = withObject "UpdateReceiptRequest" $ \o -> do
    if any (`notElem` ["title", "note", "records", "source"]) (KM.keys o)
      then fail "Unknown receipt field"
      else do
        receiptTitle <- o .: "title" >>= either (fail . T.unpack) pure . normalizeReceiptTitle
        change <- case KM.lookup "source" o of
          Nothing -> pure KeepReceiptSource
          Just Null -> pure RemoveReceiptSource
          Just value -> ReplaceReceiptSource <$> parseJSON value
        UpdateReceiptRequest receiptTitle <$> o .: "note" <*> o .: "records" <*> pure change

instance ToJSON UpdateReceiptRequest where
  toJSON (UpdateReceiptRequest receiptTitle receiptNote receiptRecords change) =
    object $ ["title" .= receiptTitle, "note" .= receiptNote, "records" .= receiptRecords] <> case change of
      KeepReceiptSource -> []
      RemoveReceiptSource -> ["source" .= Null]
      ReplaceReceiptSource value -> ["source" .= value]
