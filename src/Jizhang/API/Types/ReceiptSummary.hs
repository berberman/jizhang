{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Jizhang.API.Types.ReceiptSummary where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Char (ord)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema (..))
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)

-- | Receipt UUID encoded directly as a JSON or URL UUID string.
newtype ReceiptId = ReceiptId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

data ReceiptSummary = ReceiptSummary
  { summaryReceiptId :: !ReceiptId,
    summaryReceiptTitle :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptSummary where
  toJSON (ReceiptSummary receiptId title) = object ["receiptId" .= receiptId, "title" .= title]

instance FromJSON ReceiptSummary where
  parseJSON = withObject "ReceiptSummary" $ \o -> ReceiptSummary <$> o .: "receiptId" <*> o .: "title"

instance ToSchema ReceiptSummary where
  declareNamedSchema = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
    { Swagger.fieldLabelModifier = \name -> case name of
        "summaryReceiptId" -> "receiptId"
        "summaryReceiptTitle" -> "title"
        other -> other
    }

normalizeReceiptTitle :: Text -> Either Text Text
normalizeReceiptTitle value
  | T.any forbidden value = Left "Receipt title cannot contain control characters."
  | T.null trimmed = Left "Receipt title is required."
  | T.length trimmed > 512 = Left "Receipt title must be at most 512 characters."
  | otherwise = Right trimmed
  where
    trimmed = T.strip value
    forbidden character = let code = ord character in code <= 0x1f || (code >= 0x7f && code <= 0x9f) || code == 0x2028 || code == 0x2029
