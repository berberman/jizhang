{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reference-quote JSON objects. Unknown keys are rejected on both request and
-- response decoding; currency support and quote validity are resolved elsewhere.
module Jizhang.API.Types.ExchangeRate
  ( ExchangeQuoteRequest (..),
    ExchangeQuote (..),
  )
where

import Data.Aeson
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Jizhang.API.Types.Money

-- | An object with one required currency string, not a bare currency string.
-- It selects the source currency; the request carries no target or manual rate.
newtype ExchangeQuoteRequest = ExchangeQuoteRequest
  { currency :: CurrencyCode
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ExchangeQuoteRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Saved reference rate and provenance. All fields are required and non-null;
-- the rate is an exact decimal string, never a JSON floating-point number.
data ExchangeQuote = ExchangeQuote
  { -- | Stable reference accepted by ReferenceRateInput.
    quoteId :: !ExchangeQuoteId,
    -- | Currency being converted, including its minor-unit precision.
    sourceCurrency :: !CurrencyInfo,
    -- | Currency in which the converted amount is expressed.
    targetCurrency :: !CurrencyInfo,
    -- | Target major units per source major unit.
    rate :: !DecimalRate,
    -- | Reference provider label recorded with this quote.
    provider :: !Text,
    -- | Date to which the provider's rate applies.
    rateDate :: !Day,
    -- | Time the reference data was fetched, not its accounting date.
    fetchedAt :: !UTCTime
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ExchangeQuote where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}
