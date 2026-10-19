{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ledger JSON contracts. Record fields use their Haskell names; response sums
-- use Aeson's default "tag" discriminator with constructor names and flat fields.
-- Monetary strings are exact; these decoders do not perform ledger validation.
module Jizhang.API.Types.Record where

import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields, withObject)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Money
import Jizhang.API.Types.User
import Servant (FromHttpApiData, ToHttpApiData)

-- | Ledger record UUID, encoded as a JSON UUID string and URL UUID parameter.
newtype RecordId = RecordId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Computed participant allocation. Unknown fields are rejected, but decoding
-- does not check these amounts against the parent record's split mode or total.
data RecordSplit = RecordSplit
  { -- | Public participant identity, without private profile metadata.
    user :: !User,
    -- | Original split input, interpreted according to the parent splitMode.
    value :: !MinorAmount,
    -- | Allocated amount in the original currency's minor units.
    amountMinor :: !MinorAmount,
    -- | Allocated amount in the settlement currency's minor units.
    settlementAmountMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON RecordSplit where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Participant UUID and exact integer-string input. Equal splits use 1, shares
-- use weights, percentages use basis points, and exact splits use minor units.
data RecordSplitRequest = RecordSplitRequest
  { -- | Referenced participant; membership and uniqueness are not decoded here.
    userId :: !UserId,
    -- | Magnitude is bounded by maxRecordMinor; sign and mode rules are deferred.
    value :: !MinorAmount
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON RecordSplitRequest where
  parseJSON input = do
    withObject "RecordSplitRequest" (checkRequestMinorAmount "value") input
    genericParseJSON defaultOptions {rejectUnknownFields = True} input

-- | Expense submission with original-currency amount and payer UUID. Unknown
-- keys and overlarge amounts are rejected; positivity and allocation are deferred.
data ExpenseRecordRequest = ExpenseRecordRequest
  { -- | Required string; this codec does not normalize or reject blank titles.
    title :: !Text,
    -- | Optional text: omitted or null decodes to Nothing and encodes as null.
    note :: !(Maybe Text),
    -- | Exact minor-unit string, with absolute magnitude at most maxRecordMinor.
    amountMinor :: !MinorAmount,
    -- | Original currency code, not response CurrencyInfo metadata.
    currency :: !CurrencyCode,
    -- | User who paid, not necessarily the authenticated caller.
    byUserId :: !UserId,
    -- | Accounting day, distinct from server creation time.
    date :: !Day,
    -- | Interpretation of every submitted split value.
    splitMode :: !SplitMode,
    -- | Allocation inputs; the decoder allows empty or repeated participants.
    splits :: ![RecordSplitRequest],
    -- | Omitted/null when no rate is supplied; currency-policy checks are deferred.
    exchangeRate :: !(Maybe ExchangeRateInput)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ExpenseRecordRequest where
  parseJSON input = do
    withObject "ExpenseRecordRequest" (checkRequestMinorAmount "amountMinor") input
    genericParseJSON defaultOptions {rejectUnknownFields = True} input

-- | Income submission: receivedByUserId identifies the recipient, while splits
-- describe entitlement. Shape and magnitude checks match ExpenseRecordRequest.
data IncomeRecordRequest = IncomeRecordRequest
  { title :: !Text,
    -- | Optional note; omitted and null both decode to Nothing.
    note :: !(Maybe Text),
    -- | Original-currency minor units, not a decimal major-unit amount.
    amountMinor :: !MinorAmount,
    currency :: !CurrencyCode,
    -- | Recipient UUID, rather than the expense request's byUserId key.
    receivedByUserId :: !UserId,
    date :: !Day,
    splitMode :: !SplitMode,
    -- | Entitlement inputs using splitMode, not computed response allocations.
    splits :: ![RecordSplitRequest],
    -- | Optional manual/reference conversion input; omitted or null is accepted.
    exchangeRate :: !(Maybe ExchangeRateInput)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON IncomeRecordRequest where
  parseJSON input = do
    withObject "IncomeRecordRequest" (checkRequestMinorAmount "amountMinor") input
    genericParseJSON defaultOptions {rejectUnknownFields = True} input

-- | Payment from byUserId to toUserId in the stated currency, without splits or
-- rate input. Shape/magnitude checks do not enforce positivity or distinct users.
data TransferRecordRequest = TransferRecordRequest
  { -- | Exact integer-string minor units bounded by maxRecordMinor in magnitude.
    amountMinor :: !MinorAmount,
    currency :: !CurrencyCode,
    byUserId :: !UserId,
    toUserId :: !UserId,
    date :: !Day,
    -- | Optional note; missing and null decode identically.
    note :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON TransferRecordRequest where
  parseJSON input = do
    withObject "TransferRecordRequest" (checkRequestMinorAmount "amountMinor") input
    genericParseJSON defaultOptions {rejectUnknownFields = True} input

-- | Stored expense, income or payment with public user projections. JSON tags
-- are "ExpenseRecord", "IncomeRecord" and "TransferRecord", not lowercase kinds.
-- Notes and rates encode null for Nothing; missing nullable fields also decode.
data Record
  = ExpenseRecord
      { recordId :: !RecordId,
        title :: !Text,
        note :: !(Maybe Text),
        -- | Gross original-currency amount; not the amount owed by one member.
        amountMinor :: !MinorAmount,
        -- | Original currency and its minor-unit precision.
        currency :: !CurrencyInfo,
        -- | Gross converted amount used for settlement accounting.
        settlementAmountMinor :: !MinorAmount,
        -- | Currency and precision applying to settlementAmountMinor and splits.
        settlementCurrency :: !CurrencyInfo,
        -- | Conversion provenance, or null when no conversion was used.
        exchangeRate :: !(Maybe ExchangeRate),
        -- | Public payer identity; income instead exposes receivedBy.
        paidBy :: !User,
        date :: !Day,
        -- | Server creation timestamp, not the accounting date.
        createdAt :: !UTCTime,
        groupId :: !GroupId,
        splitMode :: !SplitMode,
        -- | Inputs plus computed allocations in both currencies.
        splits :: ![RecordSplit]
      }
  | IncomeRecord
      { recordId :: !RecordId,
        title :: !Text,
        note :: !(Maybe Text),
        amountMinor :: !MinorAmount,
        currency :: !CurrencyInfo,
        settlementAmountMinor :: !MinorAmount,
        settlementCurrency :: !CurrencyInfo,
        exchangeRate :: !(Maybe ExchangeRate),
        -- | Public recipient identity; split allocations represent entitlements.
        receivedBy :: !User,
        date :: !Day,
        createdAt :: !UTCTime,
        groupId :: !GroupId,
        splitMode :: !SplitMode,
        splits :: ![RecordSplit]
      }
  | TransferRecord
      { recordId :: !RecordId,
        title :: !Text,
        note :: !(Maybe Text),
        amountMinor :: !MinorAmount,
        currency :: !CurrencyInfo,
        settlementAmountMinor :: !MinorAmount,
        settlementCurrency :: !CurrencyInfo,
        exchangeRate :: !(Maybe ExchangeRate),
        paidBy :: !User,
        -- | Public payment recipient; transfer responses have no split fields.
        transferTo :: !User,
        date :: !Day,
        createdAt :: !UTCTime,
        groupId :: !GroupId
      }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}
