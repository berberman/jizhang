{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Dated expense/income summaries, excluding transfers. Response money uses
-- exact integer strings in each section's settlement currency, with public users.
-- Derived object decoders check types, not arithmetic or date-range consistency.
-- Response keys retain their field spelling and unknown keys are ignored.
module Jizhang.API.Types.Statistics where

import Data.Aeson
import Data.Data (Typeable)
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Money
import Jizhang.API.Types.Record
import Jizhang.API.Types.User
import Servant

-- | URL-only calendar date requiring a round-tripping, ten-character YYYY-MM-DD.
-- Invalid dates and permissive alternative spellings are rejected.
newtype StatsDate = StatsDate Day deriving stock (Show, Eq, Ord)

instance FromHttpApiData StatsDate where
  parseUrlPiece text = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack text) of
    Just day
      | T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day) == text,
        T.length text == 10 ->
          Right (StatsDate day)
    _ -> Left "Expected a canonical date YYYY-MM-DD"

instance ToHttpApiData StatsDate where
  toUrlPiece (StatsDate day) = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)

instance ToParamSchema StatsDate where
  toParamSchema _ = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaPattern = Just "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"}

-- | Calendar granularity encoded identically in JSON and URLs as "day", "week"
-- or "month". Calculator weeks start Monday; edge buckets clip to the requested range.
data StatsBucket = DayBucket | WeekBucket | MonthBucket
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON StatsBucket where toJSON = String . toUrlPiece

instance FromJSON StatsBucket where parseJSON = withText "StatsBucket" (either (fail . T.unpack) pure . parseUrlPiece)

instance FromHttpApiData StatsBucket where
  parseUrlPiece "day" = Right DayBucket
  parseUrlPiece "week" = Right WeekBucket
  parseUrlPiece "month" = Right MonthBucket
  parseUrlPiece _ = Left "Bucket must be day, week or month"

instance ToHttpApiData StatsBucket where
  toUrlPiece DayBucket = "day"
  toUrlPiece WeekBucket = "week"
  toUrlPiece MonthBucket = "month"

instance ToSchema StatsBucket where
  declareNamedSchema _ = pure $ enumSchema "StatsBucket" ["day", "week", "month"]

instance ToParamSchema StatsBucket where
  toParamSchema _ = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaEnum = Just [String "day", String "week", String "month"]}

-- | Required strict from/to dates plus optional bucket and subject UUID. Parsing
-- an individual date does not check range ordering or authorize the chosen user.
type StatisticsEndpoint =
  "statistics"
    :> QueryParam' '[Required, Strict] "from" StatsDate
    :> QueryParam' '[Required, Strict] "to" StatsDate
    :> QueryParam "bucket" StatsBucket
    :> QueryParam "userId" UserId
    :> Get '[JSON] SpendingStatistics

-- | Inclusive requested date range and one subject's statistics. All fields are
-- required and non-null JSON values; participants are public identity projections.
data SpendingStatistics = SpendingStatistics
  { groupId :: !GroupId,
    -- | Inclusive first accounting day, not a server timestamp.
    from :: !Day,
    -- | Inclusive last accounting day.
    to :: !Day,
    bucket :: !StatsBucket,
    -- | Subject of paid\/share\/top-expense metrics, not necessarily every participant.
    user :: !User,
    -- | Identities included in per-member comparisons.
    participants :: ![User],
    -- | Independent settlement-currency sections; do not sum differing currencies.
    currencies :: ![CurrencySpending]
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Group-wide gross expense/income totals plus the subject's paid/share metrics.
-- Each amount is in currency minor units; decoding does not verify their equations.
data CurrencySpending = CurrencySpending
  { currency :: !CurrencyInfo,
    -- | Gross expense total for the group, not the subject's allocated share.
    totalMinor :: !MinorAmount,
    -- | Number of expense records, not number of split participants.
    expenseCount :: !Int,
    -- | Gross group income; it does not reduce expense paid/share measures.
    incomeMinor :: !MinorAmount,
    -- | Number of income records in the range.
    incomeCount :: !Int,
    -- | Group income minus gross expenses; this may be negative.
    netMinor :: !MinorAmount,
    -- | Gross expenses paid by the selected subject.
    paidMinor :: !MinorAmount,
    -- | Expense allocations assigned to the selected subject.
    shareMinor :: !MinorAmount,
    -- | Inclusive calendar intervals, including zero-activity buckets.
    buckets :: ![SpendingBucket],
    -- | Per-participant expense paid/share totals, not income entitlements.
    members :: ![MemberSpending],
    -- | Calculator returns at most five positive shares, by descending amount/date, then UUID.
    topExpenses :: ![PersonalExpense],
    -- | Subject share omitted from topExpenses, not the group's remaining gross total.
    otherExpensesMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | One inclusive interval with the same group/subject metric meanings as
-- CurrencySpending. start/end may be clipped portions of a week or month.
data SpendingBucket = SpendingBucket
  { start :: !Day,
    end :: !Day,
    totalMinor :: !MinorAmount,
    expenseCount :: !Int,
    incomeMinor :: !MinorAmount,
    incomeCount :: !Int,
    netMinor :: !MinorAmount,
    paidMinor :: !MinorAmount,
    shareMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Public member identity with gross expenses advanced and allocated expenses
-- borne in this currency. These are not net settlement balances.
data MemberSpending = MemberSpending
  { user :: !User,
    paidMinor :: !MinorAmount,
    shareMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Ranked expense entry whose amount is the subject's saved settlement share,
-- not the record's gross amount or its original-currency amount.
data PersonalExpense = PersonalExpense
  { recordId :: !RecordId,
    title :: !Text,
    date :: !Day,
    amountMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
