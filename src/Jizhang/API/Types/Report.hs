{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | All-time balances and suggested payments, partitioned by settlement currency.
-- JSON uses literal keys and exact integer strings; decoding does not recompute totals.
-- Every field is required and non-null; generic decoders ignore extra keys.
module Jizhang.API.Types.Report where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Money
import Jizhang.API.Types.Record
import Jizhang.API.Types.User

-- | Positive amounts are owed to the user; negative amounts are owed by them.
data BalanceBreakdown = BalanceBreakdown
  { recordId :: !RecordId,
    title :: !Text,
    date :: !Day,
    -- | Calculator emits "expense", "income" or "transfer"; the codec accepts any text.
    recordType :: !Text,
    -- | Gross record amount in this report section's currency, counted once.
    recordAmountMinor :: !MinorAmount,
    -- | This user's net contribution from the record, after combining their roles.
    amountMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | One user's position in a currency. Positive totalMinor means they are owed;
-- it equals paid - expense share - received income + entitlement + sent - received payments.
data Balance = Balance
  { -- | Public identity only; no email or external identity is exposed.
    user :: !User,
    totalMinor :: !MinorAmount,
    -- | Gross expenses advanced by this user.
    expensesPaidMinor :: !MinorAmount,
    -- | Allocated expenses borne by this user.
    expenseShareMinor :: !MinorAmount,
    -- | Gross group income collected by this user.
    incomeReceivedMinor :: !MinorAmount,
    -- | Allocated income belonging to this user.
    incomeEntitlementMinor :: !MinorAmount,
    -- | Payments this user has sent, increasing their net position.
    paymentsSentMinor :: !MinorAmount,
    -- | Payments this user has received, decreasing their net position.
    paymentsReceivedMinor :: !MinorAmount,
    -- | Per-record impacts; not original-currency record amounts.
    breakdown :: ![BalanceBreakdown]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Suggested payment from debtor to creditor, in the containing currency's
-- minor units. The decoder does not enforce a positive amount or distinct users.
data Settlement = Settlement
  { fromUser :: !User,
    toUser :: !User,
    -- | Suggested payment magnitude, not a signed balance delta.
    amountMinor :: !MinorAmount
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Each currency settles independently; amounts never cross section boundaries.
data CurrencyReport = CurrencyReport
  { currency :: !CurrencyInfo,
    balances :: ![Balance],
    settlements :: ![Settlement]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Group report containing independent currency sections rather than a single
-- converted grand total. No cross-currency sum is represented by this object.
data Report = Report
  { groupId :: !GroupId,
    currencies :: ![CurrencyReport]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
