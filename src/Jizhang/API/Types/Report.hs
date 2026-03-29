{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.Report where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jizhang.API.Types.Group
import Jizhang.API.Types.Record
import Jizhang.API.Types.User

-- | The record that contributes to a user's balance, with the amount they owe or are owed
data BalanceBreakdown = BalanceBreakdown
  { recordId :: !RecordId,
    title :: !Text,
    amount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The balance for a user, including total amount owed/owing and breakdown by record
data Balance = Balance
  { user :: !User,
    totalAmount :: !Double,
    breakdown :: ![BalanceBreakdown]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A settlement that a user should make to another user to settle their balances
data Settlement = Settlement
  { fromUser :: !User,
    toUser :: !User,
    amount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The report for a group, including balances for each user and suggested settlements
data Report = Report
  { groupId :: !GroupId,
    balances :: ![Balance],
    settlements :: ![Settlement]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
