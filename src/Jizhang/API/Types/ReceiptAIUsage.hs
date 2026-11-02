{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Jizhang.API.Types.ReceiptAIUsage where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Common (PaginatedResponse)

-- Decimal strings keep aggregate and per-call counters exact in browser clients.
data ReceiptAIUsageSummary = ReceiptAIUsageSummary
  { calls :: !Text,
    completed :: !Text,
    failed :: !Text,
    inFlight :: !Text,
    pricedCalls :: !Text,
    unpricedCalls :: !Text,
    inputTokens :: !Text,
    cachedInputTokens :: !Text,
    outputTokens :: !Text,
    reasoningTokens :: !Text,
    estimatedCostNanoUsd :: !Text,
    averageDurationMs :: !(Maybe Integer)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReceiptAIUsageDaily = ReceiptAIUsageDaily
  { day :: !Day,
    calls :: !Text,
    completed :: !Text,
    failed :: !Text,
    inFlight :: !Text,
    pricedCalls :: !Text,
    unpricedCalls :: !Text,
    inputTokens :: !Text,
    cachedInputTokens :: !Text,
    outputTokens :: !Text,
    reasoningTokens :: !Text,
    estimatedCostNanoUsd :: !Text,
    averageDurationMs :: !(Maybe Integer)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReceiptAIUsageModel = ReceiptAIUsageModel
  { model :: !Text,
    calls :: !Text,
    completed :: !Text,
    failed :: !Text,
    inFlight :: !Text,
    pricedCalls :: !Text,
    unpricedCalls :: !Text,
    inputTokens :: !Text,
    cachedInputTokens :: !Text,
    outputTokens :: !Text,
    reasoningTokens :: !Text,
    estimatedCostNanoUsd :: !Text,
    averageDurationMs :: !(Maybe Integer)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReceiptAICall = ReceiptAICall
  { id :: !UUID,
    startedAt :: !UTCTime,
    finishedAt :: !(Maybe UTCTime),
    groupId :: !(Maybe UUID),
    groupName :: !(Maybe Text),
    actorLabel :: !Text,
    actorKind :: !Text,
    requestedModel :: !Text,
    actualModel :: !(Maybe Text),
    model :: !Text,
    status :: !Text,
    inputTokens :: !(Maybe Text),
    cachedInputTokens :: !(Maybe Text),
    outputTokens :: !(Maybe Text),
    reasoningTokens :: !(Maybe Text),
    durationMs :: !(Maybe Integer),
    estimatedCostNanoUsd :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReceiptAIUsageReport = ReceiptAIUsageReport
  { summary :: !ReceiptAIUsageSummary,
    daily :: ![ReceiptAIUsageDaily],
    byModel :: ![ReceiptAIUsageModel],
    calls :: !(PaginatedResponse ReceiptAICall)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
