{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Admin.ReceiptAI.Usage
  ( ReceiptAIUsageAPI, receiptAIUsageServer, parseReceiptAIUsageFilters ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), addDays, defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import qualified Data.UUID as UUID
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.API.Types.Admin (AuthAdmin (..))
import Jizhang.API.Types.Common (PageInfo (PageInfo), PaginatedResponse (PaginatedResponse))
import Jizhang.API.Types.ReceiptAIUsage (ReceiptAIUsageSummary (ReceiptAIUsageSummary), ReceiptAIUsageDaily (ReceiptAIUsageDaily), ReceiptAIUsageModel (ReceiptAIUsageModel), ReceiptAICall (ReceiptAICall), ReceiptAIUsageReport (ReceiptAIUsageReport))
import qualified Jizhang.Database.ReceiptAI as D
import qualified Jizhang.Database.Schema as S
import Jizhang.RequestProtection (Principal (AdminPrincipal))
import Servant

type ReceiptAIUsageAPI = "admin" :> "receipt-ai" :> "usage"
  :> QueryParam "from" Text :> QueryParam "to" Text
  :> QueryParam "groupId" Text :> QueryParam "status" Text
  :> QueryParam "offset" Text :> QueryParam "limit" Text
  :> Get '[JSON] ReceiptAIUsageReport

parseReceiptAIUsageFilters :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either Text (D.ReceiptAiUsageFilter, Int, Int)
parseReceiptAIUsageFilters from to group status offset limit = do
  fromDay <- traverse parseDay from
  toDay <- traverse parseDay to
  when (maybe False (\start -> maybe False (< start) toDay) fromDay) $ Left "from must not be after to"
  groupId <- traverse (maybe (Left "Invalid groupId") Right . UUID.fromText) group
  statusValue <- traverse parseStatus status
  offsetValue <- maybe (Right 0) (parseBound "offset" 0 (toInteger (maxBound :: Int))) offset
  limitValue <- maybe (Right 20) (parseBound "limit" 1 100) limit
  pure (D.ReceiptAiUsageFilter (fmap midnight fromDay) (fmap (midnight . addDays 1) toDay) groupId statusValue, offsetValue, limitValue)
  where
    midnight day = UTCTime day 0
    parseDay value
      | T.length value /= 10 = Left "Dates must be YYYY-MM-DD"
      | otherwise = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack value) :: Maybe Day of
          Just day | T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day) == value -> Right day
          _ -> Left "Dates must be YYYY-MM-DD"
    parseStatus value
      | value `elem` ["in_flight", "completed", "invalid_response", "refused", "http_error", "transport_error", "timeout", "interrupted"] = Right value
      | otherwise = Left "Invalid status"
    parseBound name lower upper value
      | T.null value || T.any (\c -> c < '0' || c > '9') value = Left $ "Invalid " <> name
      | T.length significant > length (show upper) = Left $ "Invalid " <> name
      | otherwise = let number = T.foldl' (\n c -> n * 10 + toInteger (fromEnum c - fromEnum '0')) 0 significant
                     in if number < lower || number > upper then Left ("Invalid " <> name) else Right (fromInteger number)
      where significant = T.dropWhile (== '0') value

receiptAIUsageServer :: AuthAdmin -> MyServer ReceiptAIUsageAPI
receiptAIUsageServer admin from to group status offset limit = do
  (filters, pageOffset, pageLimit) <- either (throwError . badRequest) pure $ parseReceiptAIUsageFilters from to group status offset limit
  enforceExpensiveLimit $ AdminPrincipal $ authAdminId admin
  now <- liftIO getCurrentTime
  (totals, days, models, rows) <- runDBPrivateTransaction $ do
    totals <- D.getReceiptAiUsageTotals now filters
    days <- D.getReceiptAiUsageDaily now filters
    models <- D.getReceiptAiUsageModels now filters
    rows <- D.getReceiptAiUsagePublicPage now filters pageOffset pageLimit
    pure (totals, days, models, rows)
  let summaryValue = summarize $ fromMaybe zeroTotals totals
      count = case fromMaybe zeroTotals totals of ((n, _, _, _), _, _, _) -> truncate n :: Integer
  when (count > toInteger (maxBound :: Int)) $ throwError $ err500 {errBody = "Usage page count exceeds supported pagination range"}
  let page = PageInfo pageOffset pageLimit (fromInteger count)
        (toInteger pageOffset + toInteger pageLimit < count) (pageOffset > 0)
  pure $ ReceiptAIUsageReport summaryValue (map dailyRow days) (map modelRow models)
    (PaginatedResponse (map callRow rows) page Nothing Nothing)
  where
    badRequest message = err400 {errBody = LBS.pack $ T.unpack message}

zeroTotals :: D.ReceiptAiUsageTotals
zeroTotals = ((0,0,0,0),(0,0),(0,0,0,0),(0,0,0))

summarize :: D.ReceiptAiUsageTotals -> ReceiptAIUsageSummary
summarize ((calls, completed, failed, inFlight), (priced, unpriced), (input, cached, output, reasoning), (cost, duration, durations)) =
  ReceiptAIUsageSummary (decimal calls) (decimal completed) (decimal failed) (decimal inFlight)
    (decimal priced) (decimal unpriced) (decimal input) (decimal cached) (decimal output) (decimal reasoning)
    (decimal cost) (if durations == 0 then Nothing else Just (truncate duration `div` truncate durations))

decimal :: RealFrac a => a -> Text
decimal value = T.pack $ show (truncate value :: Integer)

exact :: Integral a => a -> Text
exact = T.pack . show . toInteger

dailyRow :: (Day, D.ReceiptAiUsageTotals) -> ReceiptAIUsageDaily
dailyRow (day, totals) = case summarize totals of
  ReceiptAIUsageSummary a b c d e f g h i j k l -> ReceiptAIUsageDaily day a b c d e f g h i j k l

modelRow :: (Text, D.ReceiptAiUsageTotals) -> ReceiptAIUsageModel
modelRow (model, totals) = case summarize totals of
  ReceiptAIUsageSummary a b c d e f g h i j k l -> ReceiptAIUsageModel model a b c d e f g h i j k l

callRow :: (S.ReceiptAiUsage, Maybe Text, Maybe Text, Maybe Text) -> ReceiptAICall
callRow (row, userLabel, adminLabel, groupLabel) = ReceiptAICall
  (S._receiptAiUsageId row) (S._receiptAiUsageStartedAt row) (S._receiptAiUsageFinishedAt row)
  (S.unGroupId $ S._receiptAiUsageGroup row) groupLabel actor (S._receiptAiUsagePrincipalKind row)
  (S._receiptAiUsageRequestedModel row) (S._receiptAiUsageActualModel row)
  (fromMaybe (S._receiptAiUsageRequestedModel row) $ S._receiptAiUsageActualModel row)
  (S._receiptAiUsageStatus row)
  (exact <$> S._receiptAiUsageInputTokens row) (exact <$> S._receiptAiUsageCachedInputTokens row)
  (exact <$> S._receiptAiUsageOutputTokens row) (exact <$> S._receiptAiUsageReasoningTokens row)
  (toInteger <$> S._receiptAiUsageDurationMs row) (exact <$> S._receiptAiUsageEstimatedCostNanoUsd row)
  where
    actor = if S._receiptAiUsagePrincipalKind row == "admin"
      then fromMaybe "Deleted admin" adminLabel else fromMaybe "Deleted user" userLabel
