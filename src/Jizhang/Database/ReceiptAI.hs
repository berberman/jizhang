{-# LANGUAGE OverloadedStrings #-}

module Jizhang.Database.ReceiptAI where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime, addUTCTime)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runUpdateReturningList)
import Database.Beam.Postgres
import Database.Beam.Query.Internal (QAgg)
import qualified Jizhang.Database.Schema as S

getReceiptAiSettings :: Pg S.ReceiptAiSettings
getReceiptAiSettings = do
  row <- runSelectReturningOne $ select $ do
    settings <- all_ (S._receiptAiSettings S.jizhangDb)
    guard_ (S._receiptAiSettingsId settings ==. val_ 1)
    pure settings
  case row of
    Just settings -> pure settings
    Nothing -> liftIO $ ioError $ userError "Private data persistence failed"

-- A stale revision makes no changes, including to the retained encrypted key.
saveReceiptAiSettings :: Int64 -> Bool -> Maybe Text -> Pg (Maybe S.ReceiptAiSettings)
saveReceiptAiSettings revision enabled encryptedKey = do
  rows <-
    runUpdateReturningList $
      update
        (S._receiptAiSettings S.jizhangDb)
        ( \row ->
            mconcat
              [ S._receiptAiSettingsRevision row <-. val_ (revision + 1),
                S._receiptAiSettingsEnabled row <-. val_ enabled,
                S._receiptAiSettingsApiKeyEncrypted row <-. val_ encryptedKey
              ]
        )
        (\row -> S._receiptAiSettingsId row ==. val_ 1 &&. S._receiptAiSettingsRevision row ==. val_ revision)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

insertReceiptAiUsage :: S.ReceiptAiUsage -> Pg ()
insertReceiptAiUsage row = runInsert $ insert (S._receiptAiUsage S.jizhangDb) (insertValues [row])

getReceiptAiUsage :: UUID -> Pg (Maybe S.ReceiptAiUsage)
getReceiptAiUsage rowId = runSelectReturningOne $ select $ do
  row <- all_ (S._receiptAiUsage S.jizhangDb)
  guard_ (S._receiptAiUsageId row ==. val_ rowId)
  pure row

-- Only terminal response metadata changes. Identity, provenance and pricing
-- snapshots remain those recorded before the single outbound attempt.
finishReceiptAiUsage :: S.ReceiptAiUsage -> Pg Bool
finishReceiptAiUsage terminal
  | S._receiptAiUsageStatus terminal == "in_flight" = pure False
  | otherwise = do
      rows <-
        runUpdateReturningList $
          update
            (S._receiptAiUsage S.jizhangDb)
            ( \row ->
                mconcat
                  [ S._receiptAiUsageFinishedAt row <-. val_ (S._receiptAiUsageFinishedAt terminal),
                    S._receiptAiUsageActualModel row <-. val_ (S._receiptAiUsageActualModel terminal),
                    S._receiptAiUsageEvidenceSha256 row <-. val_ (S._receiptAiUsageEvidenceSha256 terminal),
                    S._receiptAiUsageStatus row <-. val_ (S._receiptAiUsageStatus terminal),
                    S._receiptAiUsageProviderRequestId row <-. val_ (S._receiptAiUsageProviderRequestId terminal),
                    S._receiptAiUsageHttpStatus row <-. val_ (S._receiptAiUsageHttpStatus terminal),
                    S._receiptAiUsageInputTokens row <-. val_ (S._receiptAiUsageInputTokens terminal),
                    S._receiptAiUsageCachedInputTokens row <-. val_ (S._receiptAiUsageCachedInputTokens terminal),
                    S._receiptAiUsageOutputTokens row <-. val_ (S._receiptAiUsageOutputTokens terminal),
                    S._receiptAiUsageReasoningTokens row <-. val_ (S._receiptAiUsageReasoningTokens terminal),
                    S._receiptAiUsageDurationMs row <-. val_ (S._receiptAiUsageDurationMs terminal),
                    S._receiptAiUsageEstimatedCostNanoUsd row <-. val_ (S._receiptAiUsageEstimatedCostNanoUsd terminal)
                  ]
            )
            ( \row ->
                S._receiptAiUsageId row
                  ==. val_ (S._receiptAiUsageId terminal)
                  &&. S._receiptAiUsageStatus row
                  ==. val_ "in_flight"
            )
      pure $ not $ null rows

validateReceiptExtractionSource :: UUID -> UUID -> Text -> Text -> Pg Bool
validateReceiptExtractionSource groupId extractionId imageHash evidenceHash = do
  row <- runSelectReturningOne $ select $ do
    usage <- all_ (S._receiptAiUsage S.jizhangDb)
    guard_
      ( S._receiptAiUsageId usage
          ==. val_ extractionId
          &&. S._receiptAiUsageGroup usage
          ==. val_ (S.GroupId $ Just groupId)
          &&. S._receiptAiUsageStatus usage
          ==. val_ "completed"
          &&. S._receiptAiUsageImageSha256 usage
          ==. val_ imageHash
          &&. S._receiptAiUsageEvidenceSha256 usage
          ==. val_ (Just evidenceHash)
      )
    pure (S._receiptAiUsageId usage)
  pure $ case row of
    Just _ -> True
    Nothing -> False

data ReceiptAiUsageFilter = ReceiptAiUsageFilter
  { receiptAiUsageFrom :: Maybe UTCTime,
    receiptAiUsageBefore :: Maybe UTCTime,
    receiptAiUsageGroupId :: Maybe UUID,
    receiptAiUsageStatus :: Maybe Text
  }

-- Use this same SQL projection for filters, aggregates and returned calls.
-- Abandoned attempts are not rewritten or replayed when reports are read.
effectiveReceiptAiUsage :: UTCTime -> S.ReceiptAiUsageT (QExpr Postgres s) -> S.ReceiptAiUsageT (QExpr Postgres s)
effectiveReceiptAiUsage now row =
  row
    { S._receiptAiUsageStatus = ifThenElse_ stale (val_ "interrupted") (S._receiptAiUsageStatus row),
      S._receiptAiUsageInputTokens = ifThenElse_ stale nothing_ (S._receiptAiUsageInputTokens row),
      S._receiptAiUsageCachedInputTokens = ifThenElse_ stale nothing_ (S._receiptAiUsageCachedInputTokens row),
      S._receiptAiUsageOutputTokens = ifThenElse_ stale nothing_ (S._receiptAiUsageOutputTokens row),
      S._receiptAiUsageReasoningTokens = ifThenElse_ stale nothing_ (S._receiptAiUsageReasoningTokens row),
      S._receiptAiUsageDurationMs = ifThenElse_ stale nothing_ (S._receiptAiUsageDurationMs row),
      S._receiptAiUsageEstimatedCostNanoUsd = ifThenElse_ stale nothing_ (S._receiptAiUsageEstimatedCostNanoUsd row)
    }
  where
    stale =
      S._receiptAiUsageStatus row
        ==. val_ "in_flight"
        &&. S._receiptAiUsageStartedAt row
        <. val_ (addUTCTime (-300) now)

receiptAiUsageQuery :: UTCTime -> ReceiptAiUsageFilter -> Q Postgres S.JizhangDb s (S.ReceiptAiUsageT (QExpr Postgres s))
receiptAiUsageQuery now filters = do
  stored <- all_ (S._receiptAiUsage S.jizhangDb)
  let row = effectiveReceiptAiUsage now stored
  maybe (pure ()) (\from -> guard_ $ S._receiptAiUsageStartedAt row >=. val_ from) (receiptAiUsageFrom filters)
  maybe (pure ()) (\before -> guard_ $ S._receiptAiUsageStartedAt row <. val_ before) (receiptAiUsageBefore filters)
  maybe (pure ()) (\groupId -> guard_ $ S._receiptAiUsageGroup row ==. val_ (S.GroupId $ Just groupId)) (receiptAiUsageGroupId filters)
  maybe (pure ()) (\status -> guard_ $ S._receiptAiUsageStatus row ==. val_ status) (receiptAiUsageStatus filters)
  pure row

getReceiptAiUsagePage :: UTCTime -> ReceiptAiUsageFilter -> Int -> Int -> Pg [S.ReceiptAiUsage]
getReceiptAiUsagePage now filters offset limit =
  runSelectReturningList $
    select $
      limit_ (fromIntegral limit) $
        offset_ (fromIntegral offset) $
          orderBy_ (\row -> (desc_ $ S._receiptAiUsageStartedAt row, asc_ $ S._receiptAiUsageId row)) $
            receiptAiUsageQuery now filters

-- PostgreSQL NUMERIC sums are decoded exactly; no BIGINT accumulator can overflow.
type ReceiptAiUsageTotalsF a = ((a, a, a, a), (a, a), (a, a, a, a), (a, a, a))

type ReceiptAiUsageTotals = ReceiptAiUsageTotalsF Scientific

numericSum :: QExpr Postgres s (Maybe Int64) -> QAgg Postgres s Scientific
numericSum = customExpr_ (\value -> "COALESCE(SUM(CAST(" <> value <> " AS NUMERIC)),0)")

numericCount :: QExpr Postgres s Bool -> QAgg Postgres s Scientific
numericCount = customExpr_ (\condition -> "COALESCE(SUM(CASE WHEN " <> condition <> " THEN 1::numeric ELSE 0::numeric END),0)")

usageTotals :: S.ReceiptAiUsageT (QExpr Postgres s) -> ReceiptAiUsageTotalsF (QAgg Postgres s Scientific)
usageTotals row =
  ( ( numericCount (val_ True),
      numericCount (status ==. val_ "completed"),
      numericCount (status /=. val_ "completed" &&. status /=. val_ "in_flight"),
      numericCount (status ==. val_ "in_flight")
    ),
    (numericCount (isJust_ cost), numericCount (isNothing_ cost)),
    ( numericSum (S._receiptAiUsageInputTokens row),
      numericSum (S._receiptAiUsageCachedInputTokens row),
      numericSum (S._receiptAiUsageOutputTokens row),
      numericSum (S._receiptAiUsageReasoningTokens row)
    ),
    (numericSum cost, numericSum duration, numericCount (isJust_ duration))
  )
  where
    status = S._receiptAiUsageStatus row
    cost = S._receiptAiUsageEstimatedCostNanoUsd row
    duration = ifThenElse_ (isJust_ $ S._receiptAiUsageFinishedAt row) (S._receiptAiUsageDurationMs row) nothing_

getReceiptAiUsageTotals :: UTCTime -> ReceiptAiUsageFilter -> Pg (Maybe ReceiptAiUsageTotals)
getReceiptAiUsageTotals now filters =
  runSelectReturningOne $
    select $
      aggregate_ usageTotals (receiptAiUsageQuery now filters)

usageDay :: QExpr Postgres s UTCTime -> QExpr Postgres s Day
usageDay = customExpr_ (\value -> "CAST((" <> value <> " AT TIME ZONE 'UTC') AS DATE)")

getReceiptAiUsageDaily :: UTCTime -> ReceiptAiUsageFilter -> Pg [(Day, ReceiptAiUsageTotals)]
getReceiptAiUsageDaily now filters =
  runSelectReturningList $
    select $
      orderBy_ (asc_ . fst) $
        aggregate_ (\row -> (group_ $ usageDay $ S._receiptAiUsageStartedAt row, usageTotals row)) (receiptAiUsageQuery now filters)

getReceiptAiUsageModels :: UTCTime -> ReceiptAiUsageFilter -> Pg [(Text, ReceiptAiUsageTotals)]
getReceiptAiUsageModels now filters =
  runSelectReturningList $
    select $
      orderBy_ (asc_ . fst) $
        aggregate_ (\row -> (group_ $ fromMaybe_ (S._receiptAiUsageRequestedModel row) (S._receiptAiUsageActualModel row), usageTotals row)) (receiptAiUsageQuery now filters)

getReceiptAiUsagePublicPage :: UTCTime -> ReceiptAiUsageFilter -> Int -> Int -> Pg [(S.ReceiptAiUsage, Maybe Text, Maybe Text, Maybe Text)]
getReceiptAiUsagePublicPage now filters offset limit = runSelectReturningList $
  select $
    orderBy_ (\(row, _, _, _) -> (desc_ $ S._receiptAiUsageStartedAt row, asc_ $ S._receiptAiUsageId row)) $ do
      row <-
        limit_ (fromIntegral limit) $
          offset_ (fromIntegral offset) $
            orderBy_ (\usage -> (desc_ $ S._receiptAiUsageStartedAt usage, asc_ $ S._receiptAiUsageId usage)) $
              receiptAiUsageQuery now filters
      user <- leftJoin_ (all_ $ S._users S.jizhangDb) (\user -> S._receiptAiUsageUser row ==. S.UserId (just_ $ S._userId user))
      admin <- leftJoin_ (all_ $ S._admins S.jizhangDb) (\admin -> S._receiptAiUsageAdmin row ==. S.AdminId (just_ $ S._adminId admin))
      group <- leftJoin_ (all_ $ S._groups S.jizhangDb) (\group -> S._receiptAiUsageGroup row ==. S.GroupId (just_ $ S._groupId group))
      pure (row, S._displayName user, S._adminUsername admin, S._groupName group)
