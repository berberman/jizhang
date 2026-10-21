{-# LANGUAGE OverloadedStrings #-}

-- | Persist resolved ledger snapshots, including original and settlement shares.
-- Callers own authorization, monetary validation, currency locking and transactions.
module Jizhang.Database.Record where

import Control.Monad (forM, forM_, unless)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList, runUpdateReturningList)
import Database.Beam.Postgres
import Jizhang.Database.Schema

data SplitRecordKind = ExpenseKind | IncomeKind
  deriving (Eq, Show)

splitRecordKindText :: SplitRecordKind -> Text
splitRecordKindText ExpenseKind = "expense"
splitRecordKindText IncomeKind = "income"

-- | Validated persistence values. Arithmetic and currency policy validation happen
-- at the API boundary; no monetary calculation is performed while writing.
data ResolvedSplitRecord = ResolvedSplitRecord
  { resolvedTitle :: Text,
    resolvedNote :: Maybe Text,
    resolvedAmountMinor :: Int64,
    resolvedCurrency :: Text,
    resolvedSettlementCurrency :: Text,
    resolvedSettlementAmountMinor :: Int64,
    resolvedByUserId :: UUID,
    resolvedDate :: Day,
    resolvedSplitMode :: Text,
    resolvedExchangeRate :: Maybe Scientific,
    resolvedRateSource :: Maybe Text,
    resolvedRateDate :: Maybe Day,
    resolvedRateFetchedAt :: Maybe UTCTime,
    resolvedExchangeQuoteId :: Maybe UUID,
    resolvedSplits :: [ResolvedSplit]
  }
  deriving (Eq, Show)

-- | Save both the split input and its allocated minor units. Reads use these
-- historical amounts rather than reallocating against current members or rates.
data ResolvedSplit = ResolvedSplit
  { resolvedSplitUserId :: UUID,
    resolvedSplitValue :: Int64,
    resolvedSplitAmountMinor :: Int64,
    resolvedSplitSettlementAmountMinor :: Int64
  }
  deriving (Eq, Show)

-- | Prevalidated member-to-member movement in one currency. Distinct endpoints,
-- positive amounts and group membership are not checked by these constructors.
data ResolvedTransfer = ResolvedTransfer
  { resolvedTransferAmountMinor :: Int64,
    resolvedTransferCurrency :: Text,
    resolvedTransferPaidBy :: UUID,
    resolvedTransferTo :: UUID,
    resolvedTransferDate :: Day,
    resolvedTransferNote :: Maybe Text
  }
  deriving (Eq, Show)

-- | Insert the header and supplied allocations without recalculating them.
-- Use one transaction so a failed split insert cannot leave a partial record.
insertSplitRecord :: SplitRecordKind -> UUID -> Maybe UUID -> ResolvedSplitRecord -> Pg (Record, [RecordSplit])
insertSplitRecord kind groupId receiptId expense = do
  recordId <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let record =
        Record
          { _recordId = recordId,
            _recordGroup = GroupId groupId,
            _recordKind = splitRecordKindText kind,
            _title = resolvedTitle expense,
            _recordNote = resolvedNote expense,
            _amountMinor = resolvedAmountMinor expense,
            _currency = resolvedCurrency expense,
            _settlementCurrency = resolvedSettlementCurrency expense,
            _settlementAmountMinor = resolvedSettlementAmountMinor expense,
            _splitMode = Just (resolvedSplitMode expense),
            _exchangeRate = resolvedExchangeRate expense,
            _rateSource = resolvedRateSource expense,
            _rateDate = resolvedRateDate expense,
            _rateFetchedAt = resolvedRateFetchedAt expense,
            _exchangeQuoteId = resolvedExchangeQuoteId expense,
            _paidBy = UserId (resolvedByUserId expense),
            _transferTo = nothing_,
            _date = resolvedDate expense,
            _createdAt = now,
            _recordReceipt = maybe nothing_ (just_ . ReceiptId) receiptId
          }
  inserted <- runInsertReturningList $ insert (_records jizhangDb) $ insertValues [record]
  case inserted of
    [storedRecord] -> do
      splits <- forM (resolvedSplits expense) (insertResolvedSplit recordId)
      pure (storedRecord, splits)
    _ -> liftIO $ ioError $ userError "Expected one inserted split record"

-- | Replace allocations only after matching group, UUID and split-record kind.
-- A mismatch is a no-op; atomic delete/reinsert requires the caller's transaction.
replaceSplitRecord :: SplitRecordKind -> UUID -> UUID -> ResolvedSplitRecord -> Pg ()
replaceSplitRecord kind groupId recordId expense = do
  updated <-
    runUpdateReturningList $
      update
        (_records jizhangDb)
        ( \record ->
            mconcat
              [ _title record <-. val_ (resolvedTitle expense),
                _recordNote record <-. val_ (resolvedNote expense),
                _amountMinor record <-. val_ (resolvedAmountMinor expense),
                _currency record <-. val_ (resolvedCurrency expense),
                _settlementCurrency record <-. val_ (resolvedSettlementCurrency expense),
                _settlementAmountMinor record <-. val_ (resolvedSettlementAmountMinor expense),
                _splitMode record <-. val_ (Just (resolvedSplitMode expense)),
                _exchangeRate record <-. val_ (resolvedExchangeRate expense),
                _rateSource record <-. val_ (resolvedRateSource expense),
                _rateDate record <-. val_ (resolvedRateDate expense),
                _rateFetchedAt record <-. val_ (resolvedRateFetchedAt expense),
                _exchangeQuoteId record <-. val_ (resolvedExchangeQuoteId expense),
                _paidBy record <-. val_ (UserId (resolvedByUserId expense)),
                _date record <-. val_ (resolvedDate expense)
              ]
        )
        ( \record ->
            _recordGroup record
              ==. val_ (GroupId groupId)
              &&. _recordId record
              ==. val_ recordId
              &&. _recordKind record
              ==. val_ (splitRecordKindText kind)
              &&. isNothing_ (unUserId (_transferTo record))
        )
  unless (null updated) $ do
    -- Only the scoped header update authorizes deleting shares; a foreign UUID must leave them intact.
    deleteRecordSplitsForRecord recordId
    forM_ (resolvedSplits expense) (insertResolvedSplit recordId)

-- | Store equal original/settlement amounts, no conversion snapshot and no splits.
-- Caller validation must ensure the transfer fits the group's currency policy.
insertTransferRecord :: UUID -> ResolvedTransfer -> Pg Record
insertTransferRecord groupId transfer = do
  recordId <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let record =
        Record
          { _recordId = recordId,
            _recordGroup = GroupId groupId,
            _recordKind = "transfer",
            _title = "Transfer",
            _recordNote = resolvedTransferNote transfer,
            _amountMinor = resolvedTransferAmountMinor transfer,
            _currency = resolvedTransferCurrency transfer,
            _settlementCurrency = resolvedTransferCurrency transfer,
            _settlementAmountMinor = resolvedTransferAmountMinor transfer,
            _splitMode = Nothing,
            _exchangeRate = Nothing,
            _rateSource = Nothing,
            _rateDate = Nothing,
            _rateFetchedAt = Nothing,
            _exchangeQuoteId = Nothing,
            _paidBy = UserId (resolvedTransferPaidBy transfer),
            _transferTo = just_ (UserId (resolvedTransferTo transfer)),
            _date = resolvedTransferDate transfer,
            _createdAt = now,
            _recordReceipt = nothing_
          }
  inserted <- runInsertReturningList $ insert (_records jizhangDb) $ insertValues [record]
  case inserted of
    [storedRecord] -> pure storedRecord
    _ -> liftIO $ ioError $ userError "Expected one inserted transfer record"

-- | Update only a transfer in the supplied group, preserving UUID and creation
-- time. A missing row or a split-record UUID produces no update.
replaceTransferRecord :: UUID -> UUID -> ResolvedTransfer -> Pg ()
replaceTransferRecord groupId recordId transfer =
  runUpdate $
    update
      (_records jizhangDb)
      ( \record ->
          mconcat
            [ _amountMinor record <-. val_ (resolvedTransferAmountMinor transfer),
              _recordNote record <-. val_ (resolvedTransferNote transfer),
              _currency record <-. val_ (resolvedTransferCurrency transfer),
              _settlementCurrency record <-. val_ (resolvedTransferCurrency transfer),
              _settlementAmountMinor record <-. val_ (resolvedTransferAmountMinor transfer),
              _paidBy record <-. val_ (UserId (resolvedTransferPaidBy transfer)),
              _transferTo record <-. val_ (just_ (UserId (resolvedTransferTo transfer))),
              _date record <-. val_ (resolvedTransferDate transfer)
            ]
      )
      ( \record ->
          _recordGroup record
            ==. val_ (GroupId groupId)
            &&. _recordId record
            ==. val_ recordId
            &&. _recordKind record
            ==. val_ "transfer"
            &&. isJust_ (unUserId (_transferTo record))
      )

-- | Store one already-resolved share; uniqueness is by record and user.
-- This low-level helper neither checks totals nor verifies group membership.
insertResolvedSplit :: UUID -> ResolvedSplit -> Pg RecordSplit
insertResolvedSplit recordId resolved = do
  -- Preserve both rounded allocations independently; converting a saved original share can round differently.
  let split =
        RecordSplit
          (RecordId recordId)
          (UserId (resolvedSplitUserId resolved))
          (resolvedSplitValue resolved)
          (resolvedSplitAmountMinor resolved)
          (resolvedSplitSettlementAmountMinor resolved)
  runInsert $ insert (_recordSplits jizhangDb) $ insertValues [split]
  pure split

deleteRecord :: UUID -> UUID -> Pg ()
deleteRecord groupId recordId = runDelete $ delete (_records jizhangDb) (\record -> _recordGroup record ==. val_ (GroupId groupId) &&. _recordId record ==. val_ recordId)

checkRecordExists :: UUID -> UUID -> Pg Bool
checkRecordExists groupId recordId =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          record <- all_ (_records jizhangDb)
          guard_ (_recordGroup record ==. val_ (GroupId groupId) &&. _recordId record ==. val_ recordId)
          pure (_recordId record)
      )

-- | Unscoped allocation deletion for an already-authorized record. Resolve its
-- group first when accepting an external UUID; this helper cannot enforce scope.
deleteRecordSplitsForRecord :: UUID -> Pg ()
deleteRecordSplitsForRecord recordId = runDelete $ delete (_recordSplits jizhangDb) (\rs -> _rsRecord rs ==. val_ (RecordId recordId))

-- | Resolve the group-scoped header before reading its allocations. The two
-- statements need a suitable transaction if callers require one stable snapshot.
getRecordWithSplits :: UUID -> UUID -> Pg (Maybe (Record, [RecordSplit]))
getRecordWithSplits groupId recordId = do
  record <- runSelectReturningOne $ select $ do
    r <- all_ (_records jizhangDb)
    guard_ (_recordGroup r ==. val_ (GroupId groupId) &&. _recordId r ==. val_ recordId)
    pure r
  case record of
    Just rec -> do
      splits <- getRecordSplitsForRecord recordId
      pure $ Just (rec, splits)
    Nothing -> pure Nothing

getRecordSplitsForRecord :: UUID -> Pg [RecordSplit]
getRecordSplitsForRecord recordId = runSelectReturningList $ select $ do
  rs <- all_ (_recordSplits jizhangDb)
  guard_ (_rsRecord rs ==. val_ (RecordId recordId))
  pure rs

-- | Flatten records and allocations, retaining headers with no splits. Consumers
-- must regroup repeated headers rather than counting every result as a record.
getRecordsWithSplitsForGroup :: UUID -> Pg [(Record, Maybe RecordSplit)]
getRecordsWithSplitsForGroup groupId = runSelectReturningList $ select $ do
  record <- all_ (_records jizhangDb)
  guard_ (_recordGroup record ==. val_ (GroupId groupId))
  splits <- leftJoin_ (all_ (_recordSplits jizhangDb)) (\rs -> _rsRecord rs ==. primaryKey record)
  pure (record, splits)

-- | Inclusive business-date range of expenses/income, excluding transfers.
-- Returns saved shares via a left join, not allocations recomputed from policy.
getSplitRecordsWithSplitsInRange :: UUID -> Day -> Day -> Pg [(Record, Maybe RecordSplit)]
getSplitRecordsWithSplitsInRange groupId from to = runSelectReturningList $ select $ do
  record <- all_ (_records jizhangDb)
  guard_ (_recordGroup record ==. val_ (GroupId groupId))
  guard_ (_date record >=. val_ from &&. _date record <=. val_ to)
  guard_ (_recordKind record ==. val_ "expense" ||. _recordKind record ==. val_ "income")
  guard_ (isNothing_ (unUserId $ _transferTo record))
  splits <- leftJoin_ (all_ (_recordSplits jizhangDb)) (\rs -> _rsRecord rs ==. primaryKey record)
  pure (record, splits)
