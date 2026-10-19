{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Validate persisted ledger rows while projecting the public record variants.
-- Projection preserves saved allocations rather than recomputing historical shares.
module Jizhang.API.Record.Projection
  ( storedSplitMode,
    storedRecordMoney,
    recordToSplitRecord,
    recordToTransferRecord,
    recordToApiRecord
  )
where

import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.UUID (UUID)
import Jizhang.API.Types.Group (GroupId (GroupId))
import Jizhang.API.Types.Money (CurrencyCode (CurrencyCode), CurrencyInfo, ExchangeQuoteId (ExchangeQuoteId), ExchangeRate (ExchangeRate), MinorAmount (MinorAmount), SplitMode (..), minorAmountInteger)
import Jizhang.API.Types.Record (Record (..), RecordId (RecordId), RecordSplit (..))
import Jizhang.API.Types.User (User (User))
import Jizhang.API.User.Projection (UserMap, storedUser)
import Jizhang.Currency (CurrencyCatalog, lookupCurrency)
import qualified Jizhang.Database.Record as D (SplitRecordKind (..), splitRecordKindText)
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Money as Money

-- | Split records require a recognized stored mode; missing modes are corruption.
storedSplitMode :: Maybe Text -> Either Text SplitMode
storedSplitMode (Just "equal") = Right EqualSplit
storedSplitMode (Just "shares") = Right SharesSplit
storedSplitMode (Just "percentage") = Right PercentageSplit
storedSplitMode (Just "exact") = Right ExactSplit
storedSplitMode _ = Left "Invalid stored split mode"

-- | Validate currency metadata, rate provenance and the saved converted total together.
storedRecordMoney :: CurrencyCatalog -> S.Record -> Either Text (CurrencyInfo, CurrencyInfo, MinorAmount, MinorAmount, Maybe ExchangeRate)
storedRecordMoney catalog record = do
  original <- lookupCurrency catalog (CurrencyCode $ S._currency record)
  target <- lookupCurrency catalog (CurrencyCode $ S._settlementCurrency record)
  amount <- Money.validateRecordAmount (MinorAmount $ toInteger $ S._amountMinor record)
  settlement <- Money.validateRecordAmount (MinorAmount $ toInteger $ S._settlementAmountMinor record)
  -- Treat provenance as one tuple: accepting partial metadata would hide corrupt conversion history.
  fx <- case (S._exchangeRate record, S._rateSource record, S._rateDate record, S._rateFetchedAt record, S._exchangeQuoteId record) of
    (Nothing, Nothing, Nothing, Nothing, Nothing)
      | original == target && amount == settlement -> Right Nothing
    (Just rateNumber, Just "manual", Nothing, Nothing, Nothing)
      | original /= target -> do
          valid <- Money.decimalRateFromScientific rateNumber
          Right $ Just $ ExchangeRate "manual" valid Nothing Nothing Nothing
    (Just rateNumber, Just "reference", Just day, Just fetched, Just quote)
      | original /= target -> do
          valid <- Money.decimalRateFromScientific rateNumber
          Right $ Just $ ExchangeRate "reference" valid (Just day) (Just fetched) (Just $ ExchangeQuoteId quote)
    _ -> Left "Invalid stored exchange rate provenance"
  case fx of
    Nothing -> pure ()
    Just (ExchangeRate _ rateValue _ _ _) -> do
      expected <- Money.convertAmount original target rateValue amount
      unless (expected == settlement) $ Left "Stored converted amount does not match rate"
  pure (original, target, amount, settlement, fx)

-- | Require valid participants, split weights and matching original/settlement sums.
recordToSplitRecord :: D.SplitRecordKind -> CurrencyCatalog -> UserMap -> S.Record -> [S.RecordSplit] -> Either Text Record
recordToSplitRecord kind catalog users record ssplits = do
  (currency, settlementCurrency, amountMinor, settlementAmountMinor, exchangeRate) <- storedRecordMoney catalog record
  unless (S._recordKind record == D.splitRecordKindText kind && isNothing (S.unUserId $ S._transferTo record)) $
    Left "Invalid stored split record kind or recipient"
  splitMode <- storedSplitMode (S._splitMode record)
  actor <- storedUser users (S.unUserId $ S._paidBy record)
  splits <- traverse projectSplit ssplits
  let originalSum = sum [minorAmountInteger amount | RecordSplit _ _ amount _ <- splits]
      settlementSum = sum [minorAmountInteger amount | RecordSplit _ _ _ amount <- splits]
      rows = [(uid, weight) | RecordSplit (User uid _ _) weight _ _ <- splits]
  -- Validate weights and conservation, but keep the saved allocations rather than re-apportioning history.
  _ <- Money.allocateSplits splitMode amountMinor rows
  unless (originalSum == minorAmountInteger amountMinor && settlementSum == minorAmountInteger settlementAmountMinor) $
    Left "Stored split totals do not match record"
  let recordId = coerce $ S._recordId record
      title = S._title record
      note = S._recordNote record
      date = S._date record
      createdAt = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
  pure $ case kind of
    D.ExpenseKind -> ExpenseRecord {paidBy = actor, ..}
    D.IncomeKind -> IncomeRecord {receivedBy = actor, ..}
  where
    projectSplit :: S.RecordSplit -> Either Text RecordSplit
    projectSplit split = do
      user <- storedUser users (S.unUserId $ S._rsUser split)
      value <- Money.validateSplitAmount $ MinorAmount $ toInteger $ S._splitValue split
      amountMinor <- Money.validateSplitAmount $ MinorAmount $ toInteger $ S._splitAmountMinor split
      settlementAmountMinor <- Money.validateSplitAmount $ MinorAmount $ toInteger $ S._splitSettlementAmountMinor split
      pure RecordSplit {..}

-- | Require a transfer recipient and reject split-mode or exchange-rate metadata.
recordToTransferRecord :: CurrencyCatalog -> UserMap -> UUID -> S.Record -> Either Text Record
recordToTransferRecord catalog users recipient record = do
  (currency, settlementCurrency, amountMinor, settlementAmountMinor, exchangeRate) <- storedRecordMoney catalog record
  unless (S._recordKind record == "transfer" && isNothing exchangeRate && isNothing (S._splitMode record) && S.unUserId (S._transferTo record) == Just recipient) $
    Left "Invalid stored payment"
  paidBy <- storedUser users (S.unUserId $ S._paidBy record)
  transferTo <- storedUser users recipient
  let recordId = coerce $ S._recordId record
      title = S._title record
      note = S._recordNote record
      date = S._date record
      createdAt = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
  pure TransferRecord {..}

-- | Dispatch by stored kind, returning diagnostics rather than fabricating a variant.
recordToApiRecord :: CurrencyCatalog -> UserMap -> S.Record -> [S.RecordSplit] -> Either Text Record
recordToApiRecord catalog users record ssplits =
  case S._recordKind record of
    "expense" -> recordToSplitRecord D.ExpenseKind catalog users record ssplits
    "income" -> recordToSplitRecord D.IncomeKind catalog users record ssplits
    "transfer" -> case S.unUserId (S._transferTo record) of
      Just recipient -> recordToTransferRecord catalog users recipient record
      Nothing -> Left "Stored payment recipient is missing"
    _ -> Left "Invalid stored record kind"
