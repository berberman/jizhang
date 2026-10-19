{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Resolve external record requests into persistence-ready money and participant IDs.
-- Input errors and malformed stored currency policy follow separate HTTP error paths.
module Jizhang.API.Record.Input
  ( splitModeText,
    normalizeRecordNote,
    resolveExpenseRequest,
    resolveIncomeRequest,
    resolveSplitRequest,
    resolveSplitRecordRate,
    resolveTransferRequest,
    inputCurrency,
    withGroupCurrencyLock
  )
where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Data.UUID (UUID)
import Database.Beam.Postgres (Pg)
import Jizhang.API.App (AppEnv (appCurrencies), MyHandler, runDB, runDBTransaction)
import Jizhang.API.Group.Projection (storedGroupSettings)
import Jizhang.API.Internal.Error (fetchOrFail, fromInput, fromStoredData)
import Jizhang.API.Internal.Resource (ensureActiveGroupUser)
import Jizhang.API.Types.Money (CurrencyCode, CurrencyInfo (CurrencyInfo), CurrencyMode (..), DecimalRate (DecimalRate), ExchangeQuoteId (ExchangeQuoteId), ExchangeRate (ExchangeRate), ExchangeRateInput (..), MinorAmount, SplitMode (..), currencyCodeText, minorAmountInteger)
import Jizhang.API.Types.Record (ExpenseRecordRequest (..), IncomeRecordRequest (..), RecordSplitRequest (RecordSplitRequest), TransferRecordRequest (..))
import Jizhang.API.Types.User (UserId (UserId))
import Jizhang.Currency (CurrencyCatalog, lookupCurrency)
import qualified Jizhang.Database as D (ResolvedSplit (ResolvedSplit), ResolvedSplitRecord (..), ResolvedTransfer (ResolvedTransfer), getExchangeQuote, lockGroupCurrency)
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Money as Money
import Servant (ServerError (errBody), err400, err409, throwError)

inputCurrency :: CurrencyCatalog -> CurrencyCode -> MyHandler CurrencyInfo
inputCurrency catalog = fromInput . lookupCurrency catalog

-- | The conditional row update serializes policy changes and financial writes.
-- Call only for nonempty financial mutations, after all request validation.
withGroupCurrencyLock :: S.Group -> Pg a -> MyHandler a
withGroupCurrencyLock group action = do
  catalog <- asks appCurrencies
  (mode, CurrencyInfo base _ _) <- fromStoredData $ storedGroupSettings catalog group
  result <- runDBTransaction $ do
    -- Recheck the policy snapshot under the write lock; validation alone cannot exclude a concurrent change.
    locked <- D.lockGroupCurrency (S._groupId group) mode base
    if locked then Just <$> action else pure Nothing
  maybe (throwError $ err409 {errBody = "Group currency settings changed; reload and try again"}) pure result

splitModeText :: SplitMode -> Text
splitModeText EqualSplit = "equal"
splitModeText SharesSplit = "shares"
splitModeText PercentageSplit = "percentage"
splitModeText ExactSplit = "exact"

-- | Blank notes become absent; nonblank notes retain whitespace and reject NUL.
normalizeRecordNote :: Maybe Text -> Either Text (Maybe Text)
normalizeRecordNote Nothing = Right Nothing
normalizeRecordNote (Just text)
  | T.any (== '\0') text = Left "Note cannot contain NUL"
  | T.null (T.strip text) = Right Nothing
  | otherwise = Right (Just text)

resolveExpenseRequest :: S.Group -> ExpenseRecordRequest -> MyHandler D.ResolvedSplitRecord
resolveExpenseRequest group ExpenseRecordRequest {..} =
  resolveSplitRequest group title note amountMinor currency byUserId date splitMode splits exchangeRate

resolveIncomeRequest :: S.Group -> IncomeRecordRequest -> MyHandler D.ResolvedSplitRecord
resolveIncomeRequest group IncomeRecordRequest {..} =
  resolveSplitRequest group title note amountMinor currency receivedByUserId date splitMode splits exchangeRate

-- | Require active participants and allocate both original and settlement totals
-- before persistence; conversion follows the group's stored currency policy.
resolveSplitRequest :: S.Group -> Text -> Maybe Text -> MinorAmount -> CurrencyCode -> UserId -> Day -> SplitMode -> [RecordSplitRequest] -> Maybe ExchangeRateInput -> MyHandler D.ResolvedSplitRecord
resolveSplitRequest group title note amountMinor currency actorUserId date splitMode splits exchangeRate = do
  normalizedNote <- fromInput $ normalizeRecordNote note
  catalog <- asks appCurrencies
  (mode, base) <- fromStoredData $ storedGroupSettings catalog group
  sourceInfo <- inputCurrency catalog currency
  _ <- fromInput $ Money.validateRecordAmount amountMinor
  when (T.null title) $ throwError $ err400 {errBody = "Title cannot be empty"}
  let gid = S._groupId group
      rows = [(uid, weight) | RecordSplitRequest uid weight <- splits]
  ensureActiveGroupUser gid actorUserId
  forM_ rows $ \(uid, _) -> ensureActiveGroupUser gid uid
  fx <- resolveSplitRecordRate gid mode sourceInfo base exchangeRate
  let targetInfo = case mode of SingleCurrency -> base; MultiCurrency -> sourceInfo
  converted <- case fx of
    Nothing -> pure amountMinor
    Just (ExchangeRate _ rateValue _ _ _) -> fromInput $ Money.convertAmount sourceInfo targetInfo rateValue amountMinor
  allocated <- fromInput $ Money.allocateConvertedSplits splitMode amountMinor converted rows
  let values = M.fromList [(uid, minorAmountInteger weight) | (uid, weight) <- rows]
      resolved =
        [ D.ResolvedSplit
            (coerce uid)
            (fromInteger $ values M.! uid)
            (fromInteger $ minorAmountInteger original)
            (fromInteger $ minorAmountInteger settlement)
        | (uid, original, settlement) <- allocated
        ]
      (rateValue, rateSource, rateDay, fetched, quote) = case fx of
        Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
        Just (ExchangeRate provenance (DecimalRate rateNumber) day timestamp quoteId) ->
          (Just rateNumber, Just provenance, day, timestamp, coerce quoteId)
  pure
    D.ResolvedSplitRecord
      { D.resolvedTitle = title,
        D.resolvedNote = normalizedNote,
        D.resolvedAmountMinor = fromInteger $ minorAmountInteger amountMinor,
        D.resolvedCurrency = currencyCodeText currency,
        D.resolvedSettlementCurrency = currencyCodeText $ case targetInfo of CurrencyInfo code _ _ -> code,
        D.resolvedSettlementAmountMinor = fromInteger $ minorAmountInteger converted,
        D.resolvedByUserId = coerce actorUserId,
        D.resolvedDate = date,
        D.resolvedSplitMode = splitModeText splitMode,
        D.resolvedExchangeRate = rateValue,
        D.resolvedRateSource = rateSource,
        D.resolvedRateDate = rateDay,
        D.resolvedRateFetchedAt = fetched,
        D.resolvedExchangeQuoteId = quote,
        D.resolvedSplits = resolved
      }

-- | Conversion requires an explicit manual rate or a quote from this group and pair.
-- Rates are forbidden when no conversion is needed, including multi-currency mode.
resolveSplitRecordRate :: UUID -> CurrencyMode -> CurrencyInfo -> CurrencyInfo -> Maybe ExchangeRateInput -> MyHandler (Maybe ExchangeRate)
resolveSplitRecordRate gid mode (CurrencyInfo source _ _) (CurrencyInfo target _ _) supplied
  | mode == MultiCurrency || source == target = do
      unless (isNothing supplied) $ throwError $ err400 {errBody = "Exchange rate is not allowed without conversion"}
      pure Nothing
  | otherwise = case supplied of
      Nothing -> throwError $ err400 {errBody = "An exchange rate is required for this currency"}
      Just (ManualRateInput (DecimalRate rateNumber)) -> do
        valid <- fromInput $ Money.decimalRateFromScientific rateNumber
        pure $ Just $ ExchangeRate "manual" valid Nothing Nothing Nothing
      Just (ReferenceRateInput quoteId@(ExchangeQuoteId qid)) -> do
        -- Load the saved quote within this group, then check direction; an inverse pair is not interchangeable.
        quote <- fetchOrFail "Exchange quote" qid $ runDB $ D.getExchangeQuote gid qid
        unless (S._quoteSourceCurrency quote == currencyCodeText source && S._quoteTargetCurrency quote == currencyCodeText target) $
          throwError $
            err400 {errBody = "Exchange quote does not match the record currency pair"}
        unless (S._quoteProvider quote == "frankfurter") $
          fromStoredData $
            Left "Invalid stored quote provider"
        valid <- fromStoredData $ Money.decimalRateFromScientific (S._quoteRate quote)
        -- Snapshot provenance with the rate so later provider updates cannot rewrite the record's history.
        pure $ Just $ ExchangeRate "reference" valid (Just $ S._quoteRateDate quote) (Just $ S._quoteFetchedAt quote) (Just quoteId)

-- | Payments use one currency, distinct active participants and no exchange rate.
resolveTransferRequest :: S.Group -> TransferRecordRequest -> MyHandler D.ResolvedTransfer
resolveTransferRequest group TransferRecordRequest {..} = do
  normalizedNote <- fromInput $ normalizeRecordNote note
  catalog <- asks appCurrencies
  (mode, CurrencyInfo base _ _) <- fromStoredData $ storedGroupSettings catalog group
  _ <- inputCurrency catalog currency
  _ <- fromInput $ Money.validateRecordAmount amountMinor
  when (mode == SingleCurrency && currency /= base) $
    throwError $
      err400 {errBody = "Payments must use the group default currency"}
  ensureActiveGroupUser (S._groupId group) byUserId
  ensureActiveGroupUser (S._groupId group) toUserId
  when (byUserId == toUserId) $ throwError $ err400 {errBody = "Transfer cannot be made to the same user"}
  pure $
    D.ResolvedTransfer
      (fromInteger $ minorAmountInteger amountMinor)
      (currencyCodeText currency)
      (coerce byUserId)
      (coerce toUserId)
      date
      normalizedNote
