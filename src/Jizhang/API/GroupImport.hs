{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Decode five-column expense CSV into validated request inputs, not database writes.
-- Group membership and atomic persistence are enforced by the importing caller.
module Jizhang.API.GroupImport
  ( CSV,
    CSVData (..),
    parseRecords,
    resolveImportInput,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Reader (asks)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Csv
import Data.Data (Typeable)
import Data.Map.Strict (fromListWith)
import Data.Swagger (NamedSchema (..), ToSchema (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.IsList (toList)
import Jizhang.API.App
import Jizhang.API.Types
import Jizhang.API.Internal.Error (textToLBS, fromStoredData)
import Jizhang.API.User.Account (normalizeUsername)
import Jizhang.API.User.Projection (schemaUserToApiUser)
import Jizhang.API.Group.Projection (storedGroupSettings)
import Jizhang.API.Record.Input (resolveSplitRecordRate, inputCurrency)
import Jizhang.API.Internal.Resource (lookupUser)
import qualified Jizhang.Database.Schema as S
import Jizhang.Money (parseMajorAmount)
import Network.HTTP.Media ((//))
import Servant

-- | CSV bodies remain raw bytes until row parsing can report request errors.
data CSV deriving (Typeable)

newtype CSVData = CSVData ByteString
  deriving (Show, Eq, Ord, Generic)

instance ToSchema CSVData where
  declareNamedSchema _ = pure $ NamedSchema (Just "CSV") mempty

instance Accept CSV where
  contentType _ = "text" // "csv"

instance MimeRender CSV CSVData where
  mimeRender _ (CSVData bs) = bs

instance MimeUnrender CSV CSVData where
  mimeUnrender _ = Right . CSVData

newtype SpreadSheetSplit = SpreadSheetSplit [Text]
  deriving (Show, Eq, Ord, Generic)

instance FromField SpreadSheetSplit where
  parseField v = case decodeUtf8' v of
    Left _ -> fail "Invalid UTF-8 encoding"
    Right txt -> pure $ SpreadSheetSplit (T.splitOn "," txt)

data SpreadSheetRecord = SpreadSheetRecord
  { title :: !Text,
    amount :: !Text,
    paidBy :: !Text,
    split :: !SpreadSheetSplit,
    date :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromRecord SpreadSheetRecord where
  parseRecord v
    | length v == 5 = SpreadSheetRecord <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
    | otherwise = fail "Expected exactly 5 fields"

-- | Skip #-prefixed lines and the header; repeated split handles become share counts.
-- Amount precision follows the chosen currency and dates require ISO calendar syntax.
parseRecords :: CurrencyInfo -> Maybe ExchangeRateInput -> ByteString -> MyHandler [ExpenseRecordRequest]
parseRecords currencyInfo@(CurrencyInfo currencyCode _ _) rateInput raw = case decode HasHeader (removeComments raw) of
  Left err -> throwError err400 {errBody = textToLBS $ "Failed to parse CSV: " <> T.pack err}
  -- Resolve every row without writes, so a later malformed row cannot leave a partially imported ledger.
  Right recordsVec -> forM (zip [1 :: Integer ..] (toList recordsVec)) $ \(rowNumber, SpreadSheetRecord {..}) -> do
    parsedAmount <-
      either (\message -> throwError $ err400 {errBody = textToLBS message}) pure $
        parseMajorAmount currencyInfo amount
    parsedDate <- case iso8601ParseM (T.unpack date) :: Maybe Day of
      Nothing -> throwError $ err400 {errBody = textToLBS $ "Invalid date in CSV data row " <> T.pack (show rowNumber) <> "; expected YYYY-MM-DD"}
      Just day -> pure day
    let SpreadSheetSplit rawSplitUsers = split
    splitUsers <- mapM normalizeUsername rawSplitUsers
    payerName <- normalizeUsername paidBy
    let splitCounts = fromListWith (+) [(s, 1 :: Integer) | s <- splitUsers]
    when (any (> 32767) splitCounts) $
      throwError $
        err400 {errBody = "Split share exceeds 32767"}
    splits' <- forM (toList splitCounts) $ \(name, share) -> do
      User uid _ _ <- schemaUserToApiUser <$> lookupUser name
      pure $ RecordSplitRequest uid (MinorAmount share)
    User payerId _ _ <- schemaUserToApiUser <$> lookupUser payerName
    pure $ ExpenseRecordRequest title Nothing parsedAmount currencyCode payerId parsedDate SharesSplit splits' rateInput
  where
    -- Filtering is by physical line before CSV decoding, not by parsed rows or quoted field contents.
    -- Reported row numbers therefore count decoded data rows, excluding the header and removed lines.
    removeComments = BS.unlines . filter (not . BS.isPrefixOf "#") . BS.lines

-- | Select at most one explicit rate and validate it against the group's currency pair.
resolveImportInput :: S.Group -> CurrencyCode -> Maybe ExchangeQuoteId -> Maybe DecimalRate -> MyHandler (CurrencyInfo, Maybe ExchangeRateInput)
resolveImportInput group currencyCode quote manual = do
  currencies <- asks appCurrencies
  info <- inputCurrency currencies currencyCode
  (mode, base) <- fromStoredData $ storedGroupSettings currencies group
  rateInput <- case (quote, manual) of
    (Just _, Just _) -> throwError $ err400 {errBody = "Choose either exchangeQuoteId or manualRate"}
    (Just quoteId, Nothing) -> pure $ Just (ReferenceRateInput quoteId)
    (Nothing, Just rate) -> pure $ Just (ManualRateInput rate)
    (Nothing, Nothing) -> pure Nothing
  _ <- resolveSplitRecordRate (S._groupId group) mode info base rateInput
  pure (info, rateInput)
