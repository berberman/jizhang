{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Load the packaged currency metadata used to interpret minor units throughout the ledger.
module Jizhang.Currency
  ( CurrencyCatalog,
    loadCurrencyCatalog,
    validateCurrencyCatalog,
    lookupCurrency,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Jizhang.API.Types.Money
import Paths_jizhang (getDataFileName)

-- | Currency metadata indexed by code; use 'validateCurrencyCatalog' to establish invariants.
type CurrencyCatalog = M.Map CurrencyCode CurrencyInfo

-- | Reject empty catalogs, duplicate or reserved codes, blank names and precision outside 0..4.
validateCurrencyCatalog :: [CurrencyInfo] -> Either Text CurrencyCatalog
validateCurrencyCatalog [] = Left "Currency catalogue is empty"
validateCurrencyCatalog currencies = foldl' insertCurrency (Right M.empty) currencies
  where
    insertCurrency result info@(CurrencyInfo currencyCode currencyName digits) = do
      catalog <- result
      let CurrencyCode text = currencyCode
      if T.length text /= 3
        || not (T.all (\c -> c >= 'A' && c <= 'Z') text)
        || T.null (T.strip currencyName)
        || digits < 0
        || digits > 4
        || text `elem` ["XTS", "XXX"]
        then Left "Invalid currency metadata"
        else
          if M.member currencyCode catalog
            then Left "Duplicate currency code"
            else Right (M.insert currencyCode info catalog)

-- | Resolve only catalog-supported codes; a syntactically valid code need not be supported.
lookupCurrency :: CurrencyCatalog -> CurrencyCode -> Either Text CurrencyInfo
lookupCurrency catalog currencyCode = maybe (Left "Unsupported currency") Right (M.lookup currencyCode catalog)

-- | Read Cabal's packaged JSON catalog; missing files or invalid metadata fail startup.
loadCurrencyCatalog :: IO CurrencyCatalog
loadCurrencyCatalog = do
  path <- getDataFileName "data/currencies.json"
  bytes <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case bytes of
    Left err -> fail ("Cannot load currency catalogue: " <> show err)
    Right contents -> case eitherDecodeStrict' contents of
      Left err -> fail ("Invalid currency catalogue JSON: " <> err)
      Right currencies ->
        either
          (fail . ("Invalid currency catalogue: " <>) . T.unpack)
          pure
          (validateCurrencyCatalog currencies)
