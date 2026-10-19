{-# LANGUAGE OverloadedStrings #-}

-- | Catalogue regressions for bundled currency precision, excluded codes and invalid metadata.
module CurrencyTest (currencyTests) where

import qualified Data.Map.Strict as M
import Jizhang.API.Types.Money
import Jizhang.Currency
import Test.Tasty
import Test.Tasty.HUnit

-- | Check both packaged data and validation failures; duplicate codes must never overwrite rows.
currencyTests :: TestTree
currencyTests =
  testGroup
    "Currency"
    [ testCase "bundled catalogue preserves standard minor units and excludes funds" $ do
        catalog <- loadCurrencyCatalog
        let digits currencyCode = case lookupCurrency catalog (CurrencyCode currencyCode) of
              Left err -> assertFailure (show err) >> pure (-1)
              Right (CurrencyInfo _ _ value) -> pure value
        digits "JPY" >>= (@?= 0)
        digits "USD" >>= (@?= 2)
        digits "KWD" >>= (@?= 3)
        assertBool "fund currencies excluded" (not (M.member (CurrencyCode "CLF") catalog))
        assertBool "test currency excluded" (not (M.member (CurrencyCode "XTS") catalog)),
      testCase "duplicate and invalid metadata fail instead of silently replacing" $ do
        let usd = CurrencyInfo (CurrencyCode "USD") "US Dollar" 2
            rejected rows = case validateCurrencyCatalog rows of
              Left _ -> pure ()
              Right _ -> assertFailure "Invalid catalogue accepted"
        rejected [usd, usd]
        rejected [usd, CurrencyInfo (CurrencyCode "USD") "Conflicting" 3]
        rejected [CurrencyInfo (CurrencyCode "usd") "US Dollar" 2]
        rejected [CurrencyInfo (CurrencyCode "USD") " " 2]
        rejected [CurrencyInfo (CurrencyCode "USD") "US Dollar" 5]
        rejected [],
      testCase "unsupported catalogue code has explicit boundary error" $ do
        catalog <- loadCurrencyCatalog
        lookupCurrency catalog (CurrencyCode "ZZZ") @?= Left "Unsupported currency"
    ]
