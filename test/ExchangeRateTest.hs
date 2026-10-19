{-# LANGUAGE OverloadedStrings #-}

-- | Provider-decoding regressions for exact decimal numbers, pair/date validation
-- and bounded response bodies; these cases perform no network requests.
module ExchangeRateTest (exchangeRateTests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Jizhang.API.Types.Money (CurrencyCode (..), canonicalDecimalRate)
import Jizhang.ExchangeRate
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

source, target :: CurrencyCode
source = CurrencyCode "EUR"
target = CurrencyCode "USD"

-- | Fixed validation clock so future-date rejection does not depend on the day tests run.
today :: Day
today = fromGregorian 2026 10 16

-- | Insert a raw JSON rate token, preserving numeric spelling for precision tests.
providerBody :: BL.ByteString -> BL.ByteString
providerBody value = "{\"date\":\"2026-10-16\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":" <> value <> "}"

-- | Decode the fixture pair against the fixed clock used by all response tables.
decode :: BL.ByteString -> Either ExchangeRateError ReferenceRate
decode = decodeReferenceRate source target today

exchangeRateTests :: TestTree
exchangeRateTests =
  testGroup
    "ExchangeRate"
    [ testGroup
        "exact provider numbers"
        [ testCase label $
            fmap (canonicalDecimalRate . rate) (decode (providerBody value)) @?= Right expected
        | (label, value, expected) <- validRates
        ],
      testGroup
        "invalid provider responses"
        [ testCase label $ decode body @?= Left InvalidProviderResponse
        | (label, body) <- invalidBodies
        ],
      testGroup
        "unrepresentable or nonnumeric rates"
        [ testCase label $ decode (providerBody value) @?= Left InvalidProviderResponse
        | (label, value) <- invalidRates
        ],
      testCase "actual provider shape preserves effective date" $
        fmap rateDate (decode "{\"date\":\"2026-10-15\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}")
          @?= Right (fromGregorian 2026 10 15),
      testCase "valid leap day is accepted" $
        fmap rateDate (decode "{\"date\":\"2024-02-29\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1}")
          @?= Right (fromGregorian 2024 2 29),
      testCase "expected pair is parameterized rather than hard-coded" $
        fmap
          (canonicalDecimalRate . rate)
          ( decodeReferenceRate
              (CurrencyCode "JPY")
              (CurrencyCode "GBP")
              today
              "{\"date\":\"2026-10-16\",\"base\":\"JPY\",\"quote\":\"GBP\",\"rate\":0.0051}"
          )
          @?= Right "0.0051",
      testCase "exact body limit permits valid JSON with whitespace" $ do
        let body = providerBody "1.1224"
            padded = body <> BL.replicate (65536 - BL.length body) ' '
        fmap (canonicalDecimalRate . rate) (decode padded) @?= Right "1.1224"
        decode (padded <> " ") @?= Left InvalidProviderResponse
    ]

-- | Accepted spellings and their exact canonical values, including numbers unsafe in JavaScript.
validRates :: [(String, BL.ByteString, Text)]
validRates =
  [ ("daily pair quote", "1.1224", "1.1224"),
    ("unit rate", "1", "1"),
    ("canonicalizes insignificant zeros", "1.2000000000000000000000", "1.2"),
    ("positive exponent remains exact", "11224e-4", "1.1224"),
    ("scientific notation with positive exponent", "1.234e2", "123.4"),
    ("minimum representable rate", "1e-18", "0.000000000000000001"),
    ("maximum representable rate", "999999999999999999.999999999999999999", "999999999999999999.999999999999999999"),
    ("large integer beyond JavaScript precision", "9007199254740993", "9007199254740993"),
    ("large exact fractional coefficient", "9007199254740993.123456789012345678", "9007199254740993.123456789012345678"),
    ("trailing zeros do not consume numeric scale", "1000000000000000000e-36", "0.000000000000000001")
  ]

invalidRates :: [(String, BL.ByteString)]
invalidRates =
  [ ("zero", "0"),
    ("negative zero", "-0.0"),
    ("negative", "-1.1224"),
    ("fractional precision exceeds scale", "1.0000000000000000001"),
    ("below smallest rate", "0.0000000000000000001"),
    ("integer precision exceeds range", "1000000000000000000"),
    ("exponent exceeds range", "1e18"),
    ("exponent exceeds scale", "1e-19"),
    ("very large exponent", "1e1000000"),
    ("very small exponent", "1e-1000000"),
    ("large coefficient exceeds range", "999999999999999999999999999999999999999999"),
    ("string is not a provider number", "\"1.1224\""),
    ("null rate", "null"),
    ("boolean rate", "true"),
    ("object rate", "{}"),
    ("array rate", "[]"),
    ("NaN", "NaN"),
    ("infinity", "Infinity")
  ]

invalidBodies :: [(String, BL.ByteString)]
invalidBodies =
  [ ("swapped pair", "{\"date\":\"2026-10-16\",\"base\":\"USD\",\"quote\":\"EUR\",\"rate\":1.1224}"),
    ("wrong source", "{\"date\":\"2026-10-16\",\"base\":\"GBP\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("wrong target", "{\"date\":\"2026-10-16\",\"base\":\"EUR\",\"quote\":\"GBP\",\"rate\":1.1224}"),
    ("noncanonical currency", "{\"date\":\"2026-10-16\",\"base\":\"eur\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("future date", "{\"date\":\"2026-10-17\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("invalid calendar date", "{\"date\":\"2026-02-30\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("invalid leap day", "{\"date\":\"2025-02-29\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("date is not a timestamp", "{\"date\":\"2026-10-16T00:00:00Z\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("date requires canonical padding", "{\"date\":\"2026-1-6\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("malformed date", "{\"date\":\"not-a-date\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("null date", "{\"date\":null,\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("missing date", "{\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("missing base", "{\"date\":\"2026-10-16\",\"quote\":\"USD\",\"rate\":1.1224}"),
    ("missing quote", "{\"date\":\"2026-10-16\",\"base\":\"EUR\",\"rate\":1.1224}"),
    ("missing rate", "{\"date\":\"2026-10-16\",\"base\":\"EUR\",\"quote\":\"USD\"}"),
    ("unknown field", "{\"date\":\"2026-10-16\",\"base\":\"EUR\",\"quote\":\"USD\",\"rate\":1.1224,\"extra\":true}"),
    ("malformed JSON", "{\"date\":"),
    ("trailing JSON", providerBody "1" <> "{}"),
    ("empty body", ""),
    ("array instead of pair object", "[]"),
    ("null instead of pair object", "null")
  ]
