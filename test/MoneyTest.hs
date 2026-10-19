{-# LANGUAGE OverloadedStrings #-}

-- | Exact-money regressions for wire formats, range checks, split allocation and conversion.
-- Remainder ties use UUID order; conversion rounds half up without floating-point arithmetic.
module MoneyTest (moneyTests) where

import Data.Aeson (Value (..), eitherDecode, encode, toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft)
import Data.List (sortOn)
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Jizhang.API.Types.Money
import qualified Jizhang.API.Types.Receipt as Receipt
import qualified Jizhang.API.Types.Record as R
import qualified Jizhang.API.Types.Report as Report
import Jizhang.API.Types.User (User (..), UserId (..))
import Jizhang.Money
import Test.Tasty
import Test.Tasty.HUnit

usd, jpy, kwd, eur :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "USD") "US Dollar" 2
jpy = CurrencyInfo (CurrencyCode "JPY") "Yen" 0
kwd = CurrencyInfo (CurrencyCode "KWD") "Kuwaiti Dinar" 3
eur = CurrencyInfo (CurrencyCode "EUR") "Euro" 2

-- UUID order is deliberate: equal remainders belong to a before b or c, regardless of input order.
a, b, c :: UserId
a = UserId (UUID.fromWords 0 0 0 1)
b = UserId (UUID.fromWords 0 0 0 2)
c = UserId (UUID.fromWords 0 0 0 3)

-- | Associate up to three values with the ordered identities used in allocation expectations.
rows :: [Integer] -> [(UserId, MinorAmount)]
rows = zip [a, b, c] . map MinorAmount

assertRejected :: (Show a) => Either Text a -> Assertion
assertRejected result = assertBool ("Expected rejection, got " <> show result) (isLeft result)

-- | Cover decoding separately from record validation, and conserve each currency's allocated total.
moneyTests :: TestTree
moneyTests =
  testGroup
    "Money"
    [ testCase "integer strings preserve values beyond JavaScript safe integers" $ do
        let amount = MinorAmount 9007199254740993
        encode amount @?= "\"9007199254740993\""
        eitherDecode (encode amount) @?= Right amount
        mapM_
          (\value -> assertBool (BL.unpack value) (isLeft (eitherDecode value :: Either String MinorAmount)))
          ["1", "1.2", "\"01\"", "\"-0\"", "\"+1\"", "\"1e3\"", "\" 1\""],
      testCase "signed report amounts retain arbitrary precision beyond the record range" $ do
        let aggregate = 10 ^ (80 :: Int) + 1
            positive = MinorAmount aggregate
            negative = MinorAmount (negate aggregate)
            zero = MinorAmount 0
            person = User a "alice" "Alice"
            breakdown = Report.BalanceBreakdown (R.RecordId UUID.nil) "Aggregate" (read "2026-10-14") "expense" positive negative
            balance = Report.Balance person negative positive zero zero zero zero zero [breakdown]
            settlement = Report.Settlement person (User b "bob" "Bob") positive
        mapM_ (\amount -> eitherDecode (encode amount) @?= Right amount) [positive, negative, MinorAmount maxRecordMinor, MinorAmount (maxRecordMinor + 1)]
        eitherDecode (encode balance) @?= Right balance
        eitherDecode (encode settlement) @?= Right settlement,
      testCase "record request boundaries bound amounts without changing signed or zero decoding" $ do
        let day = read "2026-10-14"
            expense amount = R.ExpenseRecordRequest "Expense" Nothing amount (CurrencyCode "USD") a day ExactSplit [R.RecordSplitRequest b amount] Nothing
            income amount = R.IncomeRecordRequest "Income" Nothing amount (CurrencyCode "USD") a day ExactSplit [R.RecordSplitRequest b amount] Nothing
            transfer amount = R.TransferRecordRequest amount (CurrencyCode "USD") a b day Nothing
        mapM_
          ( \amount -> do
              eitherDecode (encode (expense amount)) @?= Right (expense amount)
              eitherDecode (encode (income amount)) @?= Right (income amount)
              eitherDecode (encode (transfer amount)) @?= Right (transfer amount)
          )
          (map MinorAmount [0, 1, maxRecordMinor, -1, negate maxRecordMinor])
        let payload field value request = case toJSON request of
              Object fields -> encode (Object (KM.insert field (String value) fields))
              _ -> error "Record request fixture must encode as an object"
            one = MinorAmount 1
        mapM_
          ( \value -> do
              assertBool "expense amount rejected" (isLeft (eitherDecode (payload "amountMinor" value (expense one)) :: Either String R.ExpenseRecordRequest))
              assertBool "income amount rejected" (isLeft (eitherDecode (payload "amountMinor" value (income one)) :: Either String R.IncomeRecordRequest))
              assertBool "transfer amount rejected" (isLeft (eitherDecode (payload "amountMinor" value (transfer one)) :: Either String R.TransferRecordRequest))
              assertBool "split value rejected" (isLeft (eitherDecode (payload "value" value (R.RecordSplitRequest b one)) :: Either String R.RecordSplitRequest))
          )
          ["9223372036854775808", "-9223372036854775808", T.replicate 8192 "9", "-" <> T.replicate 8192 "9", "01", "-0", "+1", "1e3", " 1"]
        assertBool "unknown expense field rejected" (isLeft (eitherDecode (payload "unexpected" "1" (expense one)) :: Either String R.ExpenseRecordRequest))
        assertBool "unknown income field rejected" (isLeft (eitherDecode (payload "unexpected" "1" (income one)) :: Either String R.IncomeRecordRequest))
        assertBool "unknown transfer field rejected" (isLeft (eitherDecode (payload "unexpected" "1" (transfer one)) :: Either String R.TransferRecordRequest))
        assertBool "unknown split field rejected" (isLeft (eitherDecode (payload "unexpected" "1" (R.RecordSplitRequest b one)) :: Either String R.RecordSplitRequest))
        let oversizedSplit = R.RecordSplitRequest b (MinorAmount (maxRecordMinor + 1))
            nestedExpense = R.ExpenseRecordRequest "Expense" Nothing one (CurrencyCode "USD") a day ExactSplit [oversizedSplit] Nothing
            nestedIncome = R.IncomeRecordRequest "Income" Nothing one (CurrencyCode "USD") a day ExactSplit [oversizedSplit] Nothing
        assertBool "nested expense split rejected" (isLeft (eitherDecode (encode nestedExpense) :: Either String R.ExpenseRecordRequest))
        assertBool "nested income split rejected" (isLeft (eitherDecode (encode nestedIncome) :: Either String R.IncomeRecordRequest))
        assertBool "receipt creation child rejected" (isLeft (eitherDecode (encode (Receipt.CreateReceiptRequest "Receipt" [nestedExpense])) :: Either String Receipt.CreateReceiptRequest))
        assertBool "receipt update child rejected" (isLeft (eitherDecode (encode (Receipt.UpdateReceiptRequest "Receipt" [nestedExpense])) :: Either String Receipt.UpdateReceiptRequest)),
      testCase "currency and mode JSON accept only contract literals" $ do
        eitherDecode "\"USD\"" @?= Right (CurrencyCode "USD")
        assertBool "lowercase rejected" (isLeft (eitherDecode "\"usd\"" :: Either String CurrencyCode))
        eitherDecode "\"percentage\"" @?= Right PercentageSplit
        eitherDecode "\"multi\"" @?= Right MultiCurrency
        assertBool "constructor tag rejected" (isLeft (eitherDecode "\"EqualSplit\"" :: Either String SplitMode)),
      testCase "decimal major inputs respect currency precision and exact range" $ do
        parseMajorAmount usd " 12.34 " @?= Right (MinorAmount 1234)
        parseMajorAmount jpy "123" @?= Right (MinorAmount 123)
        parseMajorAmount kwd "1.234" @?= Right (MinorAmount 1234)
        parseMajorAmount usd "90071992547409.93" @?= Right (MinorAmount 9007199254740993)
        parseMajorAmount usd "92233720368547758.07" @?= Right (MinorAmount maxRecordMinor)
        parseMajorAmount usd "0" @?= Right (MinorAmount 0)
        assertRejected (validateRecordAmount (MinorAmount 0))
        mapM_ (assertRejected . parseMajorAmount usd) ["-1", "+1", "1e2", "NaN", "Infinity", "1,000", "1.001", "1.", ".1", "92233720368547758.08"]
        assertRejected (parseMajorAmount jpy "1.0")
        assertRejected (parseMajorAmount kwd "1.0001"),
      testCase "major amount range checks preserve leading zeros at every supported precision" $ do
        let cases = [(0, "9223372036854775807", "9223372036854775808"), (1, "922337203685477580.7", "922337203685477580.8"), (2, "92233720368547758.07", "92233720368547758.08"), (3, "9223372036854775.807", "9223372036854775.808"), (4, "922337203685477.5807", "922337203685477.5808")]
            zeros = T.replicate 8192 "0"
        mapM_
          ( \(digits, maximumValue, overflow) -> do
              let currency = CurrencyInfo (CurrencyCode "USD") "Test currency" digits
              parseMajorAmount currency maximumValue @?= Right (MinorAmount maxRecordMinor)
              parseMajorAmount currency (zeros <> maximumValue) @?= Right (MinorAmount maxRecordMinor)
              parseMajorAmount currency zeros @?= Right (MinorAmount 0)
              assertRejected (parseMajorAmount currency overflow)
              assertRejected (parseMajorAmount currency (zeros <> overflow))
              assertRejected (parseMajorAmount currency (T.replicate 8192 "9"))
          )
          cases
        parseMajorAmount usd (zeros <> "1.20") @?= Right (MinorAmount 120)
        assertRejected (parseMajorAmount usd (zeros <> "1.200")),
      testCase "formatting includes currency and standard precision" $ do
        formatMoney usd (MinorAmount 1234) @?= "USD 12.34"
        formatMoney jpy (MinorAmount 123) @?= "JPY 123"
        formatMoney kwd (MinorAmount 1234) @?= "KWD 1.234"
        formatMoney usd (MinorAmount (-1)) @?= "USD -0.01",
      testCase "largest remainder uses immutable UUID not submitted order" $ do
        allocateSplits EqualSplit (MinorAmount 1000) (rows [1, 1, 1]) @?= Right (rows [334, 333, 333])
        fmap (sortOn fst) (allocateSplits EqualSplit (MinorAmount 1000) (reverse (rows [1, 1, 1]))) @?= Right (rows [334, 333, 333])
        allocateSplits SharesSplit (MinorAmount 12000) (rows [2, 1, 1]) @?= Right (rows [6000, 3000, 3000])
        allocateSplits PercentageSplit (MinorAmount 10000) (rows [3333, 3333, 3334]) @?= Right (rows [3333, 3333, 3334])
        allocateSplits ExactSplit (MinorAmount 10000) (rows [1234, 8766]) @?= Right (rows [1234, 8766]),
      testCase "zero weights never get remainder and allocations conserve totals" $ do
        apportion (MinorAmount 1) [(a, 0), (b, 1), (c, 1)] @?= rows [0, 1, 0]
        mapM_
          ( \total ->
              mapM_
                ( \weights -> do
                    let allocated = apportion (MinorAmount total) (zip [a, b, c] weights)
                    sum [n | (_, MinorAmount n) <- allocated] @?= total
                    mapM_
                      (\((_, w), (_, MinorAmount n)) -> if w == 0 then n @?= 0 else assertBool "nonnegative" (n >= 0))
                      (zip (zip [a, b, c] weights) allocated)
                )
                [[0, 1, 1], [3, 7, 11], [50000, 50001, 0]]
          )
          [1, 2, 3, 1000, maxRecordMinor],
      testCase "invalid split shapes and totals are rejected" $ do
        let allocate mode = allocateSplits mode (MinorAmount 10000)
        assertRejected (allocate EqualSplit [])
        assertRejected (allocate EqualSplit [(a, MinorAmount 1), (a, MinorAmount 1)])
        assertRejected (allocate EqualSplit (rows [1, 0]))
        mapM_ (assertRejected . allocate SharesSplit . rows) [[0, 0], [-1, 2], [32768, 1]]
        mapM_ (assertRejected . allocate PercentageSplit . rows) [[5000, 4999], [5000, 5001], [10001, -1]]
        mapM_ (assertRejected . allocate ExactSplit . rows) [[1, 9998], [1, 10000], [-1, 10001]],
      testCase "rates canonicalize without floating point and enforce NUMERIC precision" $ do
        fmap canonicalDecimalRate (parseDecimalRate "0001.2000") @?= Right "1.2"
        fmap canonicalDecimalRate (parseDecimalRate "0.000000000000000001") @?= Right "0.000000000000000001"
        fmap canonicalDecimalRate (parseDecimalRate "999999999999999999.999999999999999999") @?= Right "999999999999999999.999999999999999999"
        mapM_ (assertRejected . parseDecimalRate) ["0", "-1", "1e2", "NaN", "1.", ".1", "1000000000000000000", "0.0000000000000000001"]
        fmap canonicalDecimalRate (decimalRateFromScientific (scientific 1200 (-3))) @?= Right "1.2"
        assertRejected (decimalRateFromScientific (scientific 1 (-19)))
        assertRejected (decimalRateFromScientific (scientific 1 18)),
      testCase "rate precision checks normalize zeros before exact bounded conversion" $ do
        let zeros = T.replicate 8192 "0"
            nines = T.replicate 8192 "9"
            minimumRate = DecimalRate (scientific 1 (-18))
            maximumRate = DecimalRate (scientific (10 ^ (36 :: Int) - 1) (-18))
        parseDecimalRate (zeros <> "1.20" <> zeros) @?= Right (DecimalRate (scientific 12 (-1)))
        parseDecimalRate (zeros <> "0.000000000000000001" <> zeros) @?= Right minimumRate
        parseDecimalRate (zeros <> "999999999999999999.999999999999999999" <> zeros) @?= Right maximumRate
        mapM_
          (assertRejected . parseDecimalRate)
          [zeros, zeros <> "." <> zeros, nines, "0." <> nines, "1." <> zeros <> "1", zeros <> "1000000000000000000", "0.0000000000000000001" <> zeros]
        assertBool "JSON rate precision rejected" (isLeft (eitherDecode (encode nines) :: Either String DecimalRate)),
      testCase "conversion is half-up with exact range and converted-zero rejection" $ do
        let convert original target text amount = parseDecimalRate text >>= \rateValue -> convertAmount original target rateValue (MinorAmount amount)
        convert eur usd "1.20" 10000 @?= Right (MinorAmount 12000)
        convert jpy usd "0.0067" 101 @?= Right (MinorAmount 68)
        convert usd jpy "1" 50 @?= Right (MinorAmount 1)
        assertRejected (convert usd jpy "1" 49)
        assertRejected (convert usd usd "2" maxRecordMinor),
      -- Source rounding can erase a share; settlement allocation must still use the logical ratio.
      testCase "foreign allocations use logical ratios independently in each currency" $ do
        allocateConvertedSplits EqualSplit (MinorAmount 1) (MinorAmount 100) (rows [1, 1])
          @?= Right [(a, MinorAmount 1, MinorAmount 50), (b, MinorAmount 0, MinorAmount 50)]
        allocateConvertedSplits ExactSplit (MinorAmount 10000) (MinorAmount 12000) (rows [2500, 7500])
          @?= Right [(a, MinorAmount 2500, MinorAmount 3000), (b, MinorAmount 7500, MinorAmount 9000)]
        let foreignRows = rows [50000, 50001]
        allocateConvertedSplits ExactSplit (MinorAmount 100001) (MinorAmount 123451) foreignRows
          @?= Right [(a, MinorAmount 50000, MinorAmount 61725), (b, MinorAmount 50001, MinorAmount 61726)],
      testCase "exchange inputs reject mixed provenance and unknown fields" $ do
        eitherDecode "{\"source\":\"manual\",\"rate\":\"1.2\"}" @?= (ManualRateInput <$> parseDecimalRateString "1.2")
        mapM_
          (\value -> assertBool (BL.unpack value) (isLeft (eitherDecode value :: Either String ExchangeRateInput)))
          [ "{\"source\":\"manual\",\"rate\":1.2}",
            "{\"source\":\"manual\",\"rate\":\"1.2\",\"quoteId\":null}",
            "{\"source\":\"reference\",\"rate\":\"1.2\"}",
            "{\"source\":\"manual\",\"rate\":\"1.2\",\"extra\":true}"
          ]
    ]
  where
    parseDecimalRateString = either (Left . show) Right . parseDecimalRate
