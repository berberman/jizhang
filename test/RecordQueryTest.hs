{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure query and sort regressions for public record fields, identity roles and exact money.
-- Currency-scoped comparisons and ascending UUID tie-breaks keep mixed ledgers deterministic.
module RecordQueryTest (recordQueryTests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID (UUID)
import Jizhang.API.Types
  ( CurrencyCode (..),
    CurrencyInfo (..),
    GroupId (..),
    MinorAmount (..),
    Record (..),
    RecordId (..),
    RecordSplit (RecordSplit),
    SplitMode (..),
    User (..),
    UserId (..),
  )
import Jizhang.RecordQuery
import Test.Tasty
import Test.Tasty.HUnit

uuid :: Int -> UUID
uuid n = read $ "00000000-0000-0000-0000-" ++ replicate (12 - length (show n)) '0' ++ show n

-- Shared display names and a former split member distinguish identity matching from membership.
alice, bob, former :: User
alice = User (UserId $ uuid 101) "alice" "Same Name"
bob = User (UserId $ uuid 102) "bob" "Same Name"
former = User (UserId $ uuid 103) "former.member" "Former Member"

usd, jpy, eur :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "USD") "Dollar" 2
jpy = CurrencyInfo (CurrencyCode "JPY") "Yen" 0
eur = CurrencyInfo (CurrencyCode "EUR") "Euro" 2

catalog :: M.Map CurrencyCode CurrencyInfo
catalog = M.fromList [(code c, c) | c <- [usd, jpy, eur]]

-- Fixtures separate record dates from creation order and include escaped Unicode note content.
expense, income, transfer :: Record
expense =
  ExpenseRecord
    { recordId = RecordId $ uuid 1,
      title = "Team lunch: \"A\" \\ invoice",
      note = Just "Invoice 42\n会议",
      amountMinor = MinorAmount 9000,
      currency = usd,
      settlementAmountMinor = MinorAmount 9000,
      settlementCurrency = usd,
      exchangeRate = Nothing,
      paidBy = alice,
      date = fromGregorian 2026 10 2,
      createdAt = UTCTime (fromGregorian 2026 10 12) 0,
      groupId = GroupId $ uuid 100,
      splitMode = EqualSplit,
      splits = [RecordSplit bob (MinorAmount 1) (MinorAmount 4500) (MinorAmount 4500), RecordSplit former (MinorAmount 1) (MinorAmount 4500) (MinorAmount 4500)]
    }
income =
  IncomeRecord
    { recordId = RecordId $ uuid 2,
      title = "Salary",
      note = Nothing,
      amountMinor = MinorAmount 6000,
      currency = usd,
      settlementAmountMinor = MinorAmount 6000,
      settlementCurrency = usd,
      exchangeRate = Nothing,
      receivedBy = bob,
      date = fromGregorian 2026 10 3,
      createdAt = UTCTime (fromGregorian 2026 10 11) 0,
      groupId = GroupId $ uuid 100,
      splitMode = EqualSplit,
      splits = [RecordSplit alice (MinorAmount 1) (MinorAmount 6000) (MinorAmount 6000)]
    }
transfer =
  TransferRecord
    { recordId = RecordId $ uuid 3,
      title = "Payment",
      note = Just "Invoice 42",
      amountMinor = MinorAmount 2000,
      currency = usd,
      settlementAmountMinor = MinorAmount 2000,
      settlementCurrency = usd,
      exchangeRate = Nothing,
      paidBy = bob,
      transferTo = alice,
      date = fromGregorian 2026 10 4,
      createdAt = UTCTime (fromGregorian 2026 10 10) 0,
      groupId = GroupId $ uuid 100
    }

records :: [Record]
records = [transfer, income, expense]

ids :: [Record] -> [RecordId]
ids = map (\r -> r.recordId)

-- | Require successful parsing and compare filtered IDs in the source's existing order.
expect :: Text -> [Int] -> [Record] -> Assertion
expect query wanted source = case parseRecordQuery catalog query of
  Left message -> assertFailure $ T.unpack message
  Right parsed -> assertEqual (T.unpack query) (map (RecordId . uuid) wanted) $ ids $ filter (matchesRecordQuery parsed) source

-- | Compare the full sorted ID sequence, so missing roles and tie-breaks remain observable.
sorted :: Maybe Text -> [Int] -> [Record] -> Assertion
sorted sort wanted source = case parseRecordSort sort of
  Left message -> assertFailure $ T.unpack message
  Right parsed -> assertEqual (show sort) (map (RecordId . uuid) wanted) $ ids $ sortRecords parsed source

recordQueryTests :: TestTree
recordQueryTests =
  testGroup
    "RecordQuery"
    [ testCase "bare, quoted literal colons and escaped values use AND across public fields" $ do
        expect "  " [3, 2, 1] records
        expect "invoice 42" [3, 1] records
        expect "team \"Same Name\" 会议" [1] records
        expect "\"lunch:\"" [1] records
        expect "title:\"lunch: \\\"A\\\" \\\\ invoice\"" [1] records
        expect "type:EXPENSE title:team title:lunch note:\"invoice 42\" currency:usd amount:>=25 date:2026-10-01..2026-10-31" [1] records
        expect "title:team title:salary" [] records
        expect "note:invoice note:42" [3, 1] records
        expect "note:salary" [] records
        expect "alice@example.test" [] records,
      testCase "malformed qualifiers, quotes, operators, dates and amounts reject" $ do
        forM_ ["\"", "title:\"unterminated", "\"\"", "note:\"\"", "title:", "Title:team", "category:food", "type:payment", "note:\"a\\nb\"", "payer:bad!handle", "date:2026-02-29", "date:2024-02-30", "date:2026-1-01", "date:2026-10-01..", "date:..2026-10-01", "date:2026-10-02..2026-10-01", "created:=>2026-10-01", "date:2026-10-01..2026-10-02..2026-10-03", "currency:ZZZ", "amount:1", "currency:ZZZ amount:1", "currency:USD currency:JPY amount:1", "currency:USD amount:1.001", "currency:JPY amount:1.0", "currency:USD amount:-1", "currency:USD amount:1e2", "currency:USD amount:1.", "currency:USD amount:92233720368547758.08", "currency:USD amount:20..10", "currency:USD amount:>=", "currency:USD amount:<=-1"] $ \query ->
          assertBool ("must reject " ++ T.unpack query) $ isLeft $ parseRecordQuery catalog query,
      testCase "identity roles are exact, current, public and include former split members" $ do
        expect "type:income payee:@BOB" [2] records
        expect "payer:bob payee:alice" [3] records
        expect "payer:bob type:income" [] records
        expect "payee:alice type:expense" [] records
        expect "member:former.member" [1] records
        expect "member:\"Former Member\"" [1] records
        expect "member:00000000-0000-0000-0000-000000000103" [1] records
        expect "payer:\"same name\"" [3, 1] records
        expect "payee:\"Same Name\"" [3, 2] records
        expect "member:unknown" [] records
        expect "member:00000000-0000-0000-0000-000000000999" [] records
        expect "member:former" [] records
        case expense of
          original@ExpenseRecord {} -> do
            let renamed = original {paidBy = User (UserId $ uuid 101) "renamed" "New Name"}
            expect "payer:alice" [] [renamed]
            expect "payer:renamed" [1] [renamed]
            expect "payer:00000000-0000-0000-0000-000000000101" [1] [renamed]
          _ -> assertFailure "expected expense fixture",
      testCase "calendar comparisons cover all operators and inclusive ranges" $ do
        forM_ [("2026-10-03", [2]), ("=2026-10-03", [2]), ("<2026-10-03", [1]), ("<=2026-10-03", [2, 1]), (">2026-10-03", [3]), (">=2026-10-03", [3, 2]), ("2026-10-02..2026-10-03", [2, 1])] $ \(queryValue, wanted) -> expect ("date:" <> queryValue) wanted records
        expect "date:2024-02-29" [1] [expense {date = fromGregorian 2024 2 29}]
        let boundary = [expense {createdAt = UTCTime (fromGregorian 2026 10 1) 86399}, income {createdAt = UTCTime (fromGregorian 2026 10 2) 0}]
        expect "created:2026-10-01" [1] boundary
        expect "created:>=2026-10-02" [2] boundary
        expect "created:2026-10-01..2026-10-02" [1, 2] boundary
        forM_ [("=2026-10-11", [2]), ("<2026-10-11", [3]), ("<=2026-10-11", [3, 2]), (">2026-10-11", [1]), (">=2026-10-11", [2, 1])] $ \(queryValue, wanted) -> expect ("created:" <> queryValue) wanted records,
      testCase "money comparisons are exact and currency precision and grouping are explicit" $ do
        forM_ [("60", [2]), ("=60.00", [2]), ("<60", [3]), ("<=60", [3, 2]), (">60", [1]), (">=60", [2, 1]), ("20..60", [3, 2])] $ \(queryValue, wanted) -> expect ("currency:USD amount:" <> queryValue) wanted records
        expect "currency:USD currency:usd amount:>=0" [3, 2, 1] records
        expect "currency:JPY amount:90" [1] [expense {currency = jpy, amountMinor = MinorAmount 90}]
        expect "currency:USD amount:90071992547409.93" [1] [expense {amountMinor = MinorAmount 9007199254740993}]
        expect "currency:USD amount:92233720368547758.07" [1] [expense {amountMinor = MinorAmount 9223372036854775807}],
      testCase "all sort fields, role absence and currency groups have deterministic direction" $ do
        sorted Nothing [3, 2, 1] records
        sorted (Just "date") [1, 2, 3] records
        sorted (Just "-date") [3, 2, 1] records
        sorted (Just "createdAt") [3, 2, 1] records
        sorted (Just "-createdAt") [1, 2, 3] records
        sorted (Just "title") [3, 2, 1] records
        sorted (Just "-title") [1, 2, 3] records
        sorted (Just "type") [1, 2, 3] records
        sorted (Just "-type") [3, 2, 1] records
        sorted (Just "payer") [1, 3, 2] records
        sorted (Just "-payer") [3, 1, 2] records
        sorted (Just "payee") [3, 2, 1] records
        sorted (Just "-payee") [2, 3, 1] records
        let mixed = [expense {currency = eur}, income {currency = jpy}, transfer, expense {recordId = RecordId $ uuid 4, amountMinor = MinorAmount 1000}]
        sorted (Just "amount") [1, 2, 4, 3] mixed
        sorted (Just "-amount") [1, 2, 3, 4] mixed
        sorted (Just "currency") [1, 2, 3, 4] mixed
        sorted (Just "-currency") [3, 4, 2, 1] mixed
        let ties = [expense {recordId = RecordId $ uuid n} | n <- [3, 1, 2]]
        forM_ ["title", "date", "createdAt", "type", "payer", "payee", "currency", "amount"] $ \field -> do
          sorted (Just field) [1, 2, 3] ties
          sorted (Just $ "-" <> field) [1, 2, 3] ties
        let sameDay = [expense {recordId = RecordId $ uuid 2, createdAt = UTCTime (fromGregorian 2026 10 12) 1}, expense]
        sorted (Just "createdAt") [1, 2] sameDay
        sorted (Just "-createdAt") [2, 1] sameDay
        forM_ ["", "-", "Date", "created", "amount,date", "unknown", " date"] $ \sortValue ->
          assertBool ("invalid sort " ++ show sortValue) $ isLeft $ parseRecordSort $ Just sortValue
    ]
