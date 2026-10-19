{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure and HTTP spending-statistics regressions for saved settlement amounts,
-- clipped calendar buckets, personal shares and independent currency sections.
module StatisticsTest (statisticsTests) where

import APITest
import Control.Monad (forM_, void)
import Data.Aeson (Value (Null), toJSON)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), fromGregorian)
import qualified Data.UUID as UUID
import Jizhang.API.Types
import Jizhang.Report.Statistics (calculateSpendingStatistics)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status
import Servant (FromHttpApiData (parseUrlPiece))
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit

uuid :: Int -> UUID.UUID
uuid n = read $ "00000000-0000-0000-0000-" ++ replicate (12 - length (show n)) '0' ++ show n

-- Duplicate labels and a historical member expose accidental name-based grouping or roster loss.
a, b, former :: User
a = User (UserId $ uuid 1) "alice" "Same label"
b = User (UserId $ uuid 2) "bob" "Same label"
former = User (UserId $ uuid 3) "former" "Former"

key :: User -> UserId
key (User value _ _) = value

gid :: GroupId
gid = GroupId $ uuid 100

usd, eur :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "USD") "US Dollar" 2
eur = CurrencyInfo (CurrencyCode "EUR") "Euro" 2

d :: Int -> Int -> Day
d = fromGregorian 2024

-- | Build an already allocated same-currency expense; supplied shares are minor units.
expense :: Int -> Day -> CurrencyInfo -> Integer -> User -> [(User, Integer)] -> Record
expense n day info amount payer shares =
  ExpenseRecord
    { recordId = RecordId $ uuid n,
      title = T.pack $ "Expense " ++ show n,
      note = Nothing,
      currency = info,
      amountMinor = MinorAmount amount,
      settlementCurrency = info,
      settlementAmountMinor = MinorAmount amount,
      paidBy = payer,
      date = day,
      createdAt = UTCTime (d 1 1) 0,
      groupId = gid,
      splitMode = ExactSplit,
      exchangeRate = Nothing,
      splits = [RecordSplit u (MinorAmount s) (MinorAmount s) (MinorAmount s) | (u, s) <- shares]
    }

-- | Build saved income independently of expense totals and the selected personal subject.
income :: Int -> Day -> CurrencyInfo -> Integer -> User -> [(User, Integer)] -> Record
income n day info amount recipient shares =
  IncomeRecord
    { recordId = RecordId $ uuid n,
      title = T.pack $ "Income " ++ show n,
      note = Nothing,
      currency = info,
      amountMinor = MinorAmount amount,
      settlementCurrency = info,
      settlementAmountMinor = MinorAmount amount,
      receivedBy = recipient,
      date = day,
      createdAt = UTCTime (d 1 1) 0,
      groupId = gid,
      splitMode = ExactSplit,
      exchangeRate = Nothing,
      splits = [RecordSplit u (MinorAmount s) (MinorAmount s) (MinorAmount s) | (u, s) <- shares]
    }

payment :: Record
payment =
  TransferRecord
    { recordId = RecordId $ uuid 13,
      title = "Payment",
      amountMinor = MinorAmount 3000,
      note = Nothing,
      currency = usd,
      settlementCurrency = usd,
      settlementAmountMinor = MinorAmount 3000,
      exchangeRate = Nothing,
      paidBy = b,
      transferTo = a,
      date = d 1 25,
      createdAt = UTCTime (d 1 1) 0,
      groupId = gid
    }

-- | Preserve distinct EUR source and USD settlement values without recalculating a live rate.
converted :: Record
converted =
  ExpenseRecord
    { recordId = RecordId $ uuid 10,
      title = "Converted expense",
      date = d 1 10,
      note = Nothing,
      currency = eur,
      amountMinor = MinorAmount 10000,
      settlementCurrency = usd,
      settlementAmountMinor = MinorAmount 12000,
      paidBy = a,
      createdAt = UTCTime (d 1 1) 0,
      groupId = gid,
      splitMode = ExactSplit,
      exchangeRate = Nothing,
      splits = [RecordSplit a (MinorAmount 2500) (MinorAmount 2500) (MinorAmount 3000), RecordSplit b (MinorAmount 7500) (MinorAmount 7500) (MinorAmount 9000)]
    }

-- | Mix converted and local spending across months with a transfer that statistics must exclude.
fixture :: [Record]
fixture = [converted, expense 11 (d 1 20) usd 8000 b [(a, 4000), (b, 4000)], expense 12 (d 2 1) usd 4000 a [(b, 4000)], payment]

-- | Use a deliberately unsorted roster including a former member and USD as the base currency.
calculate :: CurrencyMode -> Day -> Day -> StatsBucket -> User -> [Record] -> SpendingStatistics
calculate mode lo hi step person = calculateSpendingStatistics gid mode usd lo hi step person [former, b, a]

-- | Require exactly one currency section, rather than silently discarding extra sections.
one :: SpendingStatistics -> CurrencySpending
one (SpendingStatistics _ _ _ _ _ _ [section]) = section
one other = error $ "Expected one currency: " ++ show other

-- | Project gross expense total/count and the subject's paid/share amounts in minor units.
summary :: CurrencySpending -> (Integer, Int, Integer, Integer)
summary (CurrencySpending _ (MinorAmount total) count _ _ _ (MinorAmount paid) (MinorAmount share) _ _ _ _) = (total, count, paid, share)

bucketValues :: CurrencySpending -> [(Day, Day, Integer, Int, Integer, Integer)]
bucketValues (CurrencySpending _ _ _ _ _ _ _ _ buckets _ _ _) =
  [(lo, hi, t, n, p, s) | SpendingBucket lo hi (MinorAmount t) n _ _ _ (MinorAmount p) (MinorAmount s) <- buckets]

incomeValues :: CurrencySpending -> (Integer, Int, Integer)
incomeValues (CurrencySpending _ _ _ (MinorAmount amount) count (MinorAmount net) _ _ _ _ _ _) = (amount, count, net)

bucketIncomeValues :: CurrencySpending -> [(Day, Day, Integer, Int, Integer)]
bucketIncomeValues (CurrencySpending _ _ _ _ _ _ _ _ buckets _ _ _) =
  [(lo, hi, i, n, net) | SpendingBucket lo hi _ _ (MinorAmount i) n (MinorAmount net) _ _ <- buckets]

-- | Cover exact aggregates, calendar boundaries, UUID ties and user/admin HTTP subject selection.
statisticsTests :: TestTree
statisticsTests =
  testGroup
    "Statistics"
    [ testCase "saved settlement shares, not payer spending or transfers, drive the canonical fixture" $ do
        let jan = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket a fixture
            janBob = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket b fixture
            both = one $ calculate SingleCurrency (d 1 1) (d 2 29) MonthBucket a fixture
        summary jan @?= (20000, 2, 12000, 7000)
        summary janBob @?= (20000, 2, 8000, 13000)
        bucketValues both @?= [(d 1 1, d 1 31, 20000, 2, 12000, 7000), (d 2 1, d 2 29, 4000, 1, 4000, 0)]
        jan.members @?= [MemberSpending b (MinorAmount 8000) (MinorAmount 13000), MemberSpending a (MinorAmount 12000) (MinorAmount 7000), MemberSpending former (MinorAmount 0) (MinorAmount 0)]
        [(rid, amount) | PersonalExpense rid _ _ (MinorAmount amount) <- jan.topExpenses] @?= [(RecordId $ uuid 11, 4000), (RecordId $ uuid 10, 3000)],
      testCase "income adds group totals without changing expense breakdowns or subject selection" $ do
        let rows = income 30 (d 1 25) usd 6000 b [(a, 3000), (b, 3000)] : fixture
            before = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket a fixture
            alice = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket a rows
            bob = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket b rows
            historical = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket former rows
        incomeValues alice @?= (6000, 1, -14000)
        incomeValues bob @?= incomeValues alice
        incomeValues historical @?= incomeValues alice
        summary alice @?= summary before
        summary bob @?= (20000, 2, 8000, 13000)
        summary historical @?= (20000, 2, 0, 0)
        alice.members @?= before.members
        alice.topExpenses @?= before.topExpenses
        alice.otherExpensesMinor @?= before.otherExpensesMinor
        bucketValues alice @?= bucketValues before
        bucketIncomeValues alice @?= [(d 1 1, d 1 31, 6000, 1, -14000)],
      testCase "income-only periods use saved settlement values and retain zero expense detail" $ do
        convertedIncome <- case income 31 (d 1 31) eur 1 b [(a, 1), (b, 0)] of
          record@IncomeRecord {} ->
            pure
              record
                { settlementCurrency = usd,
                  settlementAmountMinor = MinorAmount 100,
                  splits = [RecordSplit a (MinorAmount 1) (MinorAmount 1) (MinorAmount 50), RecordSplit b (MinorAmount 0) (MinorAmount 0) (MinorAmount 50)]
                }
          _ -> assertFailure "Expected an income fixture"
        let rows = [income 32 (d 1 30) usd 999 a [(a, 999)], convertedIncome, income 33 (d 2 2) usd 999 a [(a, 999)], payment]
            section = one $ calculate SingleCurrency (d 1 31) (d 2 1) DayBucket a rows
        summary section @?= (0, 0, 0, 0)
        incomeValues section @?= (100, 1, 100)
        bucketIncomeValues section @?= [(d 1 31, d 1 31, 100, 1, 100), (d 2 1, d 2 1, 0, 0, 0)]
        section.members @?= [MemberSpending a (MinorAmount 0) (MinorAmount 0), MemberSpending b (MinorAmount 0) (MinorAmount 0), MemberSpending former (MinorAmount 0) (MinorAmount 0)]
        section.topExpenses @?= []
        section.otherExpensesMinor @?= MinorAmount 0,
      testCase "income and negative net preserve exact integers in independent currencies" $ do
        let huge = 9223372036854775807
            rows = [income 34 (d 1 1) eur huge a [(b, huge)], income 35 (d 1 1) eur huge b [(a, huge)], expense 36 (d 1 1) usd huge a [(b, huge)], income 37 (d 1 1) usd 1 b [(a, 1)], payment]
            result = calculate MultiCurrency (d 1 1) (d 1 31) MonthBucket a rows
        map summary result.currencies @?= [(0, 0, 0, 0), (huge, 1, huge, 0)]
        map incomeValues result.currencies @?= [(huge * 2, 2, huge * 2), (1, 1, 1 - huge)]
        map bucketIncomeValues result.currencies @?= [[(d 1 1, d 1 31, huge * 2, 2, huge * 2)], [(d 1 1, d 1 31, 1, 1, 1 - huge)]]
        case result.currencies of
          [euro, dollar] -> do
            toJSON euro.incomeMinor @?= toJSON ("18446744073709551614" :: T.Text)
            toJSON dollar.netMinor @?= toJSON ("-9223372036854775806" :: T.Text)
          _ -> assertFailure "Expected separate EUR and USD sections",
      testCase "empty single includes base, zero buckets and historical identities; multi has no invented currency" $ do
        let empty = calculate SingleCurrency (d 2 28) (d 3 1) DayBucket former []
        empty.participants @?= [a, b, former]
        bucketValues (one empty) @?= [(d 2 28, d 2 28, 0, 0, 0, 0), (d 2 29, d 2 29, 0, 0, 0, 0), (d 3 1, d 3 1, 0, 0, 0, 0)]
        incomeValues (one empty) @?= (0, 0, 0)
        bucketIncomeValues (one empty) @?= [(d 2 28, d 2 28, 0, 0, 0), (d 2 29, d 2 29, 0, 0, 0), (d 3 1, d 3 1, 0, 0, 0)]
        (one empty).topExpenses @?= []
        (one empty).otherExpensesMinor @?= MinorAmount 0
        let multi = calculate MultiCurrency (d 2 28) (d 3 1) MonthBucket former []
        multi.currencies @?= []
        multi.participants @?= [a, b, former],
      testCase "Monday weeks cross years and clip both edges with missing periods retained" $ do
        let lo = fromGregorian 2023 12 31
            hi = d 1 16
            rows = [expense 20 lo usd 100 a [(a, 100)], expense 21 (d 1 1) usd 200 b [(a, 200)], expense 22 hi usd 300 a [(b, 300)], expense 23 (d 1 17) usd 999 a [(a, 999)]]
        bucketValues (one $ calculate SingleCurrency lo hi WeekBucket a rows)
          @?= [(lo, lo, 100, 1, 100, 100), (d 1 1, d 1 7, 200, 1, 0, 200), (d 1 8, d 1 14, 0, 0, 0, 0), (d 1 15, hi, 300, 1, 300, 0)],
      testCase "month clipping, multi sections and inclusive date filtering never mix units" $ do
        let records = [expense 1 (d 1 10) eur 10000 a [(b, 10000)], expense 2 (d 1 20) usd 8000 b [(a, 4000), (b, 4000)], payment]
            result = calculate MultiCurrency (d 1 10) (d 2 2) MonthBucket a records
        map (\(CurrencySpending info _ _ _ _ _ _ _ _ _ _ _) -> info) result.currencies @?= [eur, usd]
        map summary result.currencies @?= [(10000, 1, 10000, 0), (8000, 1, 0, 4000)]
        map bucketValues result.currencies
          @?= [[(d 1 10, d 1 31, 10000, 1, 10000, 0), (d 2 1, d 2 2, 0, 0, 0, 0)], [(d 1 10, d 1 31, 8000, 1, 0, 4000), (d 2 1, d 2 2, 0, 0, 0, 0)]],
      testCase "top five ties use descending date then UUID; Other is the exact remaining share" $ do
        let rows = [expense n (d 1 (if n < 4 then 1 else 2)) usd 100 a [(a, 100)] | n <- [1 .. 7]]
            section = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket a rows
        [rid | PersonalExpense rid _ _ _ <- section.topExpenses] @?= map (RecordId . uuid) [4, 5, 6, 7, 1]
        section.otherExpensesMinor @?= MinorAmount 200
        summary section @?= (700, 7, 700, 700),
      testCase "aggregate arithmetic and JSON preserve integers beyond Int64 and JavaScript precision" $ do
        let amount = 9223372036854775807
            section = one $ calculate SingleCurrency (d 1 1) (d 1 31) MonthBucket a [expense 1 (d 1 1) usd amount a [(a, amount)], expense 2 (d 1 2) usd amount a [(a, amount)]]
        summary section @?= (amount * 2, 2, amount * 2, amount * 2)
        toJSON section.totalMinor @?= toJSON ("18446744073709551614" :: T.Text),
      testCase "canonical dates reject normalized, shortened and timestamp input" $ do
        (parseUrlPiece "2024-02-29" :: Either T.Text StatsDate) @?= Right (StatsDate $ d 2 29)
        forM_ ["2024-2-29", "2024-02-30", "2024-01-01T00:00:00Z", " 2024-01-01", "2024-01-01 ", "2024-01-001"] $ \text ->
          case parseUrlPiece text :: Either T.Text StatsDate of Left _ -> pure (); Right _ -> assertFailure $ "Accepted " ++ T.unpack text,
      apiFixture,
      apiIncome,
      apiErrors,
      apiMulti
    ]

apiFixture :: TestTree
apiFixture = testCase "API includes receipt children once, excludes payments, retains former subjects, and updates after edits/deletes" $
  withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env _ _ _ -> do
    ta <- setupUser env "alice"
    tb <- setupUser env "bob"
    aid <- fixtureUserId env "alice"
    bid <- fixtureUserId env "bob"
    adminToken <- setupAdmin env "admin" "adminpass123"
    let tc = mkTC ta
        admin = mkAdminTC adminToken
    group <- runClient env $ cCreateGroup tc "Statistics"
    let groupId = grpId group
    void $ runClient env $ cAddMember tc groupId bid
    rate <- either (assertFailure . T.unpack) pure (parseDecimalRate "1.20")
    let foreignChild = ExpenseRecordRequest "Converted receipt child" Nothing (MinorAmount 10000) (CurrencyCode "EUR") aid (d 1 10) ExactSplit [RecordSplitRequest aid (MinorAmount 2500), RecordSplitRequest bid (MinorAmount 7500)] (Just $ ManualRateInput rate)
        equalExpenseOn day = ExpenseRecordRequest "January" Nothing (MinorAmount 8000) (CurrencyCode "USD") bid day EqualSplit [RecordSplitRequest aid (MinorAmount 1), RecordSplitRequest bid (MinorAmount 1)] Nothing
        equalExpense = equalExpenseOn (d 1 20)
        febExpense = ExpenseRecordRequest "February" Nothing (MinorAmount 4000) (CurrencyCode "USD") aid (d 2 1) EqualSplit [RecordSplitRequest bid (MinorAmount 1)] Nothing
        query person = cGetStatistics tc groupId (StatsDate $ d 1 1) (StatsDate $ d 1 31) Nothing person
    void $ runClient env $ cCreateReceipt tc groupId (CreateReceiptRequest "Receipt" [foreignChild])
    saved <- runClient env $ cAddExpense tc groupId equalExpense
    void $ runClient env $ cAddExpense tc groupId febExpense
    void $ runClient env $ cAddTransfer tc groupId (TransferRecordRequest (MinorAmount 3000) (CurrencyCode "USD") bid aid (d 1 25) Nothing)
    january <- runClient env $ query Nothing
    summary (one january) @?= (20000, 2, 12000, 7000)
    january.bucket @?= MonthBucket
    january.user.userId @?= aid
    bobJanuary <- runClient env $ query (Just bid)
    summary (one bobJanuary) @?= (20000, 2, 8000, 13000)
    reportBefore <- runClient env $ cGetReport tc groupId
    void $ runClient env $ cGetStatistics tc groupId (StatsDate $ d 2 1) (StatsDate $ d 2 29) (Just DayBucket) Nothing
    reportAfter <- runClient env $ cGetReport tc groupId
    reportAfter @?= reportBefore
    void $ runClient env $ cDeleteMember tc groupId bid
    formerStats <- runClient env $ query (Just bid)
    summary (one formerStats) @?= (20000, 2, 8000, 13000)
    sort (map key formerStats.participants) @?= sort [aid, bid]
    adminStats <- runClient env $ cAdminStatistics admin groupId (StatsDate $ d 1 1) (StatsDate $ d 1 31) Nothing Nothing
    adminStats @?= january
    runClientExpectStatus env status403 $ cGetStatistics (mkTC tb) groupId (StatsDate $ d 1 1) (StatsDate $ d 1 31) Nothing Nothing
    -- Re-activate before editing an expense that includes this participant.
    void $ runClient env $ cAddMember tc groupId bid
    void $ runClient env $ cUpdateExpense tc groupId (extractRecordId saved) (equalExpenseOn $ d 2 1)
    changed <- runClient env $ query Nothing
    summary (one changed) @?= (12000, 1, 12000, 3000)
    void $ runClient env $ cDeleteRecord tc groupId (extractRecordId saved)
    afterDelete <- runClient env $ cGetStatistics tc groupId (StatsDate $ d 2 1) (StatsDate $ d 2 29) Nothing Nothing
    summary (one afterDelete) @?= (4000, 1, 4000, 0)

apiIncome :: TestTree
apiIncome = testCase "API income totals survive subject filtering, amount edits, date moves and deletion" $
  withTestAppServer Nothing id $ \env _ _ _ -> do
    token <- setupUser env "alice"
    void $ setupUser env "bob"
    aid <- fixtureUserId env "alice"
    bid <- fixtureUserId env "bob"
    let tc = mkTC token
    group <- runClient env $ cCreateGroup tc "Income statistics"
    let groupId = grpId group
        query month subject = cGetStatistics tc groupId (StatsDate $ d month 1) (StatsDate $ d month (if month == 1 then 31 else 29)) (Just MonthBucket) subject
        request amount day = IncomeRecordRequest "Shared income" Nothing (MinorAmount amount) (CurrencyCode "USD") bid day EqualSplit [RecordSplitRequest aid (MinorAmount 1), RecordSplitRequest bid (MinorAmount 1)] Nothing
    void $ runClient env $ cAddMember tc groupId bid
    void $ runClient env $ cAddExpense tc groupId (ExpenseRecordRequest "Expense" Nothing (MinorAmount 12000) (CurrencyCode "USD") aid (d 1 10) EqualSplit [RecordSplitRequest aid (MinorAmount 1), RecordSplitRequest bid (MinorAmount 1)] Nothing)
    before <- one <$> runClient env (query 1 Nothing)
    saved <- runClient env $ cAddIncome tc groupId (request 6000 $ d 1 31)
    void $ runClient env $ cAddTransfer tc groupId (TransferRecordRequest (MinorAmount 9000) (CurrencyCode "USD") bid aid (d 1 31) Nothing)
    january <- one <$> runClient env (query 1 Nothing)
    bobJanuary <- one <$> runClient env (query 1 $ Just bid)
    summary january @?= (12000, 1, 12000, 6000)
    summary bobJanuary @?= (12000, 1, 0, 6000)
    incomeValues january @?= (6000, 1, -6000)
    incomeValues bobJanuary @?= incomeValues january
    bucketIncomeValues january @?= [(d 1 1, d 1 31, 6000, 1, -6000)]
    january.members @?= before.members
    january.topExpenses @?= before.topExpenses
    january.otherExpensesMinor @?= before.otherExpensesMinor
    bucketValues january @?= bucketValues before
    void $ runClient env $ cUpdateIncome tc groupId (extractRecordId saved) (request 10000 $ d 1 31)
    edited <- one <$> runClient env (query 1 Nothing)
    incomeValues edited @?= (10000, 1, -2000)
    summary edited @?= summary before
    void $ runClient env $ cUpdateIncome tc groupId (extractRecordId saved) (request 10000 $ d 2 1)
    movedJanuary <- one <$> runClient env (query 1 Nothing)
    movedFebruary <- one <$> runClient env (query 2 $ Just bid)
    incomeValues movedJanuary @?= (0, 0, -12000)
    bucketIncomeValues movedJanuary @?= [(d 1 1, d 1 31, 0, 0, -12000)]
    summary movedJanuary @?= summary before
    incomeValues movedFebruary @?= (10000, 1, 10000)
    summary movedFebruary @?= (0, 0, 0, 0)
    bucketIncomeValues movedFebruary @?= [(d 2 1, d 2 29, 10000, 1, 10000)]
    movedFebruary.topExpenses @?= []
    movedFebruary.otherExpensesMinor @?= MinorAmount 0
    void $ runClient env $ cDeleteRecord tc groupId (extractRecordId saved)
    deleted <- one <$> runClient env (query 2 Nothing)
    incomeValues deleted @?= (0, 0, 0)
    summary deleted @?= (0, 0, 0, 0)
    bucketIncomeValues deleted @?= [(d 2 1, d 2 29, 0, 0, 0)]

apiErrors :: TestTree
apiErrors = testCase "API requires canonical bounded filters and enforces group and subject authorization" $
  withTestAppServer Nothing id $ \env port manager _ -> do
    token <- setupUser env "owner"
    outsiderToken <- setupUser env "outsider"
    outsider <- fixtureUserId env "outsider"
    let tc = mkTC token
    group <- runClient env $ cCreateGroup tc "Errors"
    let groupId@(GroupId raw) = grpId group
        query caller lo hi bucket subject = cGetStatistics caller groupId (StatsDate lo) (StatsDate hi) bucket subject
        path = "/groups/" ++ show raw ++ "/statistics"
    runClientExpectStatus env status401 $ query (mkTC "invalid") (d 1 1) (d 1 31) Nothing Nothing
    runClientExpectStatus env status403 $ query (mkTC outsiderToken) (d 1 1) (d 1 31) Nothing Nothing
    runClientExpectStatus env status404 $ query tc (d 1 1) (d 1 31) Nothing (Just outsider)
    runClientExpectStatus env status404 $ cGetStatistics tc (GroupId UUID.nil) (StatsDate $ d 1 1) (StatsDate $ d 1 31) Nothing Nothing
    forM_ [(d 2 1, d 1 1), (fromGregorian 2000 1 1, fromGregorian 2011 1 1)] $ \(lo, hi) -> do
      response <- runClientM (query tc lo hi Nothing Nothing) env
      case response of
        Left (FailureResponse _ result) -> do
          responseStatusCode result @?= status400
          responseBody result @?= "Choose a date range of at most 10 years"
        _ -> assertFailure "Expected invalid date range"
    forM_ ["", "?from=2024-01-01", "?to=2024-01-01", "?from=2024-1-01&to=2024-01-31", "?from=2024-02-30&to=2024-03-01", "?from=2024-01-01T00:00:00Z&to=2024-01-31", "?from=2024-01-01&to=2024-01-31&bucket=year", "?from=2024-01-01&to=2024-01-31&userId=bad"] $ \suffix -> do
      response <- identityRequest port manager (Just token) "GET" (path ++ suffix) Null
      HC.responseStatus response @?= status400
    boundary <- runClient env $ query tc (fromGregorian 2020 1 1) (fromGregorian 2030 1 7) Nothing Nothing
    summary (one boundary) @?= (0, 0, 0, 0)

apiMulti :: TestTree
apiMulti = testCase "API multi currencies remain independent, exact above 2^53 and selectable without range data" $
  withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
    token <- setupUser env "owner"
    otherToken <- setupUser env "member"
    ownerId <- fixtureUserId env "owner"
    memberId <- fixtureUserId env "member"
    adminToken <- setupAdmin env "admin" "adminpass123"
    let tc = mkTC token
        other = mkTC otherToken
        admin = mkAdminTC adminToken
    void $ runClient env $ cUpdateMe tc (UpdateProfileRequest "owner" "Identical label")
    void $ runClient env $ cUpdateMe other (UpdateProfileRequest "member" "Identical label")
    group <- runClient env $ cCreateGroup tc "Independent units"
    let groupId@(GroupId raw) = grpId group
        huge = 9007199254740993
        request currency amount = ExpenseRecordRequest "Exact" Nothing (MinorAmount amount) currency ownerId (d 1 1) ExactSplit [RecordSplitRequest memberId (MinorAmount amount)] Nothing
    response <- identityRequest port manager (Just token) "PUT" ("/groups/" ++ show raw) (toJSON $ UpdateGroupRequest "Independent units" MultiCurrency (CurrencyCode "USD"))
    HC.responseStatus response @?= status200
    void $ runClient env $ cAddMember tc groupId memberId
    void $ runClient env $ cAddExpense tc groupId (request (CurrencyCode "USD") huge)
    void $ runClient env $ cAddExpense tc groupId (request (CurrencyCode "USD") huge)
    void $ runClient env $ cAddExpense tc groupId (request (CurrencyCode "EUR") 10000)
    void $ runClient env $ cAddIncome tc groupId (IncomeRecordRequest "EUR income" Nothing (MinorAmount 5000) (CurrencyCode "EUR") memberId (d 1 1) EqualSplit [RecordSplitRequest ownerId (MinorAmount 1)] Nothing)
    void $ runClient env $ cAddIncome tc groupId (IncomeRecordRequest "USD income" Nothing (MinorAmount huge) (CurrencyCode "USD") memberId (d 1 1) EqualSplit [RecordSplitRequest memberId (MinorAmount 1)] Nothing)
    void $ runClient env $ cDeleteMember tc groupId memberId
    result <- runClient env $ cGetStatistics tc groupId (StatsDate $ d 1 1) (StatsDate $ d 1 1) (Just DayBucket) (Just memberId)
    map summary result.currencies @?= [(10000, 1, 0, 10000), (huge * 2, 2, 0, huge * 2)]
    map incomeValues result.currencies @?= [(5000, 1, -5000), (huge, 1, -huge)]
    map bucketIncomeValues result.currencies @?= [[(d 1 1, d 1 1, 5000, 1, -5000)], [(d 1 1, d 1 1, huge, 1, -huge)]]
    [info.code | CurrencySpending info _ _ _ _ _ _ _ _ _ _ _ <- result.currencies] @?= [CurrencyCode "EUR", CurrencyCode "USD"]
    map (\(User _ _ label) -> label) result.participants @?= ["Identical label", "Identical label"]
    sort (map key result.participants) @?= sort [ownerId, memberId]
    empty <- runClient env $ cGetStatistics tc groupId (StatsDate $ d 2 1) (StatsDate $ d 2 1) Nothing (Just memberId)
    empty.currencies @?= []
    empty.participants @?= result.participants
    empty.user @?= result.user
    adminSelected <- runClient env $ cAdminStatistics admin groupId (StatsDate $ d 1 1) (StatsDate $ d 1 1) (Just DayBucket) (Just memberId)
    adminSelected @?= result
