{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Balance, settlement and rendering regressions over saved minor-unit allocations.
-- Cases preserve currency isolation, current UUID identities and exact per-record reconciliation.
module ReportTest (reportTests) where

import Data.Aeson (Result (..), Value (String), fromJSON, object, toJSON, (.=))
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Jizhang.API.Types hiding (totalMinor)
import Jizhang.API.Types.Report (Balance (totalMinor))
import Jizhang.Report.Balance (calculateBalance, calculateReport, calculateSettlement, zeroBalances)
import Jizhang.Report.Markdown (reportToMarkdown)
import Test.Tasty
import Test.Tasty.HUnit
import Utils (userIdOf)

-- Equal display labels must not merge balances belonging to different UUIDs.
alice, bob, charlie :: User
alice = User (UserId (mkUUID 201)) "alice" "Alex"
bob = User (UserId (mkUUID 202)) "bob" "Alex"
charlie = User (UserId (mkUUID 203)) "charlie" "Charlie"

mkUUID :: Int -> UUID
mkUUID n = read $ "00000000-0000-0000-0000-" ++ replicate (12 - length (show n)) '0' ++ show n

gid :: GroupId
gid = GroupId (mkUUID 100)

usd, eur, jpy :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "USD") "US Dollar" 2
eur = CurrencyInfo (CurrencyCode "EUR") "Euro" 2
jpy = CurrencyInfo (CurrencyCode "JPY") "Yen" 0

-- | Build a saved exact-split expense with identical source and settlement amounts.
-- Allocations are already resolved; these fixtures do not exercise split rounding.
expense :: Int -> CurrencyInfo -> Integer -> User -> [(User, Integer)] -> Record
expense n info total payer allocations =
  ExpenseRecord
    { recordId = RecordId (mkUUID n),
      title = "Dinner",
      amountMinor = MinorAmount total,
      note = Nothing,
      currency = info,
      settlementAmountMinor = MinorAmount total,
      settlementCurrency = info,
      exchangeRate = Nothing,
      paidBy = payer,
      date = fromGregorian 2025 6 1,
      createdAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0),
      groupId = gid,
      splitMode = ExactSplit,
      splits = [RecordSplit person (MinorAmount amount) (MinorAmount amount) (MinorAmount amount) | (person, amount) <- allocations]
    }

-- | Model saved income with recipient and beneficiary roles kept independently observable.
income :: Int -> CurrencyInfo -> Integer -> User -> [(User, Integer)] -> Record
income n info total recipient allocations =
  IncomeRecord
    { recordId = RecordId (mkUUID n),
      title = "Shared income",
      amountMinor = MinorAmount total,
      note = Nothing,
      currency = info,
      settlementAmountMinor = MinorAmount total,
      settlementCurrency = info,
      exchangeRate = Nothing,
      receivedBy = recipient,
      date = fromGregorian 2025 6 1,
      createdAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0),
      groupId = gid,
      splitMode = ExactSplit,
      splits = [RecordSplit person (MinorAmount amount) (MinorAmount amount) (MinorAmount amount) | (person, amount) <- allocations]
    }

-- | Build a same-currency payment whose effect is confined to its own currency ledger.
payment :: Int -> CurrencyInfo -> Integer -> User -> User -> Record
payment n info total payer recipient =
  TransferRecord
    { recordId = RecordId (mkUUID n),
      title = "Payment",
      amountMinor = MinorAmount total,
      note = Nothing,
      currency = info,
      settlementAmountMinor = MinorAmount total,
      settlementCurrency = info,
      exchangeRate = Nothing,
      paidBy = payer,
      transferTo = recipient,
      date = fromGregorian 2025 6 2,
      createdAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0),
      groupId = gid
    }

-- | Look up a required roster member; a missing balance is a fixture or calculation failure.
balanceOf :: User -> M.Map UserId Balance -> MinorAmount
balanceOf person balances = case balances M.! userIdOf person of Balance {totalMinor} -> totalMinor

emptyBalance :: User -> Balance
emptyBalance person = zeroBalances [person] M.! userIdOf person

-- | State expected gross record value separately from the signed impact on one member.
entry :: Int -> T.Text -> Day -> T.Text -> Integer -> Integer -> BalanceBreakdown
entry n label day kind gross delta =
  BalanceBreakdown
    { recordId = RecordId (mkUUID n),
      title = label,
      date = day,
      recordType = kind,
      recordAmountMinor = MinorAmount gross,
      amountMinor = MinorAmount delta
    }

roles :: Balance -> [MinorAmount]
roles Balance {expensesPaidMinor, expenseShareMinor, incomeReceivedMinor, incomeEntitlementMinor, paymentsSentMinor, paymentsReceivedMinor} =
  [expensesPaidMinor, expenseShareMinor, incomeReceivedMinor, incomeEntitlementMinor, paymentsSentMinor, paymentsReceivedMinor]

-- | Require both six-role accounting and per-record impacts to reproduce the exact net balance.
assertReconciled :: Balance -> Assertion
assertReconciled Balance {totalMinor, expensesPaidMinor, expenseShareMinor, incomeReceivedMinor, incomeEntitlementMinor, paymentsSentMinor, paymentsReceivedMinor, breakdown} = do
  assertEqual
    "roles reconcile to net"
    (minorAmountInteger totalMinor)
    ( minorAmountInteger expensesPaidMinor
        - minorAmountInteger expenseShareMinor
        - minorAmountInteger incomeReceivedMinor
        + minorAmountInteger incomeEntitlementMinor
        + minorAmountInteger paymentsSentMinor
        - minorAmountInteger paymentsReceivedMinor
    )
  assertEqual
    "record impacts reconcile to net"
    (minorAmountInteger totalMinor)
    (sum [minorAmountInteger amountMinor | BalanceBreakdown {amountMinor} <- breakdown])

-- | Check gross roles, zero-sum nets, saved conversion shares and deterministic report output.
reportTests :: TestTree
reportTests =
  testGroup
    "Report"
    [ testCase "expense income and payment roles reconcile without doubling gross records" $ do
        let records =
              [ income 2 usd 6000 bob [(alice, 3000), (bob, 3000)],
                payment 3 usd 2000 bob alice,
                expense 1 usd 9000 alice [(alice, 3000), (bob, 6000)]
              ]
            balances = calculateBalance [alice, bob] records M.! code usd
            a = balances M.! userIdOf alice
            b = balances M.! userIdOf bob
        assertEqual "Alice six gross roles" (map MinorAmount [9000, 3000, 0, 3000, 0, 2000]) (roles a)
        assertEqual "Bob six gross roles" (map MinorAmount [0, 6000, 6000, 3000, 2000, 0]) (roles b)
        assertEqual "net balances" (map MinorAmount [7000, -7000]) [balanceOf alice balances, balanceOf bob balances]
        assertEqual "review settlement" [Settlement bob alice (MinorAmount 7000)] (calculateSettlement balances)
        let expectedEntries first second third =
              [ entry 3 "Payment" (fromGregorian 2025 6 2) "transfer" 2000 first,
                entry 1 "Dinner" (fromGregorian 2025 6 1) "expense" 9000 second,
                entry 2 "Shared income" (fromGregorian 2025 6 1) "income" 6000 third
              ]
        assertEqual "Alice three distinct records and saved gross amounts" (expectedEntries (-2000) 6000 3000) (breakdown a)
        assertEqual "Bob three distinct records and saved gross amounts" (expectedEntries 2000 (-6000) (-3000)) (breakdown b)
        mapM_ assertReconciled (M.elems balances),
      testCase "zero net self roles keep activity and gross amounts exactly once" $ do
        let balances = calculateBalance [alice] [expense 1 usd 9000 alice [(alice, 9000)], income 2 usd 6000 alice [(alice, 6000)]] M.! code usd
            a = balances M.! userIdOf alice
        assertEqual "gross activity remains" (map MinorAmount [9000, 9000, 6000, 6000, 0, 0]) (roles a)
        assertEqual
          "zero net records retained"
          [ entry 1 "Dinner" (fromGregorian 2025 6 1) "expense" 9000 0,
            entry 2 "Shared income" (fromGregorian 2025 6 1) "income" 6000 0
          ]
          (breakdown a)
        assertEqual "self funded and received is settled" [] (calculateSettlement balances)
        assertReconciled a,
      testCase "former member history remains in all-time role totals" $ do
        -- The report roster includes inactive members from getGroupUserMap.
        let historical = User (userIdOf bob) "bob" "Former member"
        case calculateReport
          gid
          SingleCurrency
          usd
          [alice, historical]
          [expense 1 usd 9000 alice [(alice, 3000), (historical, 6000)]] of
          Report _ [CurrencyReport _ balances settlements] -> do
            case [memberBalance | memberBalance@Balance {user} <- balances, userIdOf user == userIdOf bob] of
              former@Balance {user = formerIdentity} : _ -> do
                assertEqual "historical identity preserved" historical formerIdentity
                assertEqual "former member share is not discarded" (map MinorAmount [0, 6000, 0, 0, 0, 0]) (roles former)
              [] -> assertFailure "expected former member balance"
            assertEqual "former member settlement remains reviewable" [Settlement historical alice (MinorAmount 6000)] settlements
            mapM_ assertReconciled balances
          _ -> assertFailure "expected one currency report",
      testCase "renamed UUID history uses current identities and exact breakdowns" $ do
        let oldAlice = User (userIdOf alice) "old.alice" "Before"
            oldBob = User (userIdOf bob) "old.bob" "Before"
            records = [expense 1 usd 9000 oldAlice [(oldAlice, 3000), (oldBob, 6000)], payment 2 usd 1000 oldBob oldAlice]
            balances = calculateBalance [alice, bob, charlie] records M.! code usd
        assertEqual "credit" (MinorAmount 5000) (balanceOf alice balances)
        assertEqual "debt" (MinorAmount (-5000)) (balanceOf bob balances)
        assertEqual "inactive participant remains" (emptyBalance charlie) (balances M.! userIdOf charlie)
        assertEqual "current identities" [Settlement bob alice (MinorAmount 5000)] (calculateSettlement balances)
        case balances M.! userIdOf alice of
          Balance {breakdown = entries} ->
            assertEqual
              "payer and share merged by record, date descending"
              [entry 2 "Payment" (fromGregorian 2025 6 2) "transfer" 1000 (-1000), entry 1 "Dinner" (fromGregorian 2025 6 1) "expense" 9000 6000]
              entries,
      testCase "independent currencies never net and payment affects only its currency" $ do
        let records = [expense 1 usd 10000 alice [(alice, 5000), (bob, 5000)], expense 2 eur 8000 bob [(alice, 4000), (bob, 4000)]]
            before = calculateBalance [alice, bob] records
            afterPayment = calculateBalance [alice, bob] (payment 3 usd 5000 bob alice : records)
        assertEqual "USD debt" [Settlement bob alice (MinorAmount 5000)] (calculateSettlement (before M.! code usd))
        assertEqual "EUR opposite debt" [Settlement alice bob (MinorAmount 4000)] (calculateSettlement (before M.! code eur))
        assertEqual "USD paid" [] (calculateSettlement (afterPayment M.! code usd))
        assertEqual "EUR unchanged" (before M.! code eur) (afterPayment M.! code eur),
      testCase "converted expense uses independently persisted settlement allocations" $ do
        case expense 1 jpy 1 alice [(alice, 1), (bob, 0)] of
          original@ExpenseRecord {} -> do
            let converted =
                  original
                    { settlementCurrency = usd,
                      settlementAmountMinor = MinorAmount 100,
                      splits = [RecordSplit alice (MinorAmount 1) (MinorAmount 1) (MinorAmount 50), RecordSplit bob (MinorAmount 1) (MinorAmount 0) (MinorAmount 50)]
                    }
                balances = calculateBalance [alice, bob] [converted]
            assertEqual "only settlement currency" [code usd] (M.keys balances)
            assertEqual "rounded zero yen share still owes fifty cents" [Settlement bob alice (MinorAmount 50)] (calculateSettlement (balances M.! code usd))
            let a = balances M.! code usd M.! userIdOf alice
                b = balances M.! code usd M.! userIdOf bob
            assertEqual "expense gross uses settlement amount" (map MinorAmount [100, 50, 0, 0, 0, 0]) (roles a)
            assertEqual "zero source share retains saved allocation" (map MinorAmount [0, 50, 0, 0, 0, 0]) (roles b)
            assertEqual "breakdown gross is saved settlement amount" [entry 1 "Dinner" (fromGregorian 2025 6 1) "expense" 100 50] (breakdown a)
            mapM_ assertReconciled [a, b]
          _ -> assertFailure "expected expense fixture",
      testCase "multiple expenses merge exact per-record contributions and sum to zero" $ do
        let records = [expense 1 usd 6000 alice [(alice, 2000), (bob, 2000), (charlie, 2000)], expense 2 usd 9000 alice [(alice, 3000), (bob, 3000), (charlie, 3000)]]
            balances = calculateBalance [alice, bob, charlie] records M.! code usd
        assertEqual "credit" (MinorAmount 10000) (balanceOf alice balances)
        assertEqual "each debtor" [MinorAmount (-5000), MinorAmount (-5000)] (map (`balanceOf` balances) [bob, charlie])
        assertEqual "zero sum" 0 (sum [minorAmountInteger totalMinor | Balance {totalMinor} <- M.elems balances])
        case balances M.! userIdOf alice of
          Balance {breakdown = entries} ->
            assertEqual
              "one net contribution per expense, UUID ties ascending"
              [entry 1 "Dinner" (fromGregorian 2025 6 1) "expense" 6000 4000, entry 2 "Dinner" (fromGregorian 2025 6 1) "expense" 9000 6000]
              entries,
      testCase "shared income credits beneficiaries and merges recipient entitlement" $ do
        let balances = calculateBalance [alice, bob] [income 1 usd 6000 bob [(alice, 3000), (bob, 3000)]] M.! code usd
        assertEqual "Alice entitlement" (MinorAmount 3000) (balanceOf alice balances)
        assertEqual "Bob owes Alice" [Settlement bob alice (MinorAmount 3000)] (calculateSettlement balances)
        let expected =
              (emptyBalance bob)
                { totalMinor = MinorAmount (-3000),
                  incomeReceivedMinor = MinorAmount 6000,
                  incomeEntitlementMinor = MinorAmount 3000,
                  breakdown = [entry 1 "Shared income" (fromGregorian 2025 6 1) "income" 6000 (-3000)]
                }
        assertEqual "recipient net entry" expected (balances M.! userIdOf bob),
      testCase "income recipient need not be a beneficiary" $ do
        let balances = calculateBalance [alice, bob, charlie] [income 1 usd 6000 charlie [(alice, 3000), (bob, 3000)]] M.! code usd
        assertEqual "entitlements and recipient debt" [MinorAmount 3000, MinorAmount 3000, MinorAmount (-6000)] (map (`balanceOf` balances) [alice, bob, charlie])
        assertEqual "zero sum" 0 (sum [minorAmountInteger totalMinor | Balance {totalMinor} <- M.elems balances]),
      testCase "converted income uses persisted entitlement even with zero source share" $ do
        let converted =
              IncomeRecord
                { recordId = RecordId (mkUUID 1),
                  title = "Converted income",
                  amountMinor = MinorAmount 1,
                  note = Nothing,
                  currency = jpy,
                  settlementAmountMinor = MinorAmount 100,
                  settlementCurrency = usd,
                  exchangeRate = Nothing,
                  receivedBy = bob,
                  date = fromGregorian 2025 6 1,
                  createdAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0),
                  groupId = gid,
                  splitMode = EqualSplit,
                  splits = [RecordSplit alice (MinorAmount 1) (MinorAmount 0) (MinorAmount 50), RecordSplit bob (MinorAmount 1) (MinorAmount 1) (MinorAmount 50)]
                }
            balances = calculateBalance [alice, bob] [converted]
        assertEqual "settlement currency only" [code usd] (M.keys balances)
        assertEqual "zero source still receives entitlement" [Settlement bob alice (MinorAmount 50)] (calculateSettlement (balances M.! code usd))
        assertEqual "income receiver and entitlement use settlement amounts" (map MinorAmount [0, 0, 100, 50, 0, 0]) (roles (balances M.! code usd M.! userIdOf bob))
        mapM_ assertReconciled (M.elems (balances M.! code usd)),
      testCase "empty single report includes base and every member while multi is empty" $ do
        assertEqual "empty currency map" M.empty (calculateBalance [alice, bob] [])
        assertEqual
          "single zeros"
          (Report gid [CurrencyReport usd [emptyBalance alice, emptyBalance bob] []])
          (calculateReport gid SingleCurrency usd [bob, alice] [])
        assertEqual "multi empty" (Report gid []) (calculateReport gid MultiCurrency usd [alice, bob] []),
      testCase "currency sections sorted by code" $ do
        let Report _ sections =
              calculateReport
                gid
                MultiCurrency
                usd
                [alice, bob]
                [expense 1 usd 10000 alice [(bob, 10000)], expense 2 eur 8000 bob [(alice, 8000)]]
        assertEqual "EUR then USD" [eur, usd] [info | CurrencyReport info _ _ <- sections],
      testCase "settlement ties use ascending UUID and repay every balance exactly" $ do
        let dave = User (UserId (mkUUID 204)) "dave" "Dave"
            balances = M.fromList [(userIdOf person, (emptyBalance person) {totalMinor = MinorAmount total}) | (person, total) <- [(alice, 50), (bob, 50), (charlie, -50), (dave, -50)]]
            settlements = calculateSettlement balances
        assertEqual "UUID ties" [Settlement charlie alice (MinorAmount 50), Settlement dave bob (MinorAmount 50)] settlements
        let remaining =
              foldl
                (\mp (Settlement from to (MinorAmount amount)) -> M.adjust (+ amount) (userIdOf from) $ M.adjust (subtract amount) (userIdOf to) mp)
                (M.map (\Balance {totalMinor = MinorAmount amount} -> amount) balances)
                settlements
        assertEqual "no residual" (M.map (const 0) remaining) remaining,
      testCase "aggregate above Int64 emits recordable suggestions without losing precision" $ do
        let bound = toInteger (maxBound :: Int64)
            balances = calculateBalance [alice, bob] [expense 1 usd bound alice [(bob, bound)], expense 2 usd bound alice [(bob, bound)]] M.! code usd
        assertEqual "aggregate exact" (MinorAmount (2 * bound)) (balanceOf alice balances)
        assertEqual "two bounded payments" [Settlement bob alice (MinorAmount bound), Settlement bob alice (MinorAmount bound)] (calculateSettlement balances)
        assertEqual "wire stays string above JS precision" (String "18446744073709551614") (toJSON (balanceOf alice balances))
        let a = balances M.! userIdOf alice
            b = balances M.! userIdOf bob
        assertEqual "gross paid also exceeds Int64 exactly" (map MinorAmount [2 * bound, 0, 0, 0, 0, 0]) (roles a)
        assertEqual "gross share also exceeds Int64 exactly" (map MinorAmount [0, 2 * bound, 0, 0, 0, 0]) (roles b)
        mapM_ assertReconciled [a, b],
      testCase "expanded balance JSON preserves exact gross roles and requires new fields" $ do
        let balances =
              calculateBalance
                [alice, bob]
                [expense 1 usd 9000 alice [(alice, 3000), (bob, 6000)], income 2 usd 6000 bob [(alice, 3000), (bob, 3000)], payment 3 usd 2000 bob alice]
                M.! code usd
            expected =
              object
                [ "user" .= alice,
                  "totalMinor" .= String "7000",
                  "expensesPaidMinor" .= String "9000",
                  "expenseShareMinor" .= String "3000",
                  "incomeReceivedMinor" .= String "0",
                  "incomeEntitlementMinor" .= String "3000",
                  "paymentsSentMinor" .= String "0",
                  "paymentsReceivedMinor" .= String "2000",
                  "breakdown"
                    .= [ object
                           [ "recordId" .= RecordId (mkUUID 3),
                             "title" .= String "Payment",
                             "date" .= String "2025-06-02",
                             "recordType" .= String "transfer",
                             "recordAmountMinor" .= String "2000",
                             "amountMinor" .= String "-2000"
                           ],
                         object
                           [ "recordId" .= RecordId (mkUUID 1),
                             "title" .= String "Dinner",
                             "date" .= String "2025-06-01",
                             "recordType" .= String "expense",
                             "recordAmountMinor" .= String "9000",
                             "amountMinor" .= String "6000"
                           ],
                         object
                           [ "recordId" .= RecordId (mkUUID 2),
                             "title" .= String "Shared income",
                             "date" .= String "2025-06-01",
                             "recordType" .= String "income",
                             "recordAmountMinor" .= String "6000",
                             "amountMinor" .= String "3000"
                           ]
                       ]
                ]
        assertEqual "public financial wire values" expected (toJSON (balances M.! userIdOf alice))
        case fromJSON (object ["user" .= alice, "totalMinor" .= String "0", "breakdown" .= ([] :: [Value])]) :: Result Balance of
          Error _ -> pure ()
          Success _ -> assertFailure "missing required gross fields accepted"
        case fromJSON (object ["recordId" .= RecordId (mkUUID 1), "title" .= String "Dinner", "amountMinor" .= String "0"]) :: Result BalanceBreakdown of
          Error _ -> pure ()
          Success _ -> assertFailure "missing required breakdown metadata accepted",
      testCase "one minor unit settles exactly and zeros emit no payment" $ do
        assertEqual "one unit" [Settlement bob alice (MinorAmount 1)] (calculateSettlement (M.fromList [(userIdOf alice, (emptyBalance alice) {totalMinor = MinorAmount 1}), (userIdOf bob, (emptyBalance bob) {totalMinor = MinorAmount (-1)})]))
        assertEqual "settled" [] (calculateSettlement (M.singleton (userIdOf alice) (emptyBalance alice))),
      testCase "markdown retains currency precision and separate sections" $ do
        let rendered = reportToMarkdown (Report gid [CurrencyReport usd [] [Settlement bob alice (MinorAmount 1234)], CurrencyReport jpy [] [Settlement alice bob (MinorAmount 123)]])
        assertBool "USD units" ("USD 12.34" `T.isInfixOf` rendered)
        assertBool "JPY units" ("JPY 123" `T.isInfixOf` rendered)
    ]
