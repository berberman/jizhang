{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReportTest (reportTests) where

import qualified Data.Map.Strict as M
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Jizhang.API.Report (calculateBalance, calculateSettlement)
import Jizhang.API.Types
import Test.Tasty
import Test.Tasty.HUnit
import Utils

-- | A fixed epoch time for tests
epoch :: UTCTime
epoch = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

alice, bob, charlie :: User
alice = User (UserId (mkUUID 201)) "alice"
bob = User (UserId (mkUUID 202)) "bob"
charlie = User (UserId (mkUUID 203)) "charlie"

-- Deterministic UUIDs for testing
mkUUID :: Int -> UUID
mkUUID n = read $ "00000000-0000-0000-0000-" ++ replicate (12 - length (show n)) '0' ++ show n

rid1, rid2 :: RecordId
rid1 = RecordId (mkUUID 1)
rid2 = RecordId (mkUUID 2)

gid1 :: GroupId
gid1 = GroupId (mkUUID 100)

reportTests :: TestTree
reportTests =
  testGroup
    "Report"
    [ testGroup "calculateBalance" balanceTests,
      testGroup "calculateSettlement" settlementTests
    ]

balanceTests :: [TestTree]
balanceTests =
  [ testCase "no records => all balances zero" $ do
      let bals = calculateBalance [alice, bob] []
      assertEqual "alice balance" 0.0 (totalAmount $ bals M.! alice)
      assertEqual "bob balance" 0.0 (totalAmount $ bals M.! bob),
    testCase "single expense, even split between payer and one other" $ do
      -- Alice pays $30, split evenly between Alice and Bob (1 share each)
      let expense =
            ExpenseRecord
              { recordId = rid1,
                title = "Lunch",
                amount = 30.0,
                paidBy = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 15.0,
                    RecordSplit bob 1 15.0
                  ]
              }
          bals = calculateBalance [alice, bob] [expense]
      -- Alice: +30 (paid) -15 (her share) = +15
      assertApproxEqual "alice" 15.0 (totalAmount $ bals M.! alice)
      -- Bob: -15 (his share)
      assertApproxEqual "bob" (-15.0) (totalAmount $ bals M.! bob),
    testCase "single expense, uneven shares" $ do
      -- Bob pays $100, shares: Alice=1, Bob=2, Charlie=1 (total 4)
      -- splitAmounts: Alice=$25, Bob=$50, Charlie=$25
      let expense =
            ExpenseRecord
              { recordId = rid1,
                title = "Dinner",
                amount = 100.0,
                paidBy = bob,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 25.0,
                    RecordSplit bob 2 50.0,
                    RecordSplit charlie 1 25.0
                  ]
              }
          bals = calculateBalance [alice, bob, charlie] [expense]
      -- Alice: -25
      assertApproxEqual "alice" (-25.0) (totalAmount $ bals M.! alice)
      -- Bob: +100 - 50 = +50
      assertApproxEqual "bob" 50.0 (totalAmount $ bals M.! bob)
      -- Charlie: -25
      assertApproxEqual "charlie" (-25.0) (totalAmount $ bals M.! charlie),
    testCase "transfer record" $ do
      -- Bob transfers $20 to Alice
      let transfer =
            TransferRecord
              { recordId = rid1,
                title = "Transfer",
                amount = 20.0,
                paidBy = bob,
                transferTo = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1
              }
          bals = calculateBalance [alice, bob] [transfer]
      -- Bob: +20 (creditor, he paid)
      assertApproxEqual "bob" 20.0 (totalAmount $ bals M.! bob)
      -- Alice: -20 (debtor, she received)
      assertApproxEqual "alice" (-20.0) (totalAmount $ bals M.! alice),
    testCase "multiple records accumulate" $ do
      -- Record 1: Alice pays $30, split evenly (Alice, Bob)
      -- Record 2: Bob pays $20, split evenly (Alice, Bob)
      let r1 =
            ExpenseRecord
              { recordId = rid1,
                title = "Lunch",
                amount = 30.0,
                paidBy = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 15.0,
                    RecordSplit bob 1 15.0
                  ]
              }
          r2 =
            ExpenseRecord
              { recordId = rid2,
                title = "Coffee",
                amount = 20.0,
                paidBy = bob,
                date = read "2025-06-02",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 10.0,
                    RecordSplit bob 1 10.0
                  ]
              }
          bals = calculateBalance [alice, bob] [r1, r2]
      -- Alice: +30 -15 -10 = +5
      assertApproxEqual "alice" 5.0 (totalAmount $ bals M.! alice)
      -- Bob: -15 +20 -10 = -5
      assertApproxEqual "bob" (-5.0) (totalAmount $ bals M.! bob),
    testCase "breakdown merges entries for same record (payer is also splitter)" $ do
      -- Alice pays $30 split with Bob. Alice is both creditor (+30) and debtor (-15).
      -- The breakdown for Alice on rid1 should merge to net +15.
      let expense =
            ExpenseRecord
              { recordId = rid1,
                title = "Lunch",
                amount = 30.0,
                paidBy = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 15.0,
                    RecordSplit bob 1 15.0
                  ]
              }
          bals = calculateBalance [alice, bob] [expense]
          aliceBrk = breakdown $ bals M.! alice
      -- Alice should have exactly 1 merged breakdown entry for rid1
      assertEqual "alice breakdown count" 1 (length aliceBrk)
      let BalanceBreakdown {amount = brkAmt} = head aliceBrk
      assertApproxEqual "alice breakdown amount" 15.0 brkAmt,
    testCase "user with no records still appears with zero balance" $ do
      let expense =
            ExpenseRecord
              { recordId = rid1,
                title = "Lunch",
                amount = 30.0,
                paidBy = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 15.0,
                    RecordSplit bob 1 15.0
                  ]
              }
          bals = calculateBalance [alice, bob, charlie] [expense]
      assertApproxEqual "charlie" 0.0 (totalAmount $ bals M.! charlie)
      assertEqual "charlie breakdown" [] (breakdown $ bals M.! charlie),
    testCase "mergeBreakdown handles non-adjacent entries for same record" $ do
      -- Alice pays $60, split among Alice(1), Bob(1), Charlie(1)
      -- splitAmount = 20 each
      -- For Alice: creditor entry (+60) and debtor entry (-20) for rid1
      -- These two breakdown entries for the same record may be non-adjacent
      -- if another record intervenes. We test with two records where Alice
      -- is payer and splitter in both.
      let r1 =
            ExpenseRecord
              { recordId = rid1,
                title = "Lunch",
                amount = 60.0,
                paidBy = alice,
                date = read "2025-06-01",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 20.0,
                    RecordSplit bob 1 20.0,
                    RecordSplit charlie 1 20.0
                  ]
              }
          r2 =
            ExpenseRecord
              { recordId = rid2,
                title = "Dinner",
                amount = 90.0,
                paidBy = alice,
                date = read "2025-06-02",
                createdAt = epoch,
                groupId = gid1,
                splits =
                  [ RecordSplit alice 1 30.0,
                    RecordSplit bob 1 30.0,
                    RecordSplit charlie 1 30.0
                  ]
              }
          bals = calculateBalance [alice, bob, charlie] [r1, r2]
          aliceBrk = breakdown $ bals M.! alice
      -- Alice has two records, each with a creditor and debtor entry.
      -- After merging, there should be exactly 2 breakdown entries (one per record).
      assertEqual "alice breakdown count" 2 (length aliceBrk)
      -- rid1: +60 - 20 = +40, rid2: +90 - 30 = +60
      let brkMap = M.fromList [(recordId, amount) | BalanceBreakdown {..} <- aliceBrk]
      assertApproxEqual "rid1 net" 40.0 (brkMap M.! rid1)
      assertApproxEqual "rid2 net" 60.0 (brkMap M.! rid2)
      -- Total should be 100
      assertApproxEqual "alice total" 100.0 (totalAmount $ bals M.! alice)
  ]

settlementTests :: [TestTree]
settlementTests =
  [ testCase "all zero balances => no settlements" $ do
      let bals =
            M.fromList
              [ (alice, Balance alice 0 []),
                (bob, Balance bob 0 [])
              ]
      assertEqual "no settlements" [] (calculateSettlement bals),
    testCase "two users, one owes the other" $ do
      let bals =
            M.fromList
              [ (alice, Balance alice 15 []),
                (bob, Balance bob (-15) [])
              ]
          settlements = calculateSettlement bals
      assertEqual "one settlement" 1 (length settlements)
      let Settlement {..} = head settlements
      assertEqual "from" bob fromUser
      assertEqual "to" alice toUser
      assertApproxEqual "amount" 15.0 amount,
    testCase "three users, minimal settlement" $ do
      -- Alice: +50, Bob: -30, Charlie: -20
      let bals =
            M.fromList
              [ (alice, Balance alice 50 []),
                (bob, Balance bob (-30) []),
                (charlie, Balance charlie (-20) [])
              ]
          settlements = calculateSettlement bals
      -- Should produce 2 settlements (Bob -> Alice, Charlie -> Alice)
      assertEqual "two settlements" 2 (length settlements)
      let total = sum [amount | Settlement {..} <- settlements]
      assertApproxEqual "total settled" 50.0 total,
    testCase "balances sum to zero invariant" $ do
      -- After settlement, net effect should zero out all balances
      let bals =
            M.fromList
              [ (alice, Balance alice 40 []),
                (bob, Balance bob (-25) []),
                (charlie, Balance charlie (-15) [])
              ]
          settlements = calculateSettlement bals
          -- Compute net flows: positive = receiving money, negative = paying
          flows =
            M.fromListWith (+) $
              concatMap
                (\Settlement {..} -> [(toUser, amount), (fromUser, -amount)])
                settlements
      -- Each user's flow should equal their original balance
      assertApproxEqual "alice flow" 40.0 (M.findWithDefault 0 alice flows)
      assertApproxEqual "bob flow" (-25.0) (M.findWithDefault 0 bob flows)
      assertApproxEqual "charlie flow" (-15.0) (M.findWithDefault 0 charlie flows),
    testCase "four users, complex case" $ do
      -- Alice: +30, Bob: +10, Charlie: -25, Dave: -15
      let dave = User (UserId (mkUUID 204)) "dave"
          bals =
            M.fromList
              [ (alice, Balance alice 30 []),
                (bob, Balance bob 10 []),
                (charlie, Balance charlie (-25) []),
                (dave, Balance dave (-15) [])
              ]
          settlements = calculateSettlement bals
          -- Check the invariant: sum of all balances should be 0
          flows =
            M.fromListWith (+) $
              concatMap
                (\Settlement {..} -> [(toUser, amount), (fromUser, -amount)])
                settlements
      assertApproxEqual "alice flow" 30.0 (M.findWithDefault 0 alice flows)
      assertApproxEqual "bob flow" 10.0 (M.findWithDefault 0 bob flows)
      assertApproxEqual "charlie flow" (-25.0) (M.findWithDefault 0 charlie flows)
      assertApproxEqual "dave flow" (-15.0) (M.findWithDefault 0 dave flows)
      -- Greedy algorithm should need at most n-1 settlements
      assertBool "at most 3 settlements" (length settlements <= 3),
    testCase "settlement with fractional amounts preserves invariant" $ do
      -- Amounts that produce repeating decimals
      let bals =
            M.fromList
              [ (alice, Balance alice 33.33 []),
                (bob, Balance bob (-11.11) []),
                (charlie, Balance charlie (-22.22) [])
              ]
          settlements = calculateSettlement bals
          flows =
            M.fromListWith (+) $
              concatMap
                (\Settlement {..} -> [(toUser, amount), (fromUser, -amount)])
                settlements
      assertApproxEqual "alice flow" 33.33 (M.findWithDefault 0 alice flows)
      assertApproxEqual "bob flow" (-11.11) (M.findWithDefault 0 bob flows)
      assertApproxEqual "charlie flow" (-22.22) (M.findWithDefault 0 charlie flows)
  ]
