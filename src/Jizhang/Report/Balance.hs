{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | All-time, currency-separated balances from saved allocations, with review-only settlements.
module Jizhang.Report.Balance
  ( calculateReport,
    calculateBalance,
    calculateSettlement,
    zeroBalances,
  )
where

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import Jizhang.API.Types
import qualified Jizhang.API.Types.Report as Report (BalanceBreakdown (amountMinor))

-- | All-time accounting. Spending date filters must not be applied here.
calculateReport :: GroupId -> CurrencyMode -> CurrencyInfo -> [User] -> [Record] -> Report
calculateReport gid mode base users records = Report gid sections
  where
    currencyInfo =
      foldl'
        (\mp record -> let info = recordCurrency record in M.insert (code info) info mp)
        (case mode of SingleCurrency -> M.singleton (code base) base; MultiCurrency -> M.empty)
        records
    byCurrency = calculateBalance users records
    sections =
      [ CurrencyReport info (M.elems balances) (calculateSettlement balances)
      | (currencyCode, info) <- M.toAscList currencyInfo,
        let balances = M.findWithDefault (zeroBalances users) currencyCode byCurrency
      ]

recordCurrency :: Record -> CurrencyInfo
recordCurrency ExpenseRecord {settlementCurrency = info} = info
recordCurrency TransferRecord {settlementCurrency = info} = info
recordCurrency IncomeRecord {settlementCurrency = info} = info

-- | Seed every supplied identity, including members without activity, with zero role totals.
zeroBalances :: [User] -> Map UserId Balance
zeroBalances users = M.fromList [(uid person, zero person) | person <- users]
  where
    zero person =
      Balance
        { user = person,
          totalMinor = MinorAmount 0,
          expensesPaidMinor = MinorAmount 0,
          expenseShareMinor = MinorAmount 0,
          incomeReceivedMinor = MinorAmount 0,
          incomeEntitlementMinor = MinorAmount 0,
          paymentsSentMinor = MinorAmount 0,
          paymentsReceivedMinor = MinorAmount 0,
          breakdown = []
        }

uid :: User -> UserId
uid (User value _ _) = value

-- | Fold one group's saved settlement allocations, never current exchange rates.
-- Supply all current and historical identities; absent users receive no balance entry.
calculateBalance :: [User] -> [Record] -> Map CurrencyCode (Map UserId Balance)
calculateBalance !users = M.map (M.map mergeBreakdown) . foldl' updateCurrency M.empty
  where
    updateCurrency mp record =
      M.alter
        (Just . updateBalance record . maybe (zeroBalances users) id)
        (code (recordCurrency record))
        mp
    add (MinorAmount a) (MinorAmount b) = MinorAmount (a + b)
    -- Apply roles cumulatively: the same identity may pay and share without losing either role total.
    adjust target contribution role =
      M.adjust
        ( \balance@Balance {totalMinor, breakdown} ->
            (role balance)
              { totalMinor = add totalMinor (impact contribution),
                breakdown = contribution : breakdown
              }
        )
        (uid target)
    impact BalanceBreakdown {amountMinor} = amountMinor
    entry rid label day kind gross delta =
      BalanceBreakdown
        { recordId = rid,
          title = label,
          date = day,
          recordType = kind,
          recordAmountMinor = MinorAmount gross,
          amountMinor = MinorAmount delta
        }
    updateBalance ExpenseRecord {recordId, title, date, settlementAmountMinor = MinorAmount total, paidBy, splits} mp =
      let contribution = entry recordId title date "expense" total
       in foldl'
            ( \balances (RecordSplit person _ _ allocation@(MinorAmount allocated)) ->
                adjust
                  person
                  (contribution (-allocated))
                  (\balance@Balance {expenseShareMinor} -> balance {expenseShareMinor = add expenseShareMinor allocation})
                  balances
            )
            ( adjust
                paidBy
                (contribution total)
                (\balance@Balance {expensesPaidMinor} -> balance {expensesPaidMinor = add expensesPaidMinor (MinorAmount total)})
                mp
            )
            splits
    updateBalance TransferRecord {recordId, title, date, settlementAmountMinor = MinorAmount total, paidBy, transferTo} mp =
      let contribution = entry recordId title date "transfer" total
       in adjust
            transferTo
            (contribution (-total))
            (\balance@Balance {paymentsReceivedMinor} -> balance {paymentsReceivedMinor = add paymentsReceivedMinor (MinorAmount total)})
            ( adjust
                paidBy
                (contribution total)
                (\balance@Balance {paymentsSentMinor} -> balance {paymentsSentMinor = add paymentsSentMinor (MinorAmount total)})
                mp
            )
    updateBalance IncomeRecord {recordId, title, date, settlementAmountMinor = MinorAmount total, receivedBy, splits} mp =
      let contribution = entry recordId title date "income" total
       in foldl'
            ( \balances (RecordSplit person _ _ allocation@(MinorAmount allocated)) ->
                adjust
                  person
                  (contribution allocated)
                  (\balance@Balance {incomeEntitlementMinor} -> balance {incomeEntitlementMinor = add incomeEntitlementMinor allocation})
                  balances
            )
            ( adjust
                receivedBy
                (contribution (-total))
                (\balance@Balance {incomeReceivedMinor} -> balance {incomeReceivedMinor = add incomeReceivedMinor (MinorAmount total)})
                mp
            )
            splits
    -- A payer can also have a share: merge impacts, but retain the record gross amount once.
    mergeBreakdown balance@Balance {breakdown} =
      balance
        { breakdown =
            sortOn order $
              M.elems $
                M.fromListWith combine [(rid, value) | value@BalanceBreakdown {recordId = rid} <- breakdown]
        }
    order BalanceBreakdown {date, recordId} = (Down date, recordId)
    combine :: BalanceBreakdown -> BalanceBreakdown -> BalanceBreakdown
    combine first@BalanceBreakdown {amountMinor = a} BalanceBreakdown {amountMinor = b} =
      first {Report.amountMinor = add a b}

-- | Largest creditor/debtor first, with ascending immutable UUID ties. Every
-- suggestion fits the record storage bound even if aggregate balances do not.
calculateSettlement :: Map UserId Balance -> [Settlement]
calculateSettlement balances = go (M.map (\Balance {user, totalMinor = MinorAmount total} -> (user, total)) balances)
  where
    go mp = case (creditors, debtors) of
      ((cId, (cUser, cAmount)) : _, (dId, (dUser, dAmount)) : _) ->
        -- Aggregate debt may exceed Int64; emit bounded records and leave the excess for another pass.
        let amount = min (toInteger (maxBound :: Int64)) (min cAmount (-dAmount))
            next = M.insert cId (cUser, cAmount - amount) $ M.insert dId (dUser, dAmount + amount) mp
         in Settlement dUser cUser (MinorAmount amount) : go next
      _ -> []
      where
        (creditors, debtors) = M.foldlWithKey' select ([], []) mp
        select (creditor, debtor) userId value@(_, amount)
          | amount > 0 = (choose (\(key, (_, total)) -> (Down total, key)) (userId, value) creditor, debtor)
          | amount < 0 = (creditor, choose (\(key, (_, total)) -> (total, key)) (userId, value) debtor)
          | otherwise = (creditor, debtor)
        choose _ candidate [] = [candidate]
        choose order candidate current@(best : _)
          | order candidate < order best = [candidate]
          | otherwise = current
