{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Dated expense/income summaries from saved settlement amounts; transfers are excluded.
module Jizhang.Report.Statistics (calculateSpendingStatistics) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Jizhang.API.Types

userKey :: User -> UserId
userKey (User key _ _) = key

-- Strict totals prevent large histories accumulating chains of Integer additions.
data Totals = Totals !Integer !Int !Integer !Int !Integer !Integer

zero :: Totals
zero = Totals 0 0 0 0 0 0

plus :: Totals -> Totals -> Totals
plus (Totals a n income incomeN p s) (Totals b m otherIncome otherIncomeN q t) =
  Totals (a + b) (n + m) (income + otherIncome) (incomeN + otherIncomeN) (p + q) (s + t)

-- Per-currency totals, sparse calendar buckets, member paid/share totals and bounded top expenses.
data Acc = Acc !CurrencyInfo !Totals !(M.Map Day Totals) !(M.Map UserId (Integer, Integer)) ![PersonalExpense]

-- Weeks start on Monday; clipping to the requested date range happens in 'bucketIntervals'.
calendarStart :: StatsBucket -> Day -> Day
calendarStart DayBucket day = day
calendarStart WeekBucket day = let (_, _, weekday) = toWeekDate day in addDays (1 - toInteger weekday) day
calendarStart MonthBucket day = let (year, month, _) = toGregorian day in fromGregorian year month 1

nextStart :: StatsBucket -> Day -> Day
nextStart DayBucket = addDays 1
nextStart WeekBucket = addDays 7
nextStart MonthBucket = addGregorianMonthsClip 1

-- Emit inclusive bucket ranges even where no records exist, clipping the first and last.
bucketIntervals :: Day -> Day -> StatsBucket -> [(Day, Day)]
bucketIntervals lo hi step =
  [ (max lo day, min hi $ addDays (-1) $ nextStart step day)
  | day <- takeWhile (<= hi) $ iterate (nextStart step) (calendarStart step lo)
  ]

-- | Summarize an inclusive date range for one subject using the group's complete record history.
-- Callers supply authorized identities and a valid range; this fold performs no access checks.
calculateSpendingStatistics :: GroupId -> CurrencyMode -> CurrencyInfo -> Day -> Day -> StatsBucket -> User -> [User] -> [Record] -> SpendingStatistics
calculateSpendingStatistics gid mode base lo hi step person people records =
  SpendingStatistics gid lo hi step person participants (map section $ M.elems accumulated)
  where
    participants = sortOn (\(User key handle _) -> (handle, key)) people
    empty info = Acc info zero M.empty M.empty []
    initial = case mode of SingleCurrency -> M.singleton (code base) (empty base); MultiCurrency -> M.empty
    accumulated = foldl' addRecord initial records
    addRecord acc ExpenseRecord {recordId, title, date, settlementCurrency = info, settlementAmountMinor = MinorAmount total, paidBy, splits}
      | date >= lo && date <= hi = M.alter (Just . update . fromMaybe (empty info)) (code info) acc
      where
        subjectShare = foldl' (\n (RecordSplit u _ _ (MinorAmount amount)) -> if userKey u == userKey person then n + amount else n) 0 splits
        subjectPaid = if userKey paidBy == userKey person then total else 0
        delta = Totals total 1 0 0 subjectPaid subjectShare
        addMember paid share =
          M.insertWith
            (\(!p, !s) (!q, !t) -> (p + q, s + t))
            (userKey paid)
            (share, 0)
        update (Acc currency totals buckets members top) =
          let paidMembers = addMember paidBy total members
              allMembers = foldl' (\mp (RecordSplit u _ _ (MinorAmount amount)) -> M.insertWith (\(!p, !s) (!q, !t) -> (p + q, s + t)) (userKey u) (0, amount) mp) paidMembers splits
              -- Rank saved subject allocations, not record gross amounts; zero shares have no bar.
              -- Keeping only five after each insertion is safe: discarded entries cannot re-enter a fixed ordering.
              personal = if subjectShare > 0 then take 5 $ sortOn expenseOrder (PersonalExpense recordId title date (MinorAmount subjectShare) : top) else top
              -- Use the clipped interval start as the key, matching bucketIntervals at partial weeks/months.
           in Acc currency (plus totals delta) (M.insertWith plus (max lo $ calendarStart step date) delta buckets) allMembers personal
    addRecord acc IncomeRecord {date, settlementCurrency = info, settlementAmountMinor = MinorAmount income}
      | date >= lo && date <= hi = M.alter (Just . update . fromMaybe (empty info)) (code info) acc
      where
        delta = Totals 0 0 income 1 0 0
        update (Acc currency totals buckets members top) =
          Acc currency (plus totals delta) (M.insertWith plus (max lo $ calendarStart step date) delta buckets) members top
    addRecord acc _ = acc
    expenseOrder (PersonalExpense rid _ day (MinorAmount amount)) = (Down amount, Down day, rid)
    section (Acc info (Totals total count income incomeCount paid share) bucketMap memberMap top) =
      CurrencySpending
        info
        (MinorAmount total)
        count
        (MinorAmount income)
        incomeCount
        (MinorAmount $ income - total)
        (MinorAmount paid)
        (MinorAmount share)
        [ let Totals t n i incomeN p s = M.findWithDefault zero start bucketMap
           in SpendingBucket start end (MinorAmount t) n (MinorAmount i) incomeN (MinorAmount $ i - t) (MinorAmount p) (MinorAmount s)
        | (start, end) <- bucketIntervals lo hi step
        ]
        ( sortOn
            (\(MemberSpending u _ (MinorAmount amount)) -> (Down amount, userKey u))
            [let (p, s) = M.findWithDefault (0, 0) (userKey u) memberMap in MemberSpending u (MinorAmount p) (MinorAmount s) | u <- participants]
        )
        top
        -- The remainder includes every allocated expense omitted from the bounded top-five list.
        (MinorAmount $ share - sum [amount | PersonalExpense _ _ _ (MinorAmount amount) <- top])
