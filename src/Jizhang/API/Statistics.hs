{-# LANGUAGE OverloadedStrings #-}

-- | Spending-statistics handlers share range validation and stored-record projection.
-- Members default to their own statistics; operators default to the group's owner.
module Jizhang.API.Statistics (getStatistics, getAdminStatistics) where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time (diffDays)
import Jizhang.API.App
import Jizhang.API.Group.Projection (storedGroupSettings)
import Jizhang.API.Internal.Error (fromStoredData)
import Jizhang.API.Internal.Resource (ensureGroupExists, ensureGroupMember, getGroupUserMap)
import Jizhang.API.Record.Service (projectRecordRows)
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.API.Types (AuthAdmin (authAdminId), AuthUser (authUserId), GroupId (GroupId), SpendingStatistics, StatsBucket (MonthBucket), StatsDate (StatsDate), UserId (UserId))
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Jizhang.Report.Statistics (calculateSpendingStatistics)
import Jizhang.RequestProtection (Principal (AdminPrincipal, UserPrincipal))
import Servant

-- | Require group membership even when selecting another historical participant.
getStatistics :: AuthUser -> GroupId -> StatsDate -> StatsDate -> Maybe StatsBucket -> Maybe UserId -> MyHandler SpendingStatistics
getStatistics actor gid@(GroupId uuid) lo hi selectedBucket subject = do
  enforceExpensiveLimit (UserPrincipal $ authUserId actor)
  ensureGroupExists uuid
  ensureGroupMember (authUserId actor) uuid
  getStatisticsForGroup gid (Just $ UserId $ authUserId actor) lo hi selectedBucket subject

-- | Operator access bypasses membership but still requires a valid group and subject.
getAdminStatistics :: AuthAdmin -> GroupId -> StatsDate -> StatsDate -> Maybe StatsBucket -> Maybe UserId -> MyHandler SpendingStatistics
getAdminStatistics actor gid lo hi selectedBucket subject = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId actor)
  getStatisticsForGroup gid Nothing lo hi selectedBucket subject

-- | Limit inclusive ranges to 3660 days and load split records only; the subject
-- must exist in group history, and omitted buckets default to calendar months.
getStatisticsForGroup :: GroupId -> Maybe UserId -> StatsDate -> StatsDate -> Maybe StatsBucket -> Maybe UserId -> MyHandler SpendingStatistics
getStatisticsForGroup gid@(GroupId uuid) defaultSubject (StatsDate lo) (StatsDate hi) selectedBucket subject = do
  stored <- runDB (D.getGroupWithMembers uuid) >>= maybe (throwError err404) (pure . fst)
  when (lo > hi || diffDays hi lo + 1 > 3660) $
    throwError $
      err400 {errBody = "Choose a date range of at most 10 years"}
  catalog <- asks appCurrencies
  (mode, base) <- fromStoredData $ storedGroupSettings catalog stored
  people <- getGroupUserMap uuid
  let selected = fromMaybe (fromMaybe (UserId $ S.unUserId $ S._groupOwner stored) defaultSubject) subject
  person <- maybe (throwError err404) pure $ M.lookup (let UserId key = selected in key) people
  rows <- runDB $ D.getSplitRecordsWithSplitsInRange uuid lo hi
  records <- projectRecordRows people rows
  pure $ calculateSpendingStatistics gid mode base lo hi (fromMaybe MonthBucket selectedBucket) person (M.elems people) records
