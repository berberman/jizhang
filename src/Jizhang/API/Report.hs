{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Settlement and spending-statistics routes over validated group ledger history.
-- Member handlers authorize access before invoking shared report construction.
module Jizhang.API.Report
  ( ReportAPI,
    reportServer,
    getReportByGroupId,
  )
where

import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.UUID (UUID, toText)
import Jizhang.API.Record.Service (getRecordsByGroupId)
import Jizhang.API.Statistics (getStatistics)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (UserPrincipal))
import Jizhang.API.Types (AuthUser (authUserId), GroupId (GroupId), Report, StatisticsEndpoint)
import Jizhang.API.Internal.Error (fromStoredData)
import Jizhang.API.Group.Projection (storedGroupSettings)
import Jizhang.API.Internal.Resource (getGroupUserMap, ensureGroupMember)
import qualified Jizhang.Database as D
import Jizhang.Report.Balance (calculateReport)
import Log.Class
import Servant

-- | Group-scoped reads consume expensive-operation capacity rather than changing records.
type ReportAPI =
  ("groups" :> Capture "groupId" GroupId :> "report" :> Get '[JSON] Report)
    :<|> ("groups" :> Capture "groupId" GroupId :> StatisticsEndpoint)

reportServer :: AuthUser -> MyServer ReportAPI
reportServer authUser = settle :<|> getStatistics authUser
  where
    settle (GroupId gId) = do
      enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
      logInfo_ $ "Calculating settlement for group " <> toText gId
      ensureGroupMember (authUserId authUser) gId
      getReportByGroupId gId

-- | Shared report loader with no membership check; include historical participants
-- so balances remain attributable after a member has left the group.
getReportByGroupId :: UUID -> MyHandler Report
getReportByGroupId gId = do
  stored <- runDB (D.getGroupWithMembers gId) >>= maybe (throwError err404) (pure . fst)
  catalog <- asks appCurrencies
  (mode, base) <- fromStoredData $ storedGroupSettings catalog stored
  members <- M.elems <$> getGroupUserMap gId
  records <- getRecordsByGroupId gId
  pure $ calculateReport (coerce gId) mode base members records
