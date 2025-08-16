{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Jizhang.API.Report where

import Data.Coerce (coerce)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Jizhang.API.Group
import Jizhang.API.Record (getRecordsInGroup)
import Jizhang.API.Types
import Jizhang.Common.MyUUID
import Log.Class
import Servant

type ReportAPI = "groups" :> Capture "groupId" GroupId :> "report" :> Get '[JSON] Report

reportServer :: MyServer ReportAPI
reportServer = settle
  where
    settle (GroupId gId) = do
      logInfo_ $ "Calculating settlement for group " <> uuidToText gId
      Group {members} <- getGroup True $ coerce gId
      records <- getRecordsInGroup $ coerce gId
      let balances = calculateBalance members records
          settlements = calculateSettlement balances
      pure
        Report
          { groupId = coerce gId,
            balances = M.elems balances,
            settlements = settlements
          }

-- Records should be in the same group
calculateBalance :: [User] -> [Record] -> Map User Balance
calculateBalance users records = M.mapWithKey (\u (bal, brks) -> Balance u bal brks) mp
  where
    mp = foldr updateBalance (M.fromList [(u, (0.0, [])) | u <- users]) records
    updateBalance ExpenseRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(bal, brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) byUsername
        updateDebtor m = foldr (\RecordSplit {..} -> M.adjust (\(bal, brks) -> (bal - splitAmount, BalanceBreakdown recordId title (-splitAmount) : brks)) username) m splits
    updateBalance TransferRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(bal, brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) byUsername
        updateDebtor = M.adjust (\(bal, brks) -> (bal - amount, BalanceBreakdown recordId title (-amount) : brks)) toUsername

calculateSettlement :: Map User Balance -> [Settlement]
calculateSettlement balances = f (map (\(u, totalAmount -> b) -> (u, b)) $ M.toList balances) []
  where
    f [] mp = mp
    f xs mp = if null creditors || null debtors then mp else f xs' mp'
      where
        creditors = filter ((> 0) . snd) xs
        debtors = filter ((< 0) . snd) xs
        maxCreditor@(cName, cAmount) = maximumBy (compare `on` snd) creditors
        maxDebtor@(dName, dAmount) = minimumBy (compare `on` snd) debtors
        amount = min (-dAmount) cAmount
        mp' = Settlement dName cName amount : mp
        rest = xs \\ [maxCreditor, maxDebtor]
        xs' = rest <> [(cName, cAmount - amount), (dName, dAmount + amount)]
