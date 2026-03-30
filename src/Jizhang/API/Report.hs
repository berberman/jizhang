{-# LANGUAGE BangPatterns #-}
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, toText)
import Jizhang.API.Group
import Jizhang.API.Record (getRecordsByGroupId)
import Jizhang.API.Types
import Jizhang.API.Utils
import Log.Class
import Servant
import Text.Printf (printf)

type ReportAPI = "groups" :> Capture "groupId" GroupId :> "report" :> Get '[JSON] Report

reportServer :: AuthUser -> MyServer ReportAPI
reportServer authUser = settle
  where
    settle (GroupId gId) = do
      logInfo_ $ "Calculating settlement for group " <> toText gId
      ensureGroupMember (authUserId authUser) gId
      getReportByGroupId gId

getReportByGroupId :: UUID -> MyHandler Report
getReportByGroupId gId = do
  ensureGroupExists gId
  Group {members} <- getGroup $ coerce gId
  records <- getRecordsByGroupId gId
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
calculateBalance !users !records = M.mapWithKey (\u (bal, brks) -> Balance u bal (mergeBreakdown brks)) mp
  where
    mp = foldr updateBalance (M.fromList [(u, (0.0, [])) | u <- users]) records
    updateBalance ExpenseRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(!bal, !brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) paidBy
        updateDebtor m = foldr (\RecordSplit {..} -> M.adjust (\(!bal, !brks) -> (bal - splitAmount, BalanceBreakdown recordId title (-splitAmount) : brks)) user) m splits
    updateBalance TransferRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(!bal, !brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) paidBy
        updateDebtor = M.adjust (\(!bal, !brks) -> (bal - amount, BalanceBreakdown recordId title (-amount) : brks)) transferTo
    mergeBreakdown xs =
      M.elems $
        M.fromListWith
          (\(BalanceBreakdown r t a1) (BalanceBreakdown _ _ a2) -> BalanceBreakdown r t (a1 + a2))
          [(recordId, b) | b@BalanceBreakdown {recordId} <- xs]

calculateSettlement :: Map User Balance -> [Settlement]
calculateSettlement balances = go (M.map totalAmount balances) []
  where
    go mp acc
      | M.null creditors || M.null debtors = acc
      | otherwise = go mp' (Settlement {fromUser = dName, toUser = cName, amount = amt} : acc)
      where
        creditors = M.filter (> 0) mp
        debtors = M.filter (< 0) mp
        (cName, cAmount) = maximumBy (compare `on` snd) (M.toList creditors)
        (dName, dAmount) = minimumBy (compare `on` snd) (M.toList debtors)
        amt = min (-dAmount) cAmount
        mp' =
          M.insert cName (cAmount - amt) $
            M.insert dName (dAmount + amt) mp

reportToMarkdown :: Report -> Text
reportToMarkdown (Report grpId bals settles) =
  T.unlines
    [ "# Report for Group: " <> renderGroupId grpId,
      "",
      "## Balances",
      "",
      renderBalances bals,
      "",
      "## Settlements",
      "",
      renderSettlements settles
    ]
  where
    renderSettlements [] = "_Everyone is settled up!_"
    renderSettlements s = T.unlines (renderSettlement <$> s)
    renderBalances [] = "_No balances to report._"
    renderBalances b = T.intercalate "\n\n---\n\n" (map renderBalance b)
    renderBalance (Balance u total bdowns) =
      T.unlines $
        [ "### User: " <> renderUser u,
          "**Total Balance: " <> renderAmount total <> "**",
          ""
        ]
          <> (renderBalanceBreakdown <$> bdowns)
    renderBalanceBreakdown (BalanceBreakdown _ btitle amt) =
      T.unwords
        [ "  *",
          "\"" <> btitle <> "\":",
          renderAmount amt
        ]
    renderSettlement (Settlement from to amt) =
      T.unwords
        [ "*",
          "**" <> renderUser from <> "**",
          "owes",
          "**" <> renderUser to <> "**:",
          renderAmount amt
        ]
    renderAmount = T.pack . printf "$%.2f"
    renderGroupId (GroupId gid) = toText gid
    renderUser (User _ uname) = uname
