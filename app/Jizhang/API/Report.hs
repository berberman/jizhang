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
import Data.List (groupBy, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Jizhang.API.Group
import Jizhang.API.Record (getRecordsInGroup)
import Jizhang.API.Types
import Jizhang.Common.MyUUID
import Log.Class
import Servant
import Text.Printf (printf)

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
calculateBalance !users !records = M.mapWithKey (\u (bal, brks) -> Balance u bal (mergeBreakdown brks)) mp
  where
    mp = foldr updateBalance (M.fromList [(u, (0.0, [])) | u <- users]) records
    updateBalance ExpenseRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(!bal, !brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) byUsername
        updateDebtor m = foldr (\RecordSplit {..} -> M.adjust (\(!bal, !brks) -> (bal - splitAmount, BalanceBreakdown recordId title (-splitAmount) : brks)) username) m splits
    updateBalance TransferRecord {..} = updateDebtor . updateCreditor
      where
        updateCreditor = M.adjust (\(!bal, !brks) -> (bal + amount, BalanceBreakdown recordId title amount : brks)) byUsername
        updateDebtor = M.adjust (\(!bal, !brks) -> (bal - amount, BalanceBreakdown recordId title (-amount) : brks)) toUsername
    mergeBreakdown xs =
      let merge (BalanceBreakdown {recordId = r, title = t, amount = a1}) (BalanceBreakdown {amount = a2}) =
            BalanceBreakdown {recordId = r, title = t, amount = a1 + a2}
       in [foldr1 merge g | g <- groupBy (\BalanceBreakdown {recordId = r1} BalanceBreakdown {recordId = r2} -> r1 == r2) xs]

calculateSettlement :: Map User Balance -> [Settlement]
calculateSettlement balances = f (map (\(u, totalAmount -> b) -> (u, b)) $ M.toList balances) []
  where
    f [] mp = mp
    f !xs !mp = if null creditors || null debtors then mp else f xs' mp'
      where
        creditors = filter ((> 0) . snd) xs
        debtors = filter ((< 0) . snd) xs
        maxCreditor@(cName, cAmount) = maximumBy (compare `on` snd) creditors
        maxDebtor@(dName, dAmount) = minimumBy (compare `on` snd) debtors
        amount = min (-dAmount) cAmount
        mp' = Settlement dName cName amount : mp
        rest = xs \\ [maxCreditor, maxDebtor]
        xs' = rest <> [(cName, cAmount - amount), (dName, dAmount + amount)]

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
    renderBalance (Balance user total bdowns) =
      T.unlines $
        [ "### User: " <> renderUser user,
          "**Total Balance: " <> renderAmount total <> "**",
          ""
        ]
          <> (renderBalanceBreakdown <$> bdowns)
    renderBalanceBreakdown (BalanceBreakdown _ title amt) =
      T.unwords
        [ "  *",
          "\"" <> title <> "\":",
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
    renderGroupId (GroupId gid) = uuidToText gid
    renderUser (User u) = u
