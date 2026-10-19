{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Plain-text Markdown export of an already calculated report, preserving currency sections.
module Jizhang.Report.Markdown (reportToMarkdown) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Jizhang.API.Types
import Jizhang.Money (formatMoney)

-- | Render saved report values without recalculating balances or changing settlement order.
-- User text is inserted verbatim; this export is not an HTML-safe rendering boundary.
reportToMarkdown :: Report -> Text
reportToMarkdown (Report (GroupId gid) sections) =
  T.unlines $ ["# Report for Group: " <> toText gid, ""] <> concatMap renderCurrency sections
  where
    renderCurrency (CurrencyReport info balances settlements) =
      [ "## " <> currencyCodeText (code info),
        "",
        "### Balances",
        "",
        if null balances then "_No balances to report._" else T.intercalate "\n\n---\n\n" (map (renderBalance info) balances),
        "",
        "### Settlements",
        "",
        if null settlements then "_Everyone is settled up!_" else T.unlines (map (renderSettlement info) settlements),
        ""
      ]
    renderBalance info Balance {..} =
      T.unlines $
        [ "#### User: " <> renderUser user,
          "**Total Balance: " <> formatMoney info totalMinor <> "**",
          "",
          "* Expenses paid: " <> formatMoney info expensesPaidMinor,
          "* Expense share: " <> formatMoney info expenseShareMinor,
          "* Income received: " <> formatMoney info incomeReceivedMinor,
          "* Income entitlement: " <> formatMoney info incomeEntitlementMinor,
          "* Payments sent: " <> formatMoney info paymentsSentMinor,
          "* Payments received: " <> formatMoney info paymentsReceivedMinor,
          ""
        ]
          <> map (renderEntry info) breakdown
    renderEntry info BalanceBreakdown {..} =
      "  * "
        <> T.pack (show date)
        <> " · "
        <> recordType
        <> " · \""
        <> title
        <> "\" · Record amount: "
        <> formatMoney info recordAmountMinor
        <> " · Balance impact: "
        <> (if minorAmountInteger amountMinor > 0 then "+" else "")
        <> formatMoney info amountMinor
    renderSettlement info (Settlement from to amount) =
      "* **" <> renderUser from <> "** owes **" <> renderUser to <> "**: " <> formatMoney info amount
    renderUser (User _ handle displayName) = "@" <> handle <> " · " <> displayName
