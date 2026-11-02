{-# LANGUAGE OverloadedStrings #-}

module Jizhang.ReceiptAI.Review
  ( validateReceiptEvidence,
    computeReceiptImportAmounts,
    validateReceiptImportReview,
    receiptReviewWarningToken,
    validReceiptDate,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Jizhang.API.Types.Money
import Jizhang.API.Types.ReceiptAI
import Jizhang.Currency (CurrencyCatalog, lookupCurrency)
import Jizhang.Money (apportionByKey, parseMajorAmount, validateRecordAmount)

validReceiptDate :: Text -> Bool
validReceiptDate value = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack value) :: Maybe Day of
  Just day -> T.length value == 10 && T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day) == value
  Nothing -> False

receiptReviewWarningToken :: ReceiptReviewIssue -> Text
receiptReviewWarningToken warning
  | "model_issue_" `T.isPrefixOf` reviewIssueCode warning = reviewIssueCode warning
  | otherwise = reviewIssueCode warning <> ":" <> reviewIssueField warning

issue :: ReceiptIssueSeverity -> Text -> Text -> Text -> ReceiptReviewIssue
issue severity codeValue field message = ReceiptReviewIssue codeValue field message severity

validateReceiptEvidence :: CurrencyCatalog -> ReceiptEvidence -> [ReceiptReviewIssue]
validateReceiptEvidence catalog = take 400 . receiptEvidenceReviewIssues catalog

-- Original model issues remain in evidence; save checks are not truncated.
receiptEvidenceReviewIssues :: CurrencyCatalog -> ReceiptEvidence -> [ReceiptReviewIssue]
receiptEvidenceReviewIssues catalog evidence =
  currencyIssues
    <> dateIssues
    <> duplicateIssues
    <> concatMap itemIssues (evidenceItems evidence)
    <> concatMap adjustmentIssues (evidenceAdjustments evidence)
    <> totalIssues
    <> tipIssues
    <> modelIssues
  where
    currency = evidenceCurrency evidence
    currencyInfo = do
      codeValue <- maybe (Left "Confirm a catalog currency") Right (evidenceCurrencyCode currency)
      parsed <- parseCurrencyCode codeValue
      lookupCurrency catalog parsed
    currencyIssues =
      [issue ReviewError "currency_required" "currency" "Confirm a supported catalog currency" | isLeft currencyInfo]
        <> [issue ReviewWarning "currency_unconfirmed" "currency" "Currency was inferred or is unknown; confirm it explicitly" | evidenceCurrencyBasis currency /= CurrencyExplicit]
    dateIssues = [issue ReviewError "date_required" "transaction_date" "Confirm an explicit full YYYY-MM-DD date; no year is inferred" | maybe True (not . validReceiptDate) (observationValue $ evidenceTransactionDate evidence)]
    allIds = map evidenceItemId (evidenceItems evidence)
    duplicateIssues =
      [issue ReviewError "item_ids" "items" "Item IDs must be unique lowercase ASCII identifiers" | not (unique allIds) || any (not . validReceiptItemId) allIds]
        <> [issue ReviewError "adjustment_ids" "adjustments" "Adjustment IDs must be unique lowercase ASCII identifiers" | let ids = map evidenceAdjustmentId (evidenceAdjustments evidence), not (unique ids) || any (not . validReceiptItemId) ids]
    moneyIssues required field observation = case observationValue observation of
      Nothing -> [issue ReviewError "amount_required" field "Enter the printed amount or correct this field" | required]
      Just value -> case currencyInfo of
        Left _ -> decimalIssues ReviewError field value
        Right info -> [issue ReviewError "invalid_amount" field "Amount is not a valid nonnegative amount at the selected currency precision" | isLeft (parseEvidenceMoney info value)]
    decimalIssues severity field value = [issue severity "invalid_decimal" field "Enter an ordinary unsigned decimal with at most 18 integer and 18 fractional digits" | isLeft (parseReceiptDecimal value)]
    optionalDecimal field observation = maybe [] (decimalIssues ReviewWarning field) (observationValue observation)
    itemIssues item =
      let field = "items." <> evidenceItemId item
          numeric = moneyIssues True (field <> ".line_total") (evidenceItemLineTotal item)
          mismatch = case (currencyInfo, observationValue $ evidenceItemQuantity item, observationValue $ evidenceItemUnitPrice item, observationValue $ evidenceItemLineTotal item) of
            (Right info, Just quantity, Just price, Just total) -> case (parseReceiptDecimal quantity, parseReceiptDecimal price, parseEvidenceMoney info total) of
              (Right q, Right p, Right (MinorAmount amount)) -> quantityPriceWarnings info field (Just q) (Just p) amount
              _ -> []
            _ -> []
       in numeric
            <> optionalDecimal (field <> ".quantity") (evidenceItemQuantity item)
            <> optionalDecimal (field <> ".unit_price") (evidenceItemUnitPrice item)
            <> mismatch
    adjustmentIssues adjustment =
      let field = "adjustments." <> evidenceAdjustmentId adjustment
       in moneyIssues True (field <> ".amount") (evidenceAdjustmentAmount adjustment)
            <> [issue ReviewError "treatment_required" (field <> ".treatment") "Confirm how this adjustment affects the extracted item amounts" | evidenceAdjustmentTreatment adjustment == TreatmentUnknown]
            <> [issue ReviewError "item_reference" (field <> ".item_id") "Adjustment references an unknown item" | maybe False (`notElem` allIds) (evidenceAdjustmentItemId adjustment)]
            <> [issue ReviewError "item_reference" (field <> ".item_id") "An item-scoped adjustment must identify an item" | evidenceAdjustmentScope adjustment == ScopeItem && not (isJust $ evidenceAdjustmentItemId adjustment)]
            <> [issue ReviewError "invalid_treatment" (field <> ".treatment") "Discounts cannot add and actual tips cannot subtract" | invalidTreatment (evidenceAdjustmentKind adjustment) (evidenceAdjustmentTreatment adjustment)]
    totals = evidenceTotals evidence
    totalIssues = concat [moneyIssues False ("totals." <> field) observation | (field, observation) <- [("subtotal", evidenceSubtotal totals), ("tax_total", evidenceTaxTotal totals), ("printed_total", evidencePrintedTotal totals), ("final_paid", evidenceFinalPaid totals)]]
    tipIssues = [warning {reviewIssueSeverity = ReviewWarning} | (index, tip) <- zip [0 :: Int ..] (evidenceSuggestedTips evidence), warning <- optionalDecimal ("suggested_tips." <> indexText index <> ".rate") (suggestedTipRate tip) <> moneyIssues False ("suggested_tips." <> indexText index <> ".amount") (suggestedTipAmount tip)]
    modelIssues = [issue ReviewWarning ("model_issue_" <> indexText index) (modelIssueField value) (modelIssueReason value) | (index, value) <- zip [0 :: Int ..] (evidenceIssues evidence)]

parseEvidenceMoney :: CurrencyInfo -> Text -> Either Text MinorAmount
parseEvidenceMoney info text = do
  unless (text == T.strip text) $ Left "Money must not contain surrounding whitespace"
  parseMajorAmount info text

indexText :: Int -> Text
indexText = T.pack . show

unique :: (Ord a) => [a] -> Bool
unique values = Set.size (Set.fromList values) == length values

invalidTreatment :: ReceiptAdjustmentKind -> ReceiptAdjustmentTreatment -> Bool
invalidTreatment AdjustmentDiscount TreatmentAdd = True
invalidTreatment AdjustmentActualTip TreatmentSubtract = True
invalidTreatment _ _ = False

roundedMinor :: CurrencyInfo -> Rational -> Integer
roundedMinor info value = floor (value * fromInteger (10 ^ minorUnits info) + 1 % 2)

quantityPriceWarning :: Text -> ReceiptReviewIssue
quantityPriceWarning field = issue ReviewWarning "quantity_price_mismatch" field "Quantity times unit price differs from the printed extended amount; use the confirmed extended amount"

quantityPriceWarnings :: CurrencyInfo -> Text -> Maybe Rational -> Maybe Rational -> Integer -> [ReceiptReviewIssue]
quantityPriceWarnings info field quantity price amount = case (*) <$> quantity <*> price of
  Just productValue -> [quantityPriceWarning field | roundedMinor info productValue /= amount]
  Nothing -> []

requireAcknowledgements :: ReceiptImportReview -> [ReceiptReviewIssue] -> Either Text ()
requireAcknowledgements review warnings = mapM_ check warnings
  where
    check warning =
      unless (receiptReviewWarningToken warning `elem` reviewAcknowledgedWarnings review) $
        Left ("Acknowledge warning " <> receiptReviewWarningToken warning)

computeReceiptImportAmounts :: CurrencyCatalog -> ReceiptImportReview -> Either Text [(Text, MinorAmount)]
computeReceiptImportAmounts catalog review = do
  -- Constructors are exported for API composition, so enforce wire bounds on direct callers too.
  _ <- first (const "Invalid or oversized receipt review") (parseEither (const $ validateReviewBounds review) ())
  info <- lookupCurrency catalog (reviewCurrency review)
  unless (reviewConfirmed review) $ Left "Confirm the receipt review"
  unless (validReceiptDate $ reviewDate review) $ Left "Confirm an explicit full YYYY-MM-DD date"
  unless (reviewTotalMinor review > MinorAmount 0) $ Left "Confirmed total must be positive"
  case reviewTotalBasis review of
    TotalPrinted -> unless (reviewPrintedTotalMinor review == Just (reviewTotalMinor review)) $ Left "Printed total must equal the confirmed total"
    TotalFinalPaid -> unless (reviewFinalPaidMinor review == Just (reviewTotalMinor review)) $ Left "Final paid amount must equal the confirmed total"
    TotalUserEntered -> pure ()
  validateTip review
  case reviewMode review of
    ReviewTotal -> do
      unless (null (reviewItems review) && null (reviewAdjustments review)) $ Left "Total mode must retain empty item and adjustment lists"
      when (T.null $ T.strip $ reviewTotalTitle review) $ Left "Enter a total expense title"
      _ <- validateRecordAmount (reviewTotalMinor review)
      pure [(reviewTotalTitle review, reviewTotalMinor review)]
    ReviewItemized -> do
      let items = reviewItems review
          adjustments = reviewAdjustments review
          keys = map reviewItemId items
      when (null items) $ Left "Itemized mode requires at least one item"
      unless (unique keys) $ Left "Item IDs must be unique"
      unless (unique $ map reviewAdjustmentId adjustments) $ Left "Adjustment IDs must be unique"
      numericWarnings <- concat <$> mapM (validateItem info) items
      mapM_ (validateAdjustment keys) adjustments
      requireAcknowledgements review numericWarnings
      let original = [(reviewItemId item, minorAmountInteger $ reviewItemAmountMinor item) | item <- items]
      discounted <- foldM (\bases -> applyAdjustment keys bases bases) (Map.fromList original) [adjustment | adjustment <- adjustments, reviewAdjustmentTreatment adjustment == TreatmentSubtract]
      -- All external additions share the frozen discounted bases, not previous fees.
      final <- foldM (applyAdjustment keys discounted) discounted [adjustment | adjustment <- adjustments, reviewAdjustmentTreatment adjustment == TreatmentAdd]
      let results = [(reviewItemTitle item, MinorAmount amount) | item <- items, let amount = Map.findWithDefault 0 (reviewItemId item) final, amount /= 0]
      mapM_ (validateRecordAmount . snd) results
      unless (sum (map (minorAmountInteger . snd) results) == minorAmountInteger (reviewTotalMinor review)) $ Left "Itemized amounts must exactly equal the confirmed total"
      pure results
  where
    validateItem info item = do
      when (T.null $ T.strip $ reviewItemTitle item) $ Left ("Enter a title for item " <> reviewItemId item)
      let parseOptional = traverse (first (const $ "Invalid optional decimal for item " <> reviewItemId item) . parseReceiptDecimal)
      quantity <- parseOptional (reviewItemQuantity item)
      price <- parseOptional (reviewItemUnitPrice item)
      pure (quantityPriceWarnings info ("items." <> reviewItemId item) quantity price (minorAmountInteger $ reviewItemAmountMinor item))
    validateAdjustment keys adjustment = do
      let treatment = reviewAdjustmentTreatment adjustment
          eligible = reviewAdjustmentItemIds adjustment
      when (invalidTreatment (reviewAdjustmentKind adjustment) treatment) $ Left "Discounts cannot add and actual tips cannot subtract"
      unless (unique eligible && all (`elem` keys) eligible) $ Left "Adjustment eligibility must contain unique existing item IDs"
      when (treatment `elem` [TreatmentAdd, TreatmentSubtract] && null eligible) $ Left "Confirm a nonempty eligible item set for every external adjustment"
    applyAdjustment keys bases values adjustment = do
      let eligible = reviewAdjustmentItemIds adjustment
          weights = [(key, Map.findWithDefault 0 key bases) | key <- keys, key `elem` eligible]
          baseTotal = sum (map snd weights)
          amount = minorAmountInteger (reviewAdjustmentAmountMinor adjustment)
          subtracting = reviewAdjustmentTreatment adjustment == TreatmentSubtract
      unless (not (null weights) && baseTotal > 0) $ Left "Adjustment requires positive eligible item bases"
      when (subtracting && amount > baseTotal) $ Left "Discount exceeds eligible item bases"
      let applyShare current (key, share) =
            let signedShare = if subtracting then negate share else share
             in Map.adjust (+ signedShare) key current
      pure (foldl' applyShare values (apportionByKey amount weights))

validateTip :: ReceiptImportReview -> Either Text ()
validateTip review = case reviewTipDecision review of
  TipNone -> do
    unless (reviewActualTipMinor review == Just (MinorAmount 0)) $ Left "No tip requires actualTipMinor=0"
    when (any ((/= MinorAmount 0) . reviewAdjustmentAmountMinor) tips) $ Left "No tip forbids nonzero actual-tip adjustments"
  TipConfirmed -> do
    tip <- maybe (Left "Confirm the actual tip amount, already included in the total") Right (reviewActualTipMinor review)
    when (tip > reviewTotalMinor review) $ Left "Actual tip exceeds the confirmed total"
    when (reviewMode review == ReviewItemized && minorAmountInteger tip /= sum [minorAmountInteger (reviewAdjustmentAmountMinor adjustment) | adjustment <- tips]) $
      Left "Actual tip must equal the canonical actual-tip adjustments represented once"
  TipBillOnly -> do
    unless (reviewActualTipMinor review == Nothing && reviewFinalPaidMinor review == Nothing) $ Left "Bill-only review must leave actual tip and final payment unknown"
    when (reviewTotalBasis review == TotalFinalPaid) $ Left "Bill-only review cannot use final paid as its total basis"
    when (any ((== TreatmentAdd) . reviewAdjustmentTreatment) tips) $ Left "Bill-only review cannot add an actual tip"
    unless ("bill_only:tipDecision" `elem` reviewAcknowledgedWarnings review) $ Left "Confirm: Record bill amount only; actual tip is unknown"
  where
    tips = filter ((== AdjustmentActualTip) . reviewAdjustmentKind) (reviewAdjustments review)

-- Original numeric errors are not save gates: corrected review fields are authoritative.
validateReceiptImportReview :: CurrencyCatalog -> Maybe ReceiptEvidence -> ReceiptImportReview -> Either Text [(Text, MinorAmount)]
validateReceiptImportReview catalog evidence review = do
  amounts <- computeReceiptImportAmounts catalog review
  case evidence of
    Nothing -> pure ()
    Just original -> requireAcknowledgements review $ filter relevant $ receiptEvidenceReviewIssues catalog original
  pure amounts
  where
    relevant warning =
      reviewIssueSeverity warning == ReviewWarning
        && ("model_issue_" `T.isPrefixOf` reviewIssueCode warning || "suggested_tips." `T.isPrefixOf` reviewIssueField warning)
        && (reviewMode review == ReviewItemized || not (any (`T.isPrefixOf` reviewIssueField warning) ["items", "adjustments"]))
