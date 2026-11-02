{-# LANGUAGE OverloadedStrings #-}

module ReceiptImportTest (receiptImportTests) where

import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (nil)
import Jizhang.API.Types.Money
import qualified Jizhang.API.Types.Receipt as Receipt
import Jizhang.API.Types.ReceiptAI
import Jizhang.API.Types.Admin (AdminCreateReceiptRequest)
import Jizhang.API.Types.ReceiptSummary (ReceiptSummary (..), ReceiptId (..), normalizeReceiptTitle)
import Jizhang.Currency (CurrencyCatalog)
import Jizhang.ReceiptAI.Contract
import Jizhang.ReceiptAI.Review
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

catalog :: CurrencyCatalog
catalog = Map.fromList [(CurrencyCode codeValue, CurrencyInfo (CurrencyCode codeValue) codeValue digits) | (codeValue, digits) <- [("USD", 2), ("JPY", 0), ("KWD", 3)]]

item :: Text -> Integer -> ReceiptReviewItem
item key amount = ReceiptReviewItem key key (MinorAmount amount) Nothing Nothing Nothing

adjustment :: Text -> ReceiptAdjustmentKind -> Integer -> ReceiptAdjustmentTreatment -> [Text] -> ReceiptReviewAdjustment
adjustment key kind amount treatment eligible = ReceiptReviewAdjustment key kind (MinorAmount amount) treatment eligible

review :: ReceiptImportReview
review = ReceiptImportReview ReviewItemized "Synthetic merchant" "Receipt expense" (CurrencyCode "USD") "2026-10-21" TotalPrinted (Just $ MinorAmount 2970) Nothing (MinorAmount 2970) TipNone (Just $ MinorAmount 0)
  [item "a" 1000, item "b" 2000]
  [adjustment "discount" AdjustmentDiscount 300 TreatmentSubtract ["a", "b"], adjustment "tax" AdjustmentTax 270 TreatmentAdd ["a", "b"]]
  [] True

totalReview :: ReceiptImportReview
totalReview = review {reviewMode = ReviewTotal, reviewItems = [], reviewAdjustments = []}

withTotal :: Integer -> ReceiptImportReview -> ReceiptImportReview
withTotal amount value = value {reviewTotalMinor = MinorAmount amount, reviewPrintedTotalMinor = Just $ MinorAmount amount}

visible :: Text -> ReceiptObservation
visible text = ReceiptObservation (Just text) (Just text) ObservationVisible

absent :: ReceiptObservation
absent = ReceiptObservation Nothing Nothing ObservationAbsent

syntheticItem :: ReceiptEvidenceItem
syntheticItem = ReceiptEvidenceItem "a" "Synthetic item" "Synthetic item 29.70" absent absent absent (visible "29.70") PriceUnknown [] []

evidence :: ReceiptEvidence
evidence = ReceiptEvidence RetailReceipt (visible "Synthetic 商店") (visible "2026-10-21") (ReceiptEvidenceCurrency (Just "USD") (Just "$") CurrencyExplicit (Just "USD printed"))
  [syntheticItem]
  [] (ReceiptEvidenceTotals absent absent (visible "29.70") absent) [] []

manualSource :: ReceiptSourceInput
manualSource = ReceiptSourceInput (ReceiptImageInput "image/jpeg" "/9j/2Q==") Nothing Nothing Nothing

modelSource :: ReceiptSourceInput
modelSource = manualSource {receiptSourceExtractionId = Just nil, receiptSourceEvidence = Just evidence, receiptSourceReview = Just review}

assertAmounts :: ReceiptImportReview -> [(Text, Integer)] -> Assertion
assertAmounts value expected = computeReceiptImportAmounts catalog value @?= Right [(title, MinorAmount amount) | (title, amount) <- expected]

assertInvalid :: ReceiptImportReview -> Assertion
assertInvalid value = assertBool "Invalid review is rejected" (isLeft $ computeReceiptImportAmounts catalog value)

assertRoundTrip :: (Eq a, FromJSON a, ToJSON a) => a -> Assertion
assertRoundTrip value = assertBool "Typed JSON round trip" (eitherDecode (encode value) == Right value)

changeField :: Text -> Value -> Value -> Value
changeField key value (Object fields) = Object (KM.insert (Key.fromText key) value fields)
changeField _ _ value = value

dropField :: Text -> Value -> Value
dropField key (Object fields) = Object (KM.delete (Key.fromText key) fields)
dropField _ value = value

receiptImportTests :: TestTree
receiptImportTests = testGroup "Receipt import"
  [ testGroup "Exact review arithmetic"
      [ testCase "discount then proportional tax conserves 29.70" $ assertAmounts review [("a", 990), ("b", 1980)],
        testCase "net lines do not subtract an included discount twice" $ assertAmounts (review {reviewItems = [item "a" 900, item "b" 1800], reviewAdjustments = [adjustment "discount" AdjustmentDiscount 300 TreatmentIncluded [], adjustment "tax" AdjustmentTax 270 TreatmentAdd ["a", "b"]]}) [("a", 990), ("b", 1980)],
        testCase "remainder uses stable item IDs and preserves display order" $ do
          let value = withTotal 3 review {reviewItems = [item "b" 1, item "a" 1], reviewAdjustments = [adjustment "tax" AdjustmentTax 1 TreatmentAdd ["b", "a"]]}
          assertAmounts value [("b", 1), ("a", 2)]
          assertAmounts (value {reviewItems = reverse (reviewItems value)}) [("a", 2), ("b", 1)],
        testCase "selected tax touches only eligible items" $ assertAmounts (review {reviewAdjustments = [adjustment "discount" AdjustmentDiscount 300 TreatmentSubtract ["a", "b"], adjustment "tax" AdjustmentTax 270 TreatmentAdd ["b"]]}) [("a", 900), ("b", 2070)],
        testCase "added fees do not change subsequent fee weights" $ assertAmounts (withTotal 3300 review {reviewItems = [item "a" 1000, item "b" 2000], reviewAdjustments = [adjustment "fee" AdjustmentOther 150 TreatmentAdd ["a"], adjustment "tax" AdjustmentTax 150 TreatmentAdd ["a", "b"]]}) [("a", 1200), ("b", 2100)],
        testCase "discounts run sequentially against remaining bases" $ assertAmounts (withTotal 1500 review {reviewItems = [item "a" 1000, item "b" 2000], reviewAdjustments = [adjustment "first" AdjustmentDiscount 1000 TreatmentSubtract ["b"], adjustment "second" AdjustmentDiscount 500 TreatmentSubtract ["a", "b"]]}) [("a", 750), ("b", 750)],
        testCase "zero-cost rows remain in review but create no expense" $ assertAmounts (withTotal 100 review {reviewItems = [item "free" 0, item "paid" 100], reviewAdjustments = []}) [("paid", 100)],
        testCase "zero eligible bases cannot absorb a charge" $ assertInvalid (withTotal 1 review {reviewItems = [item "a" 0], reviewAdjustments = [adjustment "tax" AdjustmentTax 1 TreatmentAdd ["a"]]}),
        testCase "excessive and empty-eligibility discounts fail" $ do
          assertInvalid (review {reviewAdjustments = [adjustment "discount" AdjustmentDiscount 3001 TreatmentSubtract ["a", "b"]]})
          assertInvalid (review {reviewAdjustments = [adjustment "discount" AdjustmentDiscount 1 TreatmentSubtract []]}),
        testCase "eligibility cannot contain duplicates or nonexistent items" $ do
          assertInvalid (review {reviewAdjustments = [adjustment "tax" AdjustmentTax 0 TreatmentAdd ["a", "a"]]})
          assertInvalid (review {reviewAdjustments = [adjustment "tax" AdjustmentTax 0 TreatmentInformational ["missing"]]}),
        testCase "included and informational charges add zero" $ assertAmounts (withTotal 3000 review {reviewAdjustments = [adjustment "tax" AdjustmentTax 300 TreatmentIncluded [], adjustment "summary" AdjustmentTax 300 TreatmentInformational []]}) [("a", 1000), ("b", 2000)],
        testCase "reconciliation cannot be acknowledged away" $ assertInvalid (review {reviewTotalMinor = MinorAmount 2971, reviewPrintedTotalMinor = Just $ MinorAmount 2971, reviewAcknowledgedWarnings = ["total_mismatch:total"]}),
        testCase "discount add and tip subtract are forbidden" $ do
          assertInvalid (withTotal 3300 review {reviewAdjustments = [adjustment "discount" AdjustmentDiscount 300 TreatmentAdd ["a", "b"]]})
          assertInvalid (withTotal 2700 review {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 300, reviewAdjustments = [adjustment "tip" AdjustmentActualTip 300 TreatmentSubtract ["a", "b"]]}),
        testCase "final expense bounds hold after allocation" $ assertInvalid (withTotal (maxRecordMinor + 1) review {reviewItems = [item "a" (maxRecordMinor + 1)], reviewAdjustments = []}),
        testCase "itemized aggregate may exceed one record's storage bound" $ assertAmounts (withTotal (2 * maxRecordMinor) review {reviewItems = [item "a" maxRecordMinor, item "b" maxRecordMinor], reviewAdjustments = []}) [("a", maxRecordMinor), ("b", maxRecordMinor)],
        testCase "JPY uses catalog precision and exact item minor units" $ assertAmounts (withTotal 2970 review {reviewCurrency = CurrencyCode "JPY"}) [("a", 990), ("b", 1980)],
        testCase "fuel unit prices keep three decimals with a two-decimal currency" $ do
          let fuel = (item "fuel" 700) {reviewItemQuantity = Just "2", reviewItemUnitPrice = Just "3.499", reviewItemUnit = Just "L"}
          assertAmounts (withTotal 700 review {reviewItems = [fuel], reviewAdjustments = []}) [("fuel", 700)],
        testCase "quantity is not multiplied into the extended amount twice" $ assertAmounts (withTotal 1000 review {reviewItems = [(item "a" 1000) {reviewItemQuantity = Just "2", reviewItemUnitPrice = Just "5.00"}], reviewAdjustments = []}) [("a", 1000)],
        testCase "corrected quantity/price mismatch needs a stable warning acknowledgement" $ do
          let value = withTotal 1000 review {reviewItems = [(item "a" 1000) {reviewItemQuantity = Just "2", reviewItemUnitPrice = Just "4.00"}], reviewAdjustments = []}
          assertInvalid value
          assertAmounts (value {reviewAcknowledgedWarnings = ["quantity_price_mismatch:items.a"]}) [("a", 1000)],
        testCase "unusable optional numbers can be explicitly null" $ do
          let value = withTotal 1000 review {reviewItems = [(item "a" 1000) {reviewItemQuantity = Just "unreadable"}], reviewAdjustments = []}
          assertInvalid value
          assertAmounts (value {reviewItems = [item "a" 1000]}) [("a", 1000)]
      ],
    testGroup "Confirmation and tips"
      [ testCase "bill-only preserves unknown tip/payment with explicit confirmation" $ do
          let value = totalReview {reviewTipDecision = TipBillOnly, reviewActualTipMinor = Nothing}
          assertInvalid value
          assertAmounts (value {reviewAcknowledgedWarnings = ["bill_only:tipDecision"]}) [("Receipt expense", 2970)],
        testCase "bill-only forbids final-paid basis and external tip addition" $ do
          let value = review {reviewTipDecision = TipBillOnly, reviewActualTipMinor = Nothing, reviewAcknowledgedWarnings = ["bill_only:tipDecision"]}
          assertInvalid (value {reviewTotalBasis = TotalFinalPaid, reviewFinalPaidMinor = Just $ MinorAmount 2970})
          assertInvalid (value {reviewAdjustments = [adjustment "tip" AdjustmentActualTip 1 TreatmentAdd ["a"]]}),
        testCase "no tip requires known zero and forbids nonzero included tips" $ do
          assertInvalid (totalReview {reviewActualTipMinor = Nothing})
          assertInvalid (withTotal 3000 review {reviewAdjustments = [adjustment "tip" AdjustmentActualTip 1 TreatmentIncluded []]}),
        testCase "known actual tip is represented once and already included in total mode" $ do
          assertAmounts (totalReview {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 500}) [("Receipt expense", 2970)]
          assertAmounts (withTotal 3300 review {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 300, reviewAdjustments = [adjustment "tip" AdjustmentActualTip 300 TreatmentAdd ["a", "b"]]}) [("a", 1100), ("b", 2200)],
        testCase "included and informational actual tip may describe existing item bases" $ do
          mapM_ (\treatment -> assertAmounts (withTotal 3000 review {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 300, reviewAdjustments = [adjustment "tip" AdjustmentActualTip 300 treatment []]}) [("a", 1000), ("b", 2000)]) [TreatmentIncluded, TreatmentInformational],
        testCase "actual tip cannot be missing, exceed total, or mismatch components" $ do
          assertInvalid (totalReview {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Nothing})
          assertInvalid (totalReview {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 3000})
          assertInvalid (review {reviewTipDecision = TipConfirmed, reviewActualTipMinor = Just $ MinorAmount 500}),
        testCase "total basis requires equality only for the selected reference" $ do
          assertInvalid (totalReview {reviewPrintedTotalMinor = Nothing})
          assertInvalid (totalReview {reviewTotalBasis = TotalFinalPaid})
          assertAmounts (totalReview {reviewTotalBasis = TotalUserEntered, reviewPrintedTotalMinor = Nothing}) [("Receipt expense", 2970)]
          assertAmounts (totalReview {reviewTotalBasis = TotalFinalPaid, reviewFinalPaidMinor = Just $ MinorAmount 2970}) [("Receipt expense", 2970)],
        testCase "shared currency/date/confirmation requirements cannot be bypassed" $ do
          mapM_ assertInvalid [totalReview {reviewCurrency = CurrencyCode "ZZZ"}, totalReview {reviewDate = "10/21/26"}, totalReview {reviewDate = "2026-02-30"}, totalReview {reviewConfirmed = False}, totalReview {reviewTotalTitle = "  "}, withTotal 0 totalReview],
        testCase "full dates never assume a century or today's date" $ do
          assertBool "valid leap day" (validReceiptDate "2024-02-29")
          mapM_ (\date -> assertBool "ambiguous/noncanonical date" (not $ validReceiptDate date)) ["24-02-29", "2026-1-02", "", "2026-10-21T00:00:00Z"],
        testCase "total mode refuses leftover itemized arrays" $ assertInvalid (review {reviewMode = ReviewTotal})
      ],
    testGroup "Evidence diagnostics and canonical bytes"
      [ testCase "malformed readable amounts remain evidence and get field diagnostics" $ do
          let original = evidence {evidenceItems = [syntheticItem {evidenceItemLineTotal = visible "1O.00"}]}
          assertRoundTrip original
          assertBool "field-addressed numeric error" (any (\value -> reviewIssueField value == "items.a.line_total" && reviewIssueSeverity value == ReviewError) $ validateReceiptEvidence catalog original)
          assertBool "corrected total mode ignores unusable original itemization" (isRight $ validateReceiptImportReview catalog (Just original) totalReview),
        testCase "missing optional subtotal and price are not hard blockers" $ assertBool "no amount errors" (not $ any ((== ReviewError) . reviewIssueSeverity) $ validateReceiptEvidence catalog evidence),
        testCase "contextual currency and absent date are never globally assumed" $ do
          let original = evidence {evidenceCurrency = (evidenceCurrency evidence) {evidenceCurrencyBasis = CurrencyContextual}, evidenceTransactionDate = absent}
          assertBool "currency unconfirmed" (any ((== "currency_unconfirmed") . reviewIssueCode) $ validateReceiptEvidence catalog original)
          assertBool "date required" (any ((== "date_required") . reviewIssueCode) $ validateReceiptEvidence catalog original),
        testCase "malformed suggested-tip percentage needs acknowledgement, never adds money" $ do
          let original = evidence {evidenceSuggestedTips = [ReceiptSuggestedTip (visible "20%") (visible "5.94")]}
          assertBool "warning blocks without acknowledgement" (isLeft $ validateReceiptImportReview catalog (Just original) totalReview)
          assertBool "warning acknowledged without actual tip" (validateReceiptImportReview catalog (Just original) (totalReview {reviewAcknowledgedWarnings = ["invalid_decimal:suggested_tips.0.rate"]}) == Right [("Receipt expense", MinorAmount 2970)]),
        testCase "model issues use stable index tokens and item warnings do not block total mode" $ do
          let original = evidence {evidenceIssues = [ReceiptModelIssue "merchant" "Confirm literal <script> text", ReceiptModelIssue "items.a" "Unclear item"]}
          assertBool "unacknowledged merchant issue" (isLeft $ validateReceiptImportReview catalog (Just original) totalReview)
          assertBool "total mode ignores item-only issue" (isRight $ validateReceiptImportReview catalog (Just original) (totalReview {reviewAcknowledgedWarnings = ["model_issue_0"]})),
        testCase "canonicalization recursively sorts keys and keeps exact decoded Unicode" $ do
          let bytes = canonicalReceiptEvidence evidence
          assertBool "canonical begins with lexically first key" (BS.take 15 bytes == "{\"adjustments\":")
          assertBool "Unicode stays literal" (encodeUtf8 "商店" `BS.isInfixOf` bytes)
          let reordered = reorder (toJSON evidence)
          case eitherDecode (encode reordered) of
            Left _ -> assertFailure "Reordered evidence must decode"
            Right parsed -> canonicalReceiptEvidence parsed @?= bytes
          let escaped = LBS.fromStrict $ encodeUtf8 $ T.replace "Synthetic 商店" "Synthetic \\u5546\\u5e97" (decodeUtf8 $ LBS.toStrict $ encode evidence)
          case eitherDecode escaped of
            Left _ -> assertFailure "Escaped evidence must decode"
            Right parsed -> canonicalReceiptEvidence parsed @?= bytes,
        testCase "canonical digest input changes when original evidence changes" $ assertBool "different evidence bytes" (canonicalReceiptEvidence evidence /= canonicalReceiptEvidence (evidence {evidenceMerchant = visible "Corrected original is not allowed"})),
        testCase "exact decimals have bounded precision independent of currency" $ do
          assertBool "18/18 decimal accepted" (isRight $ parseReceiptDecimal "123456789012345678.123456789012345678")
          mapM_ (\value -> assertBool "invalid decimal rejected" (isLeft $ parseReceiptDecimal value)) ["1e3", "-1", "+1", "1.", ".5", " 1", "1234567890123456789", "0.1234567890123456789"]
      ],
    testGroup "Bounded strict JSON"
      [ testCase "extraction wrapper requires both keys and normalizes only its title" $ do
          let output = ReceiptExtractionOutput (Just "Synthetic Market groceries") evidence
              parse value = eitherDecode (encode value) :: Either String ReceiptExtractionOutput
          assertRoundTrip output
          assertRoundTrip (ReceiptExtractionOutput Nothing evidence)
          assertBool "old direct evidence rejected" (isLeft $ parse $ toJSON evidence)
          mapM_ (\key -> assertBool "required wrapper key" $ isLeft $ parse $ dropField key $ toJSON output) ["title", "evidence"]
          assertBool "unknown wrapper key" (isLeft $ parse $ changeField "extra" Null $ toJSON output)
          let padded = changeField "title" (String "  会议 <b>Market</b>  ") $ toJSON output
          assertBool "title trim preserves original evidence" (parse padded == Right (ReceiptExtractionOutput (Just "会议 <b>Market</b>") evidence)),
        testCase "extraction response requires nullable title" $ do
          let response = ReceiptExtractionResponse nil "gpt-5.4-mini-2026-03-17" "receipt-v2" Nothing evidence []
              parse value = eitherDecode (encode value) :: Either String ReceiptExtractionResponse
          assertRoundTrip response
          assertBool "missing response title rejected" (isLeft $ parse $ dropField "title" $ toJSON response)
          assertBool "non-null response title round trips" (parse (changeField "title" (String "Synthetic Market groceries") $ toJSON response) == Right response {extractionTitle = Just "Synthetic Market groceries"})
          mapM_ (\title -> assertBool "invalid response title rejected" $ isLeft $ parse $ changeField "title" (String title) $ toJSON response)
            ["", "　 ", "Market\nshop", "Market\x85shop", "Market\x2029shop", T.replicate 513 "😀"],
        testCase "unknown evidence keys are rejected recursively" $ do
          assertBool "outer unknown" (isLeft (eitherDecode (encode $ changeField "approval" (Bool True) (toJSON evidence)) :: Either String ReceiptEvidence))
          assertBool "nested unknown" (isLeft (eitherDecode (encode $ changeField "merchant" (changeField "groupId" (String "injected") $ toJSON $ visible "Merchant") (toJSON evidence)) :: Either String ReceiptEvidence)),
        testCase "required nullable keys cannot be omitted" $ do
          assertBool "source evidence key required" (isLeft (eitherDecode (encode $ dropField "evidence" $ toJSON manualSource) :: Either String ReceiptSourceInput))
          assertBool "observation raw key required" (isLeft (eitherDecode (encode $ dropField "raw" $ toJSON absent) :: Either String ReceiptObservation)),
        testCase "status/value mismatches and unsupported enum values fail structurally" $ do
          assertBool "visible null" (isLeft (eitherDecode "{\"value\":null,\"raw\":null,\"status\":\"visible\"}" :: Either String ReceiptObservation))
          assertBool "absent value" (isLeft (eitherDecode "{\"value\":\"1\",\"raw\":null,\"status\":\"absent\"}" :: Either String ReceiptObservation))
          assertBool "legacy invalid treatment" (isLeft (eitherDecode "\"included_in_total\"" :: Either String ReceiptAdjustmentTreatment)),
        testCase "descriptions, rows, issues, NUL and serialized bytes are bounded" $ do
          let decodeEvidence value = isLeft (eitherDecode (encode value) :: Either String ReceiptEvidence)
          assertBool "description bound" (decodeEvidence evidence {evidenceItems = [syntheticItem {evidenceItemDescription = T.replicate 513 "x"}]})
          assertBool "item count bound" (decodeEvidence evidence {evidenceItems = replicate 201 syntheticItem})
          assertBool "issue count bound" (decodeEvidence evidence {evidenceIssues = replicate 401 (ReceiptModelIssue "field" "reason")})
          assertBool "NUL rejected" (decodeEvidence evidence {evidenceMerchant = visible "bad\0text"})
          assertBool "serialized evidence bound" (decodeEvidence evidence {evidenceIssues = replicate 200 (ReceiptModelIssue "field" (T.replicate 2048 "x"))}),
        testCase "review IDs are ASCII and unique through validation" $ do
          mapM_ (\key -> assertInvalid (review {reviewItems = [item key 2970], reviewAdjustments = []})) ["UPPER", "é", "", T.replicate 65 "x", "member-id"]
          assertInvalid (review {reviewItems = [item "a" 1000, item "a" 1970], reviewAdjustments = []}),
        testCase "review rejects negative, noncanonical, unknown treatment and oversized values" $ do
          assertInvalid (review {reviewItems = [item "a" (-1)], reviewAdjustments = []})
          assertInvalid (review {reviewAdjustments = [adjustment "tax" AdjustmentTax 1 TreatmentUnknown []]})
          assertBool "leading-zero money" (isLeft (eitherDecode (encode $ changeField "totalMinor" (String "02970") $ toJSON review) :: Either String ReceiptImportReview))
          assertBool "review serialized bound" (isLeft (eitherDecode (encode review {reviewAcknowledgedWarnings = replicate 1000 (T.replicate 640 "x")}) :: Either String ReceiptImportReview)),
        testCase "source accepts only complete model provenance or all-null manual provenance" $ do
          assertBool "partial provenance" (isLeft (eitherDecode (encode manualSource {receiptSourceExtractionId = Just nil}) :: Either String ReceiptSourceInput))
          assertBool "image URL rejected" (isLeft (eitherDecode "{\"mediaType\":\"image/jpeg\",\"url\":\"https://example.invalid/image\"}" :: Either String ReceiptImageInput)),
        testCase "source metadata enforces byte, dimension, pixel and digest bounds" $ do
          let meta = ReceiptSourceMeta "image/jpeg" 100 100 4 (T.replicate 64 "a") False
              rejected value = assertBool "invalid source metadata" (isLeft (eitherDecode (encode value) :: Either String ReceiptSourceMeta))
          assertRoundTrip meta
          mapM_ rejected [meta {receiptSourceWidth = 8193}, meta {receiptSourceHeight = 0}, meta {receiptSourceWidth = 8192, receiptSourceHeight = 8192}, meta {receiptSourceByteCount = maxReceiptImageBytes + 1}, meta {receiptSourceSha256 = T.replicate 64 "A"}]
      ],
    testGroup "Receipt titles"
      [ testCase "normalization preserves literal Unicode and internal whitespace" $ do
          normalizeReceiptTitle " 会议 <b>Market</b>  " @?= Right "会议 <b>Market</b>"
          normalizeReceiptTitle "a  b" @?= Right "a  b"
          normalizeReceiptTitle "\xfeff" @?= Right "\xfeff"
          normalizeReceiptTitle "\x2003Market\x3000" @?= Right "Market",
        testCase "control rejection precedes surrounding whitespace trimming" $ do
          mapM_ (\character -> assertBool "control rejected before trimming" $ isLeft $ normalizeReceiptTitle $ T.singleton character <> "Market")
            (map toEnum ([0..31] <> [127..159] <> [0x2028,0x2029]))
          assertBool "blank title rejected" (isLeft $ normalizeReceiptTitle "   "),
        testCase "the cap counts Unicode code points rather than UTF-16 units" $ do
          normalizeReceiptTitle (T.replicate 512 "😀") @?= Right (T.replicate 512 "😀")
          assertBool "overlong title rejected" (isLeft $ normalizeReceiptTitle $ T.replicate 513 "😀"),
        testCase "all request codecs require and normalize titles" $ do
          let base = object ["uploadedByUserId" .= nil, "note" .= ("" :: Text), "records" .= ([] :: [Value])]
              member = dropField "uploadedByUserId" base
              rejected value = do
                assertBool "member create rejects invalid title" (isLeft (eitherDecode (encode $ dropField "uploadedByUserId" value) :: Either String Receipt.CreateReceiptRequest))
                assertBool "member/admin replacement rejects invalid title" (isLeft (eitherDecode (encode $ dropField "uploadedByUserId" value) :: Either String Receipt.UpdateReceiptRequest))
                assertBool "admin create rejects invalid title" (isLeft (eitherDecode (encode value) :: Either String AdminCreateReceiptRequest))
          mapM_ rejected [base, changeField "title" Null base, changeField "title" (String "  ") base, changeField "title" (String "bad\n") base, changeField "title" (String $ T.replicate 513 "😀") base]
          assertBool "create normalizes title" (eitherDecode (encode $ changeField "title" (String " Market ") member) == Right (Receipt.CreateReceiptRequest "Market" "" [] Nothing))
          assertBool "replacement normalizes title" (eitherDecode (encode $ changeField "title" (String " Market ") member) == Right (Receipt.UpdateReceiptRequest "Market" "" [] KeepReceiptSource)),
        testCase "receipt summary uses only the public wire names" $ do
          let summary = ReceiptSummary (ReceiptId nil) "会议 <b>Market</b>"
          toJSON summary @?= object ["receiptId" .= nil, "title" .= ("会议 <b>Market</b>" :: Text)]
          assertRoundTrip summary
      ],
    testGroup "Receipt source presence semantics"
      [ testCase "create source may be omitted or null" $ do
          let omitted = eitherDecode "{\"title\":\"Synthetic Market groceries\",\"note\":\"\",\"records\":[]}" :: Either String Receipt.CreateReceiptRequest
              nullValue = eitherDecode "{\"title\":\"Synthetic Market groceries\",\"note\":\"\",\"records\":[],\"source\":null}" :: Either String Receipt.CreateReceiptRequest
              expected = Receipt.CreateReceiptRequest "Synthetic Market groceries" "" [] Nothing
          assertBool "omitted source" (omitted == Right expected)
          assertBool "null source" (nullValue == Right expected),
        testCase "update omission, null and object have distinct semantics" $ do
          let decode value = eitherDecode value :: Either String Receipt.UpdateReceiptRequest
          assertBool "omission keeps" (decode "{\"title\":\"Synthetic Market groceries\",\"note\":\"\",\"records\":[]}" == Right (Receipt.UpdateReceiptRequest "Synthetic Market groceries" "" [] KeepReceiptSource))
          assertBool "null removes" (decode "{\"title\":\"Synthetic Market groceries\",\"note\":\"\",\"records\":[],\"source\":null}" == Right (Receipt.UpdateReceiptRequest "Synthetic Market groceries" "" [] RemoveReceiptSource))
          assertRoundTrip (Receipt.UpdateReceiptRequest "Synthetic Market groceries" "" [] (ReplaceReceiptSource manualSource))
          assertRoundTrip (Receipt.UpdateReceiptRequest "Synthetic Market groceries" "" [] (ReplaceReceiptSource modelSource)),
        testCase "update unknown fields remain rejected and keep encoding omits source" $ do
          assertBool "unknown update key" (isLeft (eitherDecode "{\"title\":\"Synthetic Market groceries\",\"note\":\"\",\"records\":[],\"approval\":true}" :: Either String Receipt.UpdateReceiptRequest))
          case toJSON (Receipt.UpdateReceiptRequest "Synthetic Market groceries" "" [] KeepReceiptSource) of
            Object fields -> assertBool "source omitted" (not $ KM.member "source" fields)
            _ -> assertFailure "Receipt update must encode as an object",
        testCase "receipt responses require nullable source" $ do
          let response = object ["receiptId" .= nil, "groupId" .= nil, "uploadedBy" .= object ["userId" .= nil, "username" .= ("synthetic" :: Text), "displayName" .= ("Synthetic" :: Text)], "title" .= ("Synthetic Market groceries" :: Text), "note" .= ("" :: Text), "records" .= ([] :: [Value]), "createdAt" .= ("2026-10-21T00:00:00Z" :: Text)]
          assertBool "missing source rejected" (isLeft (eitherDecode (encode response) :: Either String Receipt.Receipt))
          assertBool "explicit null accepted" (isRight (eitherDecode (encode $ changeField "source" Null response) :: Either String Receipt.Receipt))
          let complete = changeField "source" Null response
          assertBool "missing title rejected" (isLeft (eitherDecode (encode $ dropField "title" complete) :: Either String Receipt.Receipt))
          assertBool "null title rejected" (isLeft (eitherDecode (encode $ changeField "title" Null complete) :: Either String Receipt.Receipt))
      ],
    testCase "packaged generic contract loads with pinned model and schema" $ do
      contract <- loadReceiptAIContract
      contractPromptVersion contract @?= "receipt-v2"
      assertBool "provider model cannot be changed" (isLeft $ validateReceiptAIContract contract {contractModel = "other-model"})
      assertBool "old provider prompt cannot be selected" (isLeft $ validateReceiptAIContract contract {contractPromptVersion = "receipt-v1"})
      let invalid schema = isLeft $ validateReceiptAIContract contract {contractSchema = schema}
      assertBool "old direct evidence schema rejected" (invalid $ nestedField "evidence" $ nestedField "properties" $ contractSchema contract)
      assertBool "missing wrapper property rejected" (invalid $ changeField "properties" (dropField "title" $ nestedField "properties" $ contractSchema contract) $ contractSchema contract)
      assertBool "extra wrapper required key rejected" (invalid $ changeField "required" (toJSON (["title", "evidence", "extra"] :: [Text])) $ contractSchema contract)
      let properties = nestedField "properties" $ contractSchema contract
          nestedEvidence = nestedField "evidence" properties
          evidenceProperties = nestedField "properties" nestedEvidence
      assertBool "nested evidence keys checked" (invalid $ changeField "properties"
        (changeField "evidence" (changeField "properties" (dropField "merchant" evidenceProperties) nestedEvidence) properties) $ contractSchema contract)
      assertBool "nested evidence remains strict" (invalid $ changeField "properties"
        (changeField "evidence" (changeField "additionalProperties" (Bool True) nestedEvidence) properties) $ contractSchema contract)
  ]
  where
    nestedField key (Object fields) = maybe Null id $ KM.lookup key fields
    nestedField _ _ = Null
    reorder (Object fields) = Object $ KM.fromList $ reverse [(key, reorder value) | (key, value) <- KM.toList fields]
    reorder (Array values) = Array (fmap reorder values)
    reorder value = value
