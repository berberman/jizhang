{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Jizhang.API.Types.ReceiptAI
  ( ReceiptObservationStatus (..),
    ReceiptDocumentKind (..),
    ReceiptCurrencyBasis (..),
    ReceiptPriceBasis (..),
    ReceiptAdjustmentKind (..),
    ReceiptAdjustmentScope (..),
    ReceiptAdjustmentTreatment (..),
    ReceiptReviewMode (..),
    ReceiptTotalBasis (..),
    ReceiptTipDecision (..),
    ReceiptIssueSeverity (..),
    ReceiptObservation (..),
    ReceiptEvidenceCurrency (..),
    ReceiptEvidenceItem (..),
    ReceiptEvidenceAdjustment (..),
    ReceiptEvidenceTotals (..),
    ReceiptSuggestedTip (..),
    ReceiptModelIssue (..),
    ReceiptEvidence (..),
    ReceiptExtractionOutput (..),
    ReceiptReviewItem (..),
    ReceiptReviewAdjustment (..),
    ReceiptImportReview (..),
    ReceiptImageInput (..),
    ReceiptSourceInput (..),
    ReceiptSourceMeta (..),
    ReceiptReviewIssue (..),
    ReceiptExtractionOptions (..),
    ReceiptExtractionResponse (..),
    ReceiptSourceChange (..),
    maxReceiptImageBytes,
    maxReceiptImagePixels,
    maxReceiptImageDimension,
    maxReceiptEvidenceBytes,
    canonicalReceiptEvidence,
    parseReceiptDecimal,
    validReceiptItemId,
    validateReviewBounds
  )
where

import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema, defaultSchemaOptions)
import qualified Data.Swagger as Swagger
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Jizhang.API.Types.Money (CurrencyCode, MinorAmount (..), asciiDigits, decimalInteger, enumSchema)
import Jizhang.API.Types.ReceiptSummary (normalizeReceiptTitle)

maxReceiptImageBytes, maxReceiptImagePixels, maxReceiptImageDimension, maxReceiptEvidenceBytes :: Int
maxReceiptImageBytes = 5 * 1024 * 1024
maxReceiptImagePixels = 16000000
maxReceiptImageDimension = 8192
maxReceiptEvidenceBytes = 256 * 1024

jsonOptions :: [(String, String)] -> Options
jsonOptions fields = defaultOptions {fieldLabelModifier = \name -> maybe name id (lookup name fields), rejectUnknownFields = True}

schemaOptions :: [(String, String)] -> Swagger.SchemaOptions
schemaOptions fields = defaultSchemaOptions {Swagger.fieldLabelModifier = \name -> maybe name id (lookup name fields)}

receiptFieldKeys :: [(String, String)] -> [Text]
receiptFieldKeys = map (T.pack . snd)

requireReceiptSchemaFields :: [(String, String)] -> Swagger.NamedSchema -> Swagger.NamedSchema
requireReceiptSchemaFields fields (Swagger.NamedSchema schemaName schema) =
  Swagger.NamedSchema schemaName schema {Swagger._schemaRequired = receiptFieldKeys fields}

strictObject :: [(String, String)] -> (Value -> Parser a) -> Value -> Parser a
strictObject fields parser value = withObject "Receipt object" (\obj -> do
  unless (Set.fromList (map Key.toText (KM.keys obj)) == Set.fromList (receiptFieldKeys fields)) $ fail "Receipt object has missing or unknown fields"
  checkWireText value
  parser value) value

checkWireText :: Value -> Parser ()
checkWireText (String text) = when (T.any (== '\0') text) $ fail "Receipt text must not contain NUL"
checkWireText (Object obj) = mapM_ checkWireText (KM.elems obj)
checkWireText (Array values) = mapM_ checkWireText values
checkWireText _ = pure ()

parseEnum :: [(Text, a)] -> Value -> Parser a
parseEnum entries = withText "Receipt enum" $ \value -> maybe (fail "Unknown receipt enum value") pure (lookup value entries)

data ReceiptObservationStatus = ObservationVisible | ObservationAbsent | ObservationUnreadable | ObservationAmbiguous
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptObservationStatus where
  toJSON value = String $ case value of
    ObservationVisible -> "visible"
    ObservationAbsent -> "absent"
    ObservationUnreadable -> "unreadable"
    ObservationAmbiguous -> "ambiguous"

instance FromJSON ReceiptObservationStatus where
  parseJSON = parseEnum [("visible", ObservationVisible), ("absent", ObservationAbsent), ("unreadable", ObservationUnreadable), ("ambiguous", ObservationAmbiguous)]

instance ToSchema ReceiptObservationStatus where
  declareNamedSchema _ = pure $ enumSchema "ReceiptObservationStatus" ["visible", "absent", "unreadable", "ambiguous"]

data ReceiptDocumentKind = RetailReceipt | RestaurantReceipt | RestaurantBill | FuelReceipt | UnknownDocument
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptDocumentKind where
  toJSON value = String $ case value of
    RetailReceipt -> "retail_receipt"
    RestaurantReceipt -> "restaurant_receipt"
    RestaurantBill -> "restaurant_bill"
    FuelReceipt -> "fuel_receipt"
    UnknownDocument -> "unknown"

instance FromJSON ReceiptDocumentKind where
  parseJSON = parseEnum [("retail_receipt", RetailReceipt), ("restaurant_receipt", RestaurantReceipt), ("restaurant_bill", RestaurantBill), ("fuel_receipt", FuelReceipt), ("unknown", UnknownDocument)]

instance ToSchema ReceiptDocumentKind where
  declareNamedSchema _ = pure $ enumSchema "ReceiptDocumentKind" ["retail_receipt", "restaurant_receipt", "restaurant_bill", "fuel_receipt", "unknown"]

data ReceiptCurrencyBasis = CurrencyExplicit | CurrencyContextual | CurrencyUnknown
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptCurrencyBasis where
  toJSON value = String $ case value of
    CurrencyExplicit -> "explicit"
    CurrencyContextual -> "contextual"
    CurrencyUnknown -> "unknown"

instance FromJSON ReceiptCurrencyBasis where
  parseJSON = parseEnum [("explicit", CurrencyExplicit), ("contextual", CurrencyContextual), ("unknown", CurrencyUnknown)]

instance ToSchema ReceiptCurrencyBasis where
  declareNamedSchema _ = pure $ enumSchema "ReceiptCurrencyBasis" ["explicit", "contextual", "unknown"]

data ReceiptPriceBasis = PriceNet | PriceGross | PriceUnknown
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptPriceBasis where
  toJSON value = String $ case value of
    PriceNet -> "net"
    PriceGross -> "gross"
    PriceUnknown -> "unknown"

instance FromJSON ReceiptPriceBasis where
  parseJSON = parseEnum [("net", PriceNet), ("gross", PriceGross), ("unknown", PriceUnknown)]

instance ToSchema ReceiptPriceBasis where
  declareNamedSchema _ = pure $ enumSchema "ReceiptPriceBasis" ["net", "gross", "unknown"]

data ReceiptAdjustmentKind = AdjustmentDiscount | AdjustmentTax | AdjustmentServiceCharge | AdjustmentActualTip | AdjustmentRounding | AdjustmentOther
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptAdjustmentKind where
  toJSON value = String $ case value of
    AdjustmentDiscount -> "discount"
    AdjustmentTax -> "tax"
    AdjustmentServiceCharge -> "service_charge"
    AdjustmentActualTip -> "actual_tip"
    AdjustmentRounding -> "rounding"
    AdjustmentOther -> "other"

instance FromJSON ReceiptAdjustmentKind where
  parseJSON = parseEnum [("discount", AdjustmentDiscount), ("tax", AdjustmentTax), ("service_charge", AdjustmentServiceCharge), ("actual_tip", AdjustmentActualTip), ("rounding", AdjustmentRounding), ("other", AdjustmentOther)]

instance ToSchema ReceiptAdjustmentKind where
  declareNamedSchema _ = pure $ enumSchema "ReceiptAdjustmentKind" ["discount", "tax", "service_charge", "actual_tip", "rounding", "other"]

data ReceiptAdjustmentScope = ScopeItem | ScopeReceipt | ScopeUnknown
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptAdjustmentScope where
  toJSON value = String $ case value of
    ScopeItem -> "item"
    ScopeReceipt -> "receipt"
    ScopeUnknown -> "unknown"

instance FromJSON ReceiptAdjustmentScope where
  parseJSON = parseEnum [("item", ScopeItem), ("receipt", ScopeReceipt), ("unknown", ScopeUnknown)]

instance ToSchema ReceiptAdjustmentScope where
  declareNamedSchema _ = pure $ enumSchema "ReceiptAdjustmentScope" ["item", "receipt", "unknown"]

data ReceiptAdjustmentTreatment = TreatmentAdd | TreatmentSubtract | TreatmentIncluded | TreatmentInformational | TreatmentUnknown
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptAdjustmentTreatment where
  toJSON value = String $ case value of
    TreatmentAdd -> "add"
    TreatmentSubtract -> "subtract"
    TreatmentIncluded -> "included_in_line_total"
    TreatmentInformational -> "informational"
    TreatmentUnknown -> "unknown"

instance FromJSON ReceiptAdjustmentTreatment where
  parseJSON = parseEnum [("add", TreatmentAdd), ("subtract", TreatmentSubtract), ("included_in_line_total", TreatmentIncluded), ("informational", TreatmentInformational), ("unknown", TreatmentUnknown)]

instance ToSchema ReceiptAdjustmentTreatment where
  declareNamedSchema _ = pure $ enumSchema "ReceiptAdjustmentTreatment" ["add", "subtract", "included_in_line_total", "informational", "unknown"]

data ReceiptReviewMode = ReviewItemized | ReviewTotal
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptReviewMode where
  toJSON value = String $ case value of
    ReviewItemized -> "itemized"
    ReviewTotal -> "total"

instance FromJSON ReceiptReviewMode where
  parseJSON = parseEnum [("itemized", ReviewItemized), ("total", ReviewTotal)]

instance ToSchema ReceiptReviewMode where
  declareNamedSchema _ = pure $ enumSchema "ReceiptReviewMode" ["itemized", "total"]

data ReceiptTotalBasis = TotalPrinted | TotalFinalPaid | TotalUserEntered
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptTotalBasis where
  toJSON value = String $ case value of
    TotalPrinted -> "printed_total"
    TotalFinalPaid -> "final_paid"
    TotalUserEntered -> "user_entered"

instance FromJSON ReceiptTotalBasis where
  parseJSON = parseEnum [("printed_total", TotalPrinted), ("final_paid", TotalFinalPaid), ("user_entered", TotalUserEntered)]

instance ToSchema ReceiptTotalBasis where
  declareNamedSchema _ = pure $ enumSchema "ReceiptTotalBasis" ["printed_total", "final_paid", "user_entered"]

data ReceiptTipDecision = TipConfirmed | TipNone | TipBillOnly
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptTipDecision where
  toJSON value = String $ case value of
    TipConfirmed -> "confirmed"
    TipNone -> "none"
    TipBillOnly -> "bill_only"

instance FromJSON ReceiptTipDecision where
  parseJSON = parseEnum [("confirmed", TipConfirmed), ("none", TipNone), ("bill_only", TipBillOnly)]

instance ToSchema ReceiptTipDecision where
  declareNamedSchema _ = pure $ enumSchema "ReceiptTipDecision" ["confirmed", "none", "bill_only"]

data ReceiptIssueSeverity = ReviewWarning | ReviewError
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ReceiptIssueSeverity where
  toJSON value = String $ case value of
    ReviewWarning -> "warning"
    ReviewError -> "error"

instance FromJSON ReceiptIssueSeverity where
  parseJSON = parseEnum [("warning", ReviewWarning), ("error", ReviewError)]

instance ToSchema ReceiptIssueSeverity where
  declareNamedSchema _ = pure $ enumSchema "ReceiptIssueSeverity" ["warning", "error"]

data ReceiptObservation = ReceiptObservation
  { observationValue :: !(Maybe Text),
    observationRaw :: !(Maybe Text),
    observationStatus :: !(ReceiptObservationStatus)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptObservationFields :: [(String, String)]
receiptObservationFields =
  [ ("observationValue", "value"),
    ("observationRaw", "raw"),
    ("observationStatus", "status")
  ]

instance ToJSON ReceiptObservation where
  toJSON = genericToJSON (jsonOptions receiptObservationFields)

instance FromJSON ReceiptObservation where
  parseJSON = strictObject receiptObservationFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptObservationFields) value
    validateObservation result
    pure result

instance ToSchema ReceiptObservation where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptObservationFields <$> genericDeclareNamedSchema (schemaOptions receiptObservationFields) proxy

data ReceiptEvidenceCurrency = ReceiptEvidenceCurrency
  { evidenceCurrencyCode :: !(Maybe Text),
    evidenceCurrencySymbol :: !(Maybe Text),
    evidenceCurrencyBasis :: !(ReceiptCurrencyBasis),
    evidenceCurrencyEvidence :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptEvidenceCurrencyFields :: [(String, String)]
receiptEvidenceCurrencyFields =
  [ ("evidenceCurrencyCode", "code"),
    ("evidenceCurrencySymbol", "symbol"),
    ("evidenceCurrencyBasis", "basis"),
    ("evidenceCurrencyEvidence", "evidence")
  ]

instance ToJSON ReceiptEvidenceCurrency where
  toJSON = genericToJSON (jsonOptions receiptEvidenceCurrencyFields)

instance FromJSON ReceiptEvidenceCurrency where
  parseJSON = strictObject receiptEvidenceCurrencyFields $ genericParseJSON (jsonOptions receiptEvidenceCurrencyFields)

instance ToSchema ReceiptEvidenceCurrency where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptEvidenceCurrencyFields <$> genericDeclareNamedSchema (schemaOptions receiptEvidenceCurrencyFields) proxy

data ReceiptEvidenceItem = ReceiptEvidenceItem
  { evidenceItemId :: !(Text),
    evidenceItemDescription :: !(Text),
    evidenceItemRawLine :: !(Text),
    evidenceItemQuantity :: !(ReceiptObservation),
    evidenceItemUnit :: !(ReceiptObservation),
    evidenceItemUnitPrice :: !(ReceiptObservation),
    evidenceItemLineTotal :: !(ReceiptObservation),
    evidenceItemPriceBasis :: !(ReceiptPriceBasis),
    evidenceItemModifiers :: !([Text]),
    evidenceItemTaxMarkers :: !([Text])
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptEvidenceItemFields :: [(String, String)]
receiptEvidenceItemFields =
  [ ("evidenceItemId", "id"),
    ("evidenceItemDescription", "description"),
    ("evidenceItemRawLine", "raw_line"),
    ("evidenceItemQuantity", "quantity"),
    ("evidenceItemUnit", "unit"),
    ("evidenceItemUnitPrice", "unit_price"),
    ("evidenceItemLineTotal", "line_total"),
    ("evidenceItemPriceBasis", "price_basis"),
    ("evidenceItemModifiers", "modifiers"),
    ("evidenceItemTaxMarkers", "tax_markers")
  ]

instance ToJSON ReceiptEvidenceItem where
  toJSON = genericToJSON (jsonOptions receiptEvidenceItemFields)

instance FromJSON ReceiptEvidenceItem where
  parseJSON = strictObject receiptEvidenceItemFields $ genericParseJSON (jsonOptions receiptEvidenceItemFields)

instance ToSchema ReceiptEvidenceItem where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptEvidenceItemFields <$> genericDeclareNamedSchema (schemaOptions receiptEvidenceItemFields) proxy

data ReceiptEvidenceAdjustment = ReceiptEvidenceAdjustment
  { evidenceAdjustmentId :: !(Text),
    evidenceAdjustmentKind :: !(ReceiptAdjustmentKind),
    evidenceAdjustmentScope :: !(ReceiptAdjustmentScope),
    evidenceAdjustmentItemId :: !(Maybe Text),
    evidenceAdjustmentAmount :: !(ReceiptObservation),
    evidenceAdjustmentTreatment :: !(ReceiptAdjustmentTreatment),
    evidenceAdjustmentEvidence :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptEvidenceAdjustmentFields :: [(String, String)]
receiptEvidenceAdjustmentFields =
  [ ("evidenceAdjustmentId", "id"),
    ("evidenceAdjustmentKind", "kind"),
    ("evidenceAdjustmentScope", "scope"),
    ("evidenceAdjustmentItemId", "item_id"),
    ("evidenceAdjustmentAmount", "amount"),
    ("evidenceAdjustmentTreatment", "treatment"),
    ("evidenceAdjustmentEvidence", "evidence")
  ]

instance ToJSON ReceiptEvidenceAdjustment where
  toJSON = genericToJSON (jsonOptions receiptEvidenceAdjustmentFields)

instance FromJSON ReceiptEvidenceAdjustment where
  parseJSON = strictObject receiptEvidenceAdjustmentFields $ genericParseJSON (jsonOptions receiptEvidenceAdjustmentFields)

instance ToSchema ReceiptEvidenceAdjustment where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptEvidenceAdjustmentFields <$> genericDeclareNamedSchema (schemaOptions receiptEvidenceAdjustmentFields) proxy

data ReceiptEvidenceTotals = ReceiptEvidenceTotals
  { evidenceSubtotal :: !(ReceiptObservation),
    evidenceTaxTotal :: !(ReceiptObservation),
    evidencePrintedTotal :: !(ReceiptObservation),
    evidenceFinalPaid :: !(ReceiptObservation)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptEvidenceTotalsFields :: [(String, String)]
receiptEvidenceTotalsFields =
  [ ("evidenceSubtotal", "subtotal"),
    ("evidenceTaxTotal", "tax_total"),
    ("evidencePrintedTotal", "printed_total"),
    ("evidenceFinalPaid", "final_paid")
  ]

instance ToJSON ReceiptEvidenceTotals where
  toJSON = genericToJSON (jsonOptions receiptEvidenceTotalsFields)

instance FromJSON ReceiptEvidenceTotals where
  parseJSON = strictObject receiptEvidenceTotalsFields $ genericParseJSON (jsonOptions receiptEvidenceTotalsFields)

instance ToSchema ReceiptEvidenceTotals where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptEvidenceTotalsFields <$> genericDeclareNamedSchema (schemaOptions receiptEvidenceTotalsFields) proxy

data ReceiptSuggestedTip = ReceiptSuggestedTip
  { suggestedTipRate :: !(ReceiptObservation),
    suggestedTipAmount :: !(ReceiptObservation)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptSuggestedTipFields :: [(String, String)]
receiptSuggestedTipFields =
  [ ("suggestedTipRate", "rate"),
    ("suggestedTipAmount", "amount")
  ]

instance ToJSON ReceiptSuggestedTip where
  toJSON = genericToJSON (jsonOptions receiptSuggestedTipFields)

instance FromJSON ReceiptSuggestedTip where
  parseJSON = strictObject receiptSuggestedTipFields $ genericParseJSON (jsonOptions receiptSuggestedTipFields)

instance ToSchema ReceiptSuggestedTip where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptSuggestedTipFields <$> genericDeclareNamedSchema (schemaOptions receiptSuggestedTipFields) proxy

data ReceiptModelIssue = ReceiptModelIssue
  { modelIssueField :: !(Text),
    modelIssueReason :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptModelIssueFields :: [(String, String)]
receiptModelIssueFields =
  [ ("modelIssueField", "field"),
    ("modelIssueReason", "reason")
  ]

instance ToJSON ReceiptModelIssue where
  toJSON = genericToJSON (jsonOptions receiptModelIssueFields)

instance FromJSON ReceiptModelIssue where
  parseJSON = strictObject receiptModelIssueFields $ genericParseJSON (jsonOptions receiptModelIssueFields)

instance ToSchema ReceiptModelIssue where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptModelIssueFields <$> genericDeclareNamedSchema (schemaOptions receiptModelIssueFields) proxy

data ReceiptEvidence = ReceiptEvidence
  { evidenceDocumentKind :: !(ReceiptDocumentKind),
    evidenceMerchant :: !(ReceiptObservation),
    evidenceTransactionDate :: !(ReceiptObservation),
    evidenceCurrency :: !(ReceiptEvidenceCurrency),
    evidenceItems :: !([ReceiptEvidenceItem]),
    evidenceAdjustments :: !([ReceiptEvidenceAdjustment]),
    evidenceTotals :: !(ReceiptEvidenceTotals),
    evidenceSuggestedTips :: !([ReceiptSuggestedTip]),
    evidenceIssues :: !([ReceiptModelIssue])
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptEvidenceFields :: [(String, String)]
receiptEvidenceFields =
  [ ("evidenceDocumentKind", "document_kind"),
    ("evidenceMerchant", "merchant"),
    ("evidenceTransactionDate", "transaction_date"),
    ("evidenceCurrency", "currency"),
    ("evidenceItems", "items"),
    ("evidenceAdjustments", "adjustments"),
    ("evidenceTotals", "totals"),
    ("evidenceSuggestedTips", "suggested_tips"),
    ("evidenceIssues", "issues")
  ]

instance ToJSON ReceiptEvidence where
  toJSON = genericToJSON (jsonOptions receiptEvidenceFields)

instance FromJSON ReceiptEvidence where
  parseJSON = strictObject receiptEvidenceFields $ \value -> do
    boundedJson value
    result <- genericParseJSON (jsonOptions receiptEvidenceFields) value
    validateEvidenceBounds result
    pure result

instance ToSchema ReceiptEvidence where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptEvidenceFields <$> genericDeclareNamedSchema (schemaOptions receiptEvidenceFields) proxy

data ReceiptReviewItem = ReceiptReviewItem
  { reviewItemId :: !(Text),
    reviewItemTitle :: !(Text),
    reviewItemAmountMinor :: !(MinorAmount),
    reviewItemQuantity :: !(Maybe Text),
    reviewItemUnit :: !(Maybe Text),
    reviewItemUnitPrice :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptReviewItemFields :: [(String, String)]
receiptReviewItemFields =
  [ ("reviewItemId", "id"),
    ("reviewItemTitle", "title"),
    ("reviewItemAmountMinor", "amountMinor"),
    ("reviewItemQuantity", "quantity"),
    ("reviewItemUnit", "unit"),
    ("reviewItemUnitPrice", "unitPrice")
  ]

instance ToJSON ReceiptReviewItem where
  toJSON = genericToJSON (jsonOptions receiptReviewItemFields)

instance FromJSON ReceiptReviewItem where
  parseJSON = strictObject receiptReviewItemFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptReviewItemFields) value
    validateReviewItemWire result
    pure result

instance ToSchema ReceiptReviewItem where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptReviewItemFields <$> genericDeclareNamedSchema (schemaOptions receiptReviewItemFields) proxy

data ReceiptReviewAdjustment = ReceiptReviewAdjustment
  { reviewAdjustmentId :: !(Text),
    reviewAdjustmentKind :: !(ReceiptAdjustmentKind),
    reviewAdjustmentAmountMinor :: !(MinorAmount),
    reviewAdjustmentTreatment :: !(ReceiptAdjustmentTreatment),
    reviewAdjustmentItemIds :: !([Text])
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptReviewAdjustmentFields :: [(String, String)]
receiptReviewAdjustmentFields =
  [ ("reviewAdjustmentId", "id"),
    ("reviewAdjustmentKind", "kind"),
    ("reviewAdjustmentAmountMinor", "amountMinor"),
    ("reviewAdjustmentTreatment", "treatment"),
    ("reviewAdjustmentItemIds", "itemIds")
  ]

instance ToJSON ReceiptReviewAdjustment where
  toJSON = genericToJSON (jsonOptions receiptReviewAdjustmentFields)

instance FromJSON ReceiptReviewAdjustment where
  parseJSON = strictObject receiptReviewAdjustmentFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptReviewAdjustmentFields) value
    validateReviewAdjustmentWire result
    pure result

instance ToSchema ReceiptReviewAdjustment where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptReviewAdjustmentFields <$> genericDeclareNamedSchema (schemaOptions receiptReviewAdjustmentFields) proxy

data ReceiptImportReview = ReceiptImportReview
  { reviewMode :: !(ReceiptReviewMode),
    reviewMerchant :: !(Text),
    reviewTotalTitle :: !(Text),
    reviewCurrency :: !(CurrencyCode),
    reviewDate :: !(Text),
    reviewTotalBasis :: !(ReceiptTotalBasis),
    reviewPrintedTotalMinor :: !(Maybe MinorAmount),
    reviewFinalPaidMinor :: !(Maybe MinorAmount),
    reviewTotalMinor :: !(MinorAmount),
    reviewTipDecision :: !(ReceiptTipDecision),
    reviewActualTipMinor :: !(Maybe MinorAmount),
    reviewItems :: !([ReceiptReviewItem]),
    reviewAdjustments :: !([ReceiptReviewAdjustment]),
    reviewAcknowledgedWarnings :: !([Text]),
    reviewConfirmed :: !(Bool)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptImportReviewFields :: [(String, String)]
receiptImportReviewFields =
  [ ("reviewMode", "mode"),
    ("reviewMerchant", "merchant"),
    ("reviewTotalTitle", "totalTitle"),
    ("reviewCurrency", "currency"),
    ("reviewDate", "date"),
    ("reviewTotalBasis", "totalBasis"),
    ("reviewPrintedTotalMinor", "printedTotalMinor"),
    ("reviewFinalPaidMinor", "finalPaidMinor"),
    ("reviewTotalMinor", "totalMinor"),
    ("reviewTipDecision", "tipDecision"),
    ("reviewActualTipMinor", "actualTipMinor"),
    ("reviewItems", "items"),
    ("reviewAdjustments", "adjustments"),
    ("reviewAcknowledgedWarnings", "acknowledgedWarnings"),
    ("reviewConfirmed", "confirmed")
  ]

instance ToJSON ReceiptImportReview where
  toJSON = genericToJSON (jsonOptions receiptImportReviewFields)

instance FromJSON ReceiptImportReview where
  parseJSON = strictObject receiptImportReviewFields $ \value -> do
    boundedJson value
    result <- genericParseJSON (jsonOptions receiptImportReviewFields) value
    validateReviewBounds result
    pure result

instance ToSchema ReceiptImportReview where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptImportReviewFields <$> genericDeclareNamedSchema (schemaOptions receiptImportReviewFields) proxy

data ReceiptImageInput = ReceiptImageInput
  { receiptImageMediaType :: !(Text),
    receiptImageDataBase64 :: !(Text)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptImageInputFields :: [(String, String)]
receiptImageInputFields =
  [ ("receiptImageMediaType", "mediaType"),
    ("receiptImageDataBase64", "dataBase64")
  ]

instance ToJSON ReceiptImageInput where
  toJSON = genericToJSON (jsonOptions receiptImageInputFields)

instance FromJSON ReceiptImageInput where
  parseJSON = strictObject receiptImageInputFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptImageInputFields) value
    boundedText 64 (receiptImageMediaType result)
    boundedText (64 * 1024 * 1024) (receiptImageDataBase64 result)
    pure result

instance ToSchema ReceiptImageInput where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptImageInputFields <$> genericDeclareNamedSchema (schemaOptions receiptImageInputFields) proxy

data ReceiptSourceInput = ReceiptSourceInput
  { receiptSourceImage :: !(ReceiptImageInput),
    receiptSourceExtractionId :: !(Maybe UUID),
    receiptSourceEvidence :: !(Maybe ReceiptEvidence),
    receiptSourceReview :: !(Maybe ReceiptImportReview)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptSourceInputFields :: [(String, String)]
receiptSourceInputFields =
  [ ("receiptSourceImage", "image"),
    ("receiptSourceExtractionId", "extractionId"),
    ("receiptSourceEvidence", "evidence"),
    ("receiptSourceReview", "review")
  ]

instance ToJSON ReceiptSourceInput where
  toJSON = genericToJSON (jsonOptions receiptSourceInputFields)

instance FromJSON ReceiptSourceInput where
  parseJSON = strictObject receiptSourceInputFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptSourceInputFields) value
    validateSourceWire result
    pure result

instance ToSchema ReceiptSourceInput where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptSourceInputFields <$> genericDeclareNamedSchema (schemaOptions receiptSourceInputFields) proxy

data ReceiptSourceMeta = ReceiptSourceMeta
  { receiptSourceMediaType :: !(Text),
    receiptSourceWidth :: !(Int),
    receiptSourceHeight :: !(Int),
    receiptSourceByteCount :: !(Int),
    receiptSourceSha256 :: !(Text),
    receiptSourceHasReview :: !(Bool)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)

receiptSourceMetaFields :: [(String, String)]
receiptSourceMetaFields =
  [ ("receiptSourceMediaType", "mediaType"),
    ("receiptSourceWidth", "width"),
    ("receiptSourceHeight", "height"),
    ("receiptSourceByteCount", "byteCount"),
    ("receiptSourceSha256", "sha256"),
    ("receiptSourceHasReview", "hasReview")
  ]

instance ToJSON ReceiptSourceMeta where
  toJSON = genericToJSON (jsonOptions receiptSourceMetaFields)

instance FromJSON ReceiptSourceMeta where
  parseJSON = strictObject receiptSourceMetaFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptSourceMetaFields) value
    validateSourceMeta result
    pure result

instance ToSchema ReceiptSourceMeta where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptSourceMetaFields <$> genericDeclareNamedSchema (schemaOptions receiptSourceMetaFields) proxy

data ReceiptReviewIssue = ReceiptReviewIssue
  { reviewIssueCode :: !(Text),
    reviewIssueField :: !(Text),
    reviewIssueMessage :: !(Text),
    reviewIssueSeverity :: !(ReceiptIssueSeverity)
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptReviewIssueFields :: [(String, String)]
receiptReviewIssueFields =
  [ ("reviewIssueCode", "code"),
    ("reviewIssueField", "field"),
    ("reviewIssueMessage", "message"),
    ("reviewIssueSeverity", "severity")
  ]

instance ToJSON ReceiptReviewIssue where
  toJSON = genericToJSON (jsonOptions receiptReviewIssueFields)

instance FromJSON ReceiptReviewIssue where
  parseJSON = strictObject receiptReviewIssueFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptReviewIssueFields) value
    boundedText 64 (reviewIssueCode result)
    boundedText 512 (reviewIssueField result)
    boundedText 2048 (reviewIssueMessage result)
    pure result

instance ToSchema ReceiptReviewIssue where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptReviewIssueFields <$> genericDeclareNamedSchema (schemaOptions receiptReviewIssueFields) proxy

data ReceiptExtractionOptions = ReceiptExtractionOptions
  { extractionAvailable :: !(Bool),
    extractionModel :: !(Text),
    extractionReasoningEffort :: !(Text),
    extractionMaxImageBytes :: !(Int),
    extractionAcceptedMediaTypes :: !([Text])
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptExtractionOptionsFields :: [(String, String)]
receiptExtractionOptionsFields =
  [ ("extractionAvailable", "available"),
    ("extractionModel", "model"),
    ("extractionReasoningEffort", "reasoningEffort"),
    ("extractionMaxImageBytes", "maxImageBytes"),
    ("extractionAcceptedMediaTypes", "acceptedMediaTypes")
  ]

instance ToJSON ReceiptExtractionOptions where
  toJSON = genericToJSON (jsonOptions receiptExtractionOptionsFields)

instance FromJSON ReceiptExtractionOptions where
  parseJSON = strictObject receiptExtractionOptionsFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptExtractionOptionsFields) value
    unless (extractionMaxImageBytes result >= 0 && extractionMaxImageBytes result <= maxReceiptImageBytes && extractionAcceptedMediaTypes result == ["image/jpeg"]) $
      fail "Invalid receipt extraction upload limits"
    boundedText 128 (extractionModel result)
    boundedText 32 (extractionReasoningEffort result)
    pure result

instance ToSchema ReceiptExtractionOptions where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptExtractionOptionsFields <$> genericDeclareNamedSchema (schemaOptions receiptExtractionOptionsFields) proxy

data ReceiptExtractionOutput = ReceiptExtractionOutput
  { extractedTitle :: !(Maybe Text),
    extractedEvidence :: !ReceiptEvidence
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptExtractionOutputFields :: [(String, String)]
receiptExtractionOutputFields =
  [ ("extractedTitle", "title"),
    ("extractedEvidence", "evidence")
  ]

instance ToJSON ReceiptExtractionOutput where
  toJSON = genericToJSON (jsonOptions receiptExtractionOutputFields)

instance FromJSON ReceiptExtractionOutput where
  parseJSON = strictObject receiptExtractionOutputFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptExtractionOutputFields) value
    title <- traverse (either (fail . T.unpack) pure . normalizeReceiptTitle) (extractedTitle result)
    pure result {extractedTitle = title}

instance ToSchema ReceiptExtractionOutput where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptExtractionOutputFields <$> genericDeclareNamedSchema (schemaOptions receiptExtractionOutputFields) proxy

data ReceiptExtractionResponse = ReceiptExtractionResponse
  { extractionId :: !(UUID),
    extractionResponseModel :: !(Text),
    extractionPromptVersion :: !(Text),
    extractionTitle :: !(Maybe Text),
    extractionEvidence :: !(ReceiptEvidence),
    extractionIssues :: !([ReceiptReviewIssue])
  }
  deriving stock (Eq, Ord, Generic, Typeable)

receiptExtractionResponseFields :: [(String, String)]
receiptExtractionResponseFields =
  [ ("extractionId", "extractionId"),
    ("extractionResponseModel", "model"),
    ("extractionPromptVersion", "promptVersion"),
    ("extractionTitle", "title"),
    ("extractionEvidence", "evidence"),
    ("extractionIssues", "issues")
  ]

instance ToJSON ReceiptExtractionResponse where
  toJSON = genericToJSON (jsonOptions receiptExtractionResponseFields)

instance FromJSON ReceiptExtractionResponse where
  parseJSON = strictObject receiptExtractionResponseFields $ \value -> do
    result <- genericParseJSON (jsonOptions receiptExtractionResponseFields) value
    boundedList 400 (extractionIssues result)
    boundedText 128 (extractionResponseModel result)
    boundedText 64 (extractionPromptVersion result)
    title <- traverse (either (fail . T.unpack) pure . normalizeReceiptTitle) (extractionTitle result)
    pure result {extractionTitle = title}

instance ToSchema ReceiptExtractionResponse where
  declareNamedSchema proxy = requireReceiptSchemaFields receiptExtractionResponseFields <$> genericDeclareNamedSchema (schemaOptions receiptExtractionResponseFields) proxy

data ReceiptSourceChange
  = KeepReceiptSource
  | RemoveReceiptSource
  | ReplaceReceiptSource !ReceiptSourceInput
  deriving stock (Eq, Ord, Generic, Typeable)

-- Omission belongs to the enclosing update object, not to a standalone JSON value.
instance ToSchema ReceiptSourceChange where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy ReceiptSourceInput)

validateObservation :: ReceiptObservation -> Parser ()
validateObservation observation = do
  let visible = observationStatus observation == ObservationVisible
  unless (isJust (observationValue observation) == visible && (not visible || isJust (observationRaw observation))) $
    fail "Observation status does not match value/raw"
  mapM_ (boundedText 2048) [text | Just text <- [observationValue observation, observationRaw observation]]

boundedText :: Int -> Text -> Parser ()
boundedText bound value = unless (T.length value <= bound && not (T.any (== '\0') value)) $ fail "Receipt text exceeds its bound or contains NUL"

validateSourceMeta :: ReceiptSourceMeta -> Parser ()
validateSourceMeta meta = do
  unless (receiptSourceMediaType meta == "image/jpeg") $ fail "Invalid receipt source media type"
  let width = receiptSourceWidth meta
      height = receiptSourceHeight meta
      byteCount = receiptSourceByteCount meta
      digest = receiptSourceSha256 meta
  unless (width > 0 && height > 0 && width <= maxReceiptImageDimension && height <= maxReceiptImageDimension && toInteger width * toInteger height <= toInteger maxReceiptImagePixels) $
    fail "Invalid receipt image dimensions"
  unless (byteCount > 0 && byteCount <= maxReceiptImageBytes) $ fail "Invalid receipt image byte count"
  unless (T.length digest == 64 && T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) digest) $ fail "Invalid receipt image SHA-256"

boundedList :: Int -> [a] -> Parser ()
boundedList bound values = when (length values > bound) $ fail "Too many receipt entries"

validReceiptItemId :: Text -> Bool
validReceiptItemId value = not (T.null value) && T.length value <= 64 && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_') value

validateId :: Text -> Parser ()
validateId value = unless (validReceiptItemId value) $ fail "Invalid receipt row ID"

boundedJson :: ToJSON a => a -> Parser ()
boundedJson value = when (LBS.length (encode value) > fromIntegral maxReceiptEvidenceBytes) $ fail "Receipt evidence/review exceeds 256 KiB"

validateEvidenceBounds :: ReceiptEvidence -> Parser ()
validateEvidenceBounds evidence = do
  boundedJson evidence
  boundedList 200 (evidenceItems evidence)
  boundedList 200 (evidenceAdjustments evidence)
  boundedList 200 (evidenceSuggestedTips evidence)
  boundedList 400 (evidenceIssues evidence)
  mapM_ (boundedText 512) (maybe [] pure $ observationValue $ evidenceMerchant evidence)
  mapM_ (boundedText 2048) [text | Just text <- [evidenceCurrencyCode currency, evidenceCurrencySymbol currency, evidenceCurrencyEvidence currency]]
  mapM_ itemBounds (evidenceItems evidence)
  mapM_ adjustmentBounds (evidenceAdjustments evidence)
  mapM_ (\issue -> boundedText 512 (modelIssueField issue) >> boundedText 2048 (modelIssueReason issue)) (evidenceIssues evidence)
  where
    currency = evidenceCurrency evidence
    itemBounds item = do
      boundedText 64 (evidenceItemId item)
      boundedText 512 (evidenceItemDescription item)
      boundedText 2048 (evidenceItemRawLine item)
      boundedList 200 (evidenceItemModifiers item)
      boundedList 200 (evidenceItemTaxMarkers item)
      mapM_ (boundedText 512) (evidenceItemModifiers item <> evidenceItemTaxMarkers item)
    adjustmentBounds adjustment = do
      boundedText 64 (evidenceAdjustmentId adjustment)
      mapM_ (boundedText 64) (evidenceAdjustmentItemId adjustment)
      boundedText 2048 (evidenceAdjustmentEvidence adjustment)

validateCanonicalMinor :: MinorAmount -> Parser ()
validateCanonicalMinor (MinorAmount value) = when (value < 0) $ fail "Receipt amount must be nonnegative"

validateReviewItemWire :: ReceiptReviewItem -> Parser ()
validateReviewItemWire item = do
  validateId (reviewItemId item)
  boundedText 512 (reviewItemTitle item)
  validateCanonicalMinor (reviewItemAmountMinor item)
  mapM_ (boundedText 512) [text | Just text <- [reviewItemQuantity item, reviewItemUnit item, reviewItemUnitPrice item]]

validateReviewAdjustmentWire :: ReceiptReviewAdjustment -> Parser ()
validateReviewAdjustmentWire adjustment = do
  validateId (reviewAdjustmentId adjustment)
  validateCanonicalMinor (reviewAdjustmentAmountMinor adjustment)
  when (reviewAdjustmentTreatment adjustment == TreatmentUnknown) $ fail "Review adjustment treatment must be confirmed"
  boundedList 200 (reviewAdjustmentItemIds adjustment)
  mapM_ validateId (reviewAdjustmentItemIds adjustment)

validateReviewBounds :: ReceiptImportReview -> Parser ()
validateReviewBounds review = do
  boundedJson review
  boundedText 512 (reviewMerchant review)
  boundedText 512 (reviewTotalTitle review)
  boundedText 10 (reviewDate review)
  boundedList 200 (reviewItems review)
  boundedList 200 (reviewAdjustments review)
  mapM_ validateReviewItemWire (reviewItems review)
  mapM_ validateReviewAdjustmentWire (reviewAdjustments review)
  boundedList 1200 (reviewAcknowledgedWarnings review)
  mapM_ (boundedText 640) (reviewAcknowledgedWarnings review)
  mapM_ validateCanonicalMinor (reviewTotalMinor review : [value | Just value <- [reviewPrintedTotalMinor review, reviewFinalPaidMinor review, reviewActualTipMinor review]])


validateSourceWire :: ReceiptSourceInput -> Parser ()
validateSourceWire source = unless (all not supplied || and supplied) $ fail "Model source requires extractionId, evidence and review together"
  where supplied = [isJust (receiptSourceExtractionId source), isJust (receiptSourceEvidence source), isJust (receiptSourceReview source)]

-- Values are typed before canonicalization: only object order and JSON escaping disappear.
canonicalReceiptEvidence :: ReceiptEvidence -> BS.ByteString
canonicalReceiptEvidence = LBS.toStrict . Builder.toLazyByteString . canonicalValue . toJSON
  where
    canonicalValue (Object fields) = Builder.char8 '{' <> joined [Builder.lazyByteString (encode (Key.toText key)) <> Builder.char8 ':' <> canonicalValue value | (key, value) <- sortOn (Key.toText . fst) (KM.toList fields)] <> Builder.char8 '}'
    canonicalValue (Array values) = Builder.char8 '[' <> joined (foldr (\value rest -> canonicalValue value : rest) [] values) <> Builder.char8 ']'
    canonicalValue value = Builder.lazyByteString (encode value)
    joined [] = mempty
    joined (first : rest) = first <> foldMap (Builder.char8 ',' <>) rest

-- Nonnegative ordinary decimals, retaining unit-price precision independently of currency.
parseReceiptDecimal :: Text -> Either Text Rational
parseReceiptDecimal text = do
  let (whole, suffix) = T.breakOn "." text
      fraction = if T.null suffix then "" else T.drop 1 suffix
  unless (asciiDigits whole && (T.null suffix || asciiDigits fraction) && T.length whole <= 18 && T.length fraction <= 18) $
    Left "Use an unsigned decimal with at most 18 integer and 18 fractional digits"
  pure $ fromInteger (decimalInteger (whole <> fraction)) / fromInteger (10 ^ T.length fraction)
