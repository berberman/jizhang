{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exact money and exchange-rate wire formats. Amounts and rates are JSON
-- strings to avoid floating-point loss; exported constructors can bypass parsers.
module Jizhang.API.Types.Money where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import Data.Swagger hiding (name, schemaName)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

-- | Three uppercase ASCII letters in JSON and URLs. Parsing checks spelling,
-- not membership in the configured currency catalog.
newtype CurrencyCode = CurrencyCode Text
  deriving stock (Show, Eq, Ord, Generic, Typeable)

-- | Unwrap the stored spelling without normalizing or validating it.
currencyCodeText :: CurrencyCode -> Text
currencyCodeText (CurrencyCode value) = value

-- | Accept exactly three ASCII letters A..Z; lowercase is not normalized.
parseCurrencyCode :: Text -> Either Text CurrencyCode
parseCurrencyCode value
  | T.length value == 3 && T.all (\c -> c >= 'A' && c <= 'Z') value = Right (CurrencyCode value)
  | otherwise = Left "Currency code must contain three uppercase ASCII letters"

instance ToJSON CurrencyCode where toJSON = String . currencyCodeText

instance FromJSON CurrencyCode where
  parseJSON = withText "CurrencyCode" (either (fail . T.unpack) pure . parseCurrencyCode)

instance FromHttpApiData CurrencyCode where parseUrlPiece = parseCurrencyCode

instance ToHttpApiData CurrencyCode where toUrlPiece = currencyCodeText

instance ToSchema CurrencyCode where
  declareNamedSchema _ = pure $ stringSchema "CurrencyCode" "^[A-Z]{3}$"

instance ToParamSchema CurrencyCode where
  toParamSchema _ = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaPattern = Just "^[A-Z]{3}$"}

-- | Currency metadata object with required code, name and minorUnits fields.
-- Decoding rejects unknown fields, blank names and precision outside 0..4.
data CurrencyInfo = CurrencyInfo
  { -- | Catalog code, subject to CurrencyCode's spelling check on decoding.
    code :: !CurrencyCode,
    -- | Display label; validation trims only to test for emptiness.
    name :: !Text,
    -- | Decimal places per major unit: 2 makes minor amount "123" mean 1.23.
    minorUnits :: !Int
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON CurrencyInfo where
  parseJSON value = do
    result <- genericParseJSON defaultOptions {rejectUnknownFields = True} value
    if minorUnits result >= 0 && minorUnits result <= 4 && not (T.null (T.strip (name result)))
      then pure result
      else fail "Invalid currency metadata"

-- | Currency policy encoded as "single" or "multi", never constructor names.
-- Choosing a policy does not itself validate any record's currency.
data CurrencyMode = SingleCurrency | MultiCurrency
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON CurrencyMode where
  toJSON SingleCurrency = String "single"
  toJSON MultiCurrency = String "multi"

instance FromJSON CurrencyMode where
  parseJSON = withText "CurrencyMode" $ \value -> case value of
    "single" -> pure SingleCurrency
    "multi" -> pure MultiCurrency
    _ -> fail "Invalid currency mode"

instance ToSchema CurrencyMode where
  declareNamedSchema _ = pure $ enumSchema "CurrencyMode" ["single", "multi"]

-- | Split interpretation encoded as "equal", "shares", "percentage" or "exact".
-- Values mean unit weights, share weights, basis points or original minor units.
data SplitMode = EqualSplit | SharesSplit | PercentageSplit | ExactSplit
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON SplitMode where
  toJSON mode = String $ case mode of
    EqualSplit -> "equal"
    SharesSplit -> "shares"
    PercentageSplit -> "percentage"
    ExactSplit -> "exact"

instance FromJSON SplitMode where
  parseJSON = withText "SplitMode" $ \value -> case value of
    "equal" -> pure EqualSplit
    "shares" -> pure SharesSplit
    "percentage" -> pure PercentageSplit
    "exact" -> pure ExactSplit
    _ -> fail "Invalid split mode"

instance ToSchema SplitMode where
  declareNamedSchema _ = pure $ enumSchema "SplitMode" ["equal", "shares", "percentage", "exact"]

-- | Signed, arbitrary-precision minor units encoded as canonical integer strings.
-- This codec accepts negatives and imposes no record-size bound by itself.
newtype MinorAmount = MinorAmount Integer
  deriving stock (Show, Eq, Ord, Generic, Typeable)

-- | Extract exact minor units without converting through a fractional type.
minorAmountInteger :: MinorAmount -> Integer
minorAmountInteger (MinorAmount value) = value

-- | Maximum absolute request magnitude: the positive signed 64-bit limit.
maxRecordMinor :: Integer
maxRecordMinor = 9223372036854775807

-- | Reject overlarge textual magnitude before building an Integer. This guard
-- requires the field to be a string; canonical syntax is checked separately.
checkRequestMinorAmount :: Key -> Object -> Parser ()
checkRequestMinorAmount field obj = do
  value <- obj .: field
  let digits = maybe value id (T.stripPrefix "-" value)
      maximumDigits = T.pack (show maxRecordMinor)
      digitCount = T.length digits
      maximumDigitCount = T.length maximumDigits
  -- Equal-length canonical magnitudes compare lexically; no attacker-sized Integer is needed here.
  if digitCount > maximumDigitCount || (digitCount == maximumDigitCount && digits > maximumDigits)
    then fail "Amount exceeds the supported range"
    else pure ()

-- | Accept "0" or signed nonzero ASCII integers, without plus, whitespace,
-- leading zeroes or negative zero. There is no precision or magnitude limit.
parseMinorAmount :: Text -> Either Text MinorAmount
parseMinorAmount value
  | value == "0" = Right (MinorAmount 0)
  | Just digits <- T.stripPrefix "-" value, canonicalPositive digits = Right (MinorAmount (negate (decimalInteger digits)))
  | canonicalPositive value = Right (MinorAmount (decimalInteger value))
  | otherwise = Left "Money must be a canonical decimal integer string"
  where
    canonicalPositive digits = not (T.null digits) && T.head digits /= '0' && asciiDigits digits

-- | True only for a nonempty sequence of ASCII digits, not Unicode numerals.
asciiDigits :: Text -> Bool
asciiDigits value = not (T.null value) && T.all (\c -> c >= '0' && c <= '9') value

-- | Fold decimal digits exactly. Callers must establish ASCII digit syntax first.
decimalInteger :: Text -> Integer
decimalInteger = T.foldl' (\n c -> n * 10 + toInteger (fromEnum c - fromEnum '0')) 0

instance ToJSON MinorAmount where toJSON (MinorAmount value) = String (T.pack (show value))

instance FromJSON MinorAmount where
  parseJSON = withText "MinorAmount" (either (fail . T.unpack) pure . parseMinorAmount)

instance ToSchema MinorAmount where
  declareNamedSchema _ = pure $ stringSchema "MinorAmount" "^(0|-?[1-9][0-9]*)$"

-- | Positive exact decimal rate encoded as a string, with at most 18 integer
-- places and 18 fractional places after removing redundant zeroes.
newtype DecimalRate = DecimalRate Scientific
  deriving stock (Show, Eq, Ord, Generic, Typeable)

-- | Parse ordinary positive decimals without exponent notation, signs or spaces.
-- Leading integer and trailing fractional zeroes do not consume precision.
parseDecimalRate :: Text -> Either Text DecimalRate
parseDecimalRate value = do
  let (whole, suffix) = T.breakOn "." value
      fraction = if T.null suffix then "" else T.drop 1 suffix
  if not (asciiDigits whole) || (not (T.null suffix) && not (asciiDigits fraction))
    then Left "Rate must be an ordinary positive decimal"
    else do
      -- Normalize text before allocating the coefficient, so redundant zeroes do not inflate Integer work.
      let normalizedWhole = T.dropWhile (== '0') whole
          normalizedFraction = T.dropWhileEnd (== '0') fraction
      if T.length normalizedWhole > 18 || T.length normalizedFraction > 18 || (T.null normalizedWhole && T.null normalizedFraction)
        then Left "Rate must be positive with at most 18 integer and 18 fractional digits"
        else Right (DecimalRate (scientific (decimalInteger (normalizedWhole <> normalizedFraction)) (negate (T.length normalizedFraction))))

-- | Apply the same positive precision bounds directly to Scientific values,
-- including exponent notation, without a floating-point intermediate.
decimalRateFromScientific :: Scientific -> Either Text DecimalRate
decimalRateFromScientific value
  | coefficient value <= 0 = Left "Rate must be positive"
  | otherwise =
      -- Strip coefficient zeroes before testing precision; equivalent Scientific encodings must agree.
      let stripZeros digits exponentValue
            | digits `mod` 10 == 0 = stripZeros (digits `div` 10) (exponentValue + 1)
            | otherwise = (digits, exponentValue)
          (c, e) = stripZeros (coefficient value) (toInteger (base10Exponent value))
          digitCount = length (show c)
       in if e < -18 || toInteger digitCount + e > 18
            then Left "Rate exceeds supported precision"
            else Right (DecimalRate (scientific c (fromInteger e)))

-- | Render an ordinary decimal without exponent notation or trailing fractional
-- zeroes. The intended domain is rates accepted by the smart parsers.
canonicalDecimalRate :: DecimalRate -> Text
canonicalDecimalRate (DecimalRate value) =
  let c = coefficient value
      e = base10Exponent value
      digits = T.pack (show c)
      point = T.length digits + e
      rendered
        | e >= 0 = digits <> T.replicate e "0"
        | point > 0 = T.take point digits <> "." <> T.drop point digits
        | otherwise = "0." <> T.replicate (negate point) "0" <> digits
   in if T.isInfixOf "." rendered then T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') rendered) else rendered

instance ToJSON DecimalRate where toJSON = String . canonicalDecimalRate

instance FromJSON DecimalRate where
  parseJSON = withText "DecimalRate" (either (fail . T.unpack) pure . parseDecimalRate)

instance ToSchema DecimalRate where
  declareNamedSchema _ = pure $ stringSchema "DecimalRate" "^([1-9][0-9]{0,17}(\\.[0-9]{0,17}[1-9])?|0\\.[0-9]{0,17}[1-9])$"

instance FromHttpApiData DecimalRate where
  parseUrlPiece = parseDecimalRate

instance ToHttpApiData DecimalRate where
  toUrlPiece = canonicalDecimalRate

instance ToParamSchema DecimalRate where
  toParamSchema _ =
    mempty
      { _paramSchemaType = Just SwaggerString,
        _paramSchemaPattern = Just "^([1-9][0-9]{0,17}(\\.[0-9]{0,17}[1-9])?|0\\.[0-9]{0,17}[1-9])$"
      }

-- | Saved quote UUID, encoded directly as a UUID string in JSON and URLs.
newtype ExchangeQuoteId = ExchangeQuoteId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Exact two-key tagged object: source="reference" with quoteId, or
-- source="manual" with rate. Unknown, missing and cross-branch keys are rejected.
data ExchangeRateInput = ReferenceRateInput ExchangeQuoteId | ManualRateInput DecimalRate
  deriving stock (Show, Eq, Ord, Generic, Typeable)

instance ToJSON ExchangeRateInput where
  toJSON (ReferenceRateInput quote) = object ["source" .= String "reference", "quoteId" .= quote]
  toJSON (ManualRateInput value) = object ["source" .= String "manual", "rate" .= value]

instance FromJSON ExchangeRateInput where
  parseJSON = withObject "ExchangeRateInput" $ \obj -> do
    inputSource <- obj .: "source"
    case (inputSource :: Text) of
      "reference" | KM.size obj == 2 -> ReferenceRateInput <$> obj .: "quoteId"
      "manual" | KM.size obj == 2 -> ManualRateInput <$> obj .: "rate"
      _ -> fail "Expected source/quoteId or source/rate only"

instance ToSchema ExchangeRateInput where
  declareNamedSchema _ = do
    quoteSchema <- declareSchemaRef (Proxy :: Proxy ExchangeQuoteId)
    rateSchema <- declareSchemaRef (Proxy :: Proxy DecimalRate)
    let NamedSchema _ sourceSchema = enumSchema "RateSource" ["reference", "manual"]
    pure $
      NamedSchema
        (Just "ExchangeRateInput")
        mempty
          { _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject},
            _schemaProperties = [("source", Inline sourceSchema), ("quoteId", quoteSchema), ("rate", rateSchema)],
            _schemaRequired = ["source"],
            _schemaMinProperties = Just 2,
            _schemaMaxProperties = Just 2,
            _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed False),
            _schemaDescription = Just "Exactly {source: reference, quoteId: UUID} or {source: manual, rate: positive decimal string}; no other fields. Swagger 2 cannot express the disjoint object union."
          }

-- | Persisted rate provenance. Decoding allows only "manual" with all metadata
-- absent/null, or "reference" with rateDate, fetchedAt and quoteId all present.
data ExchangeRate = ExchangeRate
  { -- | Wire discriminator; unlike ExchangeRateInput this is stored as Text.
    source :: !Text,
    -- | Target major units per original major unit, as an exact decimal string.
    rate :: !DecimalRate,
    -- | Provider rate date; encoded null for a manual rate.
    rateDate :: !(Maybe Day),
    -- | Provider retrieval time; encoded null for a manual rate.
    fetchedAt :: !(Maybe UTCTime),
    -- | Saved reference quote; encoded null for a manual rate.
    quoteId :: !(Maybe ExchangeQuoteId)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON ExchangeRate where
  parseJSON value = do
    result <- genericParseJSON defaultOptions {rejectUnknownFields = True} value
    case result of
      ExchangeRate "manual" _ Nothing Nothing Nothing -> pure result
      ExchangeRate "reference" _ (Just _) (Just _) (Just _) -> pure result
      _ -> fail "Invalid exchange rate provenance"

-- | Describe a string's canonical spelling in Swagger without parsing values.
stringSchema :: Text -> Text -> NamedSchema
stringSchema schemaName patternText =
  NamedSchema
    (Just schemaName)
    mempty
      { _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaPattern = Just patternText}
      }

-- | Describe a closed set of string literals for Swagger consumers.
enumSchema :: Text -> [Text] -> NamedSchema
enumSchema schemaName literals =
  NamedSchema
    (Just schemaName)
    mempty
      { _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaEnum = Just (map String literals)}
      }
