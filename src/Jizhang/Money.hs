{-# LANGUAGE OverloadedStrings #-}

-- | Exact minor-unit arithmetic, currency-aware decimal input and deterministic splits.
-- Conversion rounds once; allocations always preserve the requested integer total.
module Jizhang.Money
  ( parseMajorAmount,
    formatMoney,
    maxRecordMinor,
    validateRecordAmount,
    validateSplitAmount,
    parseDecimalRate,
    decimalRateFromScientific,
    convertAmount,
    allocateSplits,
    apportion,
    allocateConvertedSplits,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Jizhang.API.Types.Money
import Jizhang.API.Types.User (UserId (..))

-- | Require a positive amount within the persisted record range; report totals are unbounded.
validateRecordAmount :: MinorAmount -> Either Text MinorAmount
validateRecordAmount amount@(MinorAmount value)
  | value <= 0 = Left "Amount must be positive"
  | value > maxRecordMinor = Left "Amount exceeds the supported range"
  | otherwise = Right amount

-- | Accept zero allocations while enforcing the same storage bound as record amounts.
validateSplitAmount :: MinorAmount -> Either Text MinorAmount
validateSplitAmount amount@(MinorAmount value)
  | value < 0 || value > maxRecordMinor = Left "Split amount is outside the supported range"
  | otherwise = Right amount

-- | Nonnegative major units, including zero (exact split rows may be zero).
-- Record callers additionally use validateRecordAmount.
parseMajorAmount :: CurrencyInfo -> Text -> Either Text MinorAmount
parseMajorAmount currency input = do
  let digits = minorUnits currency
      value = T.strip input
      (whole, suffix) = T.breakOn "." value
      fraction = if T.null suffix then "" else T.drop 1 suffix
  if digits < 0 || digits > 4
    then Left "Invalid currency precision"
    else
      if not (asciiDigits whole) || (not (T.null suffix) && not (asciiDigits fraction))
        then Left "Enter a nonnegative decimal amount"
        else
          if T.length fraction > digits
            then Left "Amount has too many decimal places for this currency"
            else
              -- Bound significant digits before the Integer fold; leading zeroes need no big-integer work.
              let normalizedWhole = T.dropWhile (== '0') whole
                  maximumWholeDigits = length (show maxRecordMinor) - digits
               in if T.length normalizedWhole > maximumWholeDigits
                    then Left "Split amount is outside the supported range"
                    else
                      validateSplitAmount $
                        MinorAmount $
                          decimalInteger normalizedWhole * 10 ^ digits + decimalInteger fraction * 10 ^ (digits - T.length fraction)

-- | Render signed minor units with the currency code and its catalog-defined precision.
formatMoney :: CurrencyInfo -> MinorAmount -> Text
formatMoney currency (MinorAmount amount) =
  let digits = minorUnits currency
      magnitude = abs amount
      scale = 10 ^ digits
      (whole, fraction) = magnitude `divMod` scale
      fractionalText = T.justifyRight digits '0' (T.pack (show fraction))
   in currencyCodeText (code currency)
        <> " "
        <> (if amount < 0 then "-" else "")
        <> T.pack (show whole)
        <> (if digits == 0 then "" else "." <> fractionalText)

-- | Direction: one source major unit = rate target major units. Round half-up once.
convertAmount :: CurrencyInfo -> CurrencyInfo -> DecimalRate -> MinorAmount -> Either Text MinorAmount
convertAmount original target (DecimalRate rateValue) amount@(MinorAmount value) = do
  _ <- validateRecordAmount amount
  _ <- decimalRateFromScientific rateValue
  if any (\digits -> digits < 0 || digits > 4) [minorUnits original, minorUnits target]
    then Left "Invalid currency precision"
    else do
      -- Keep both precision scaling and the rate rational so only the final minor unit is rounded.
      let rational =
            fromInteger value
              * toRational rateValue
              * (10 ^ minorUnits target % 10 ^ minorUnits original)
          rounded = floor (rational + 1 % 2)
      if rounded == 0
        then Left "Converted amount is below one minor unit"
        else validateRecordAmount (MinorAmount rounded)

-- | Validate unique participants and mode-specific values, then allocate the full amount.
-- Percentage values are basis points; exact values are already minor-unit allocations.
allocateSplits :: SplitMode -> MinorAmount -> [(UserId, MinorAmount)] -> Either Text [(UserId, MinorAmount)]
allocateSplits mode amount@(MinorAmount total) rows = do
  _ <- validateRecordAmount amount
  if null rows then Left "Select at least one participant" else Right ()
  if Set.size (Set.fromList (map fst rows)) /= length rows
    then Left "Split participants must be unique"
    else Right ()
  let weights = [(user, value) | (user, MinorAmount value) <- rows]
      values = map snd weights
      weightTotal = sum values
  if any (< 0) values then Left "Split values must be nonnegative" else Right ()
  case mode of
    EqualSplit
      | any (/= 1) values -> Left "Equal splits require value 1 for each participant"
    SharesSplit
      | any (> 32767) values -> Left "Shares must be between 0 and 32767"
      | weightTotal == 0 -> Left "Shares must have a positive total"
    PercentageSplit
      | any (> 10000) values || weightTotal /= 10000 -> Left "Percentages must total exactly 100%"
    ExactSplit
      | weightTotal /= total -> Left "Exact amounts must total the record amount"
    _ -> Right ()
  pure $ if mode == ExactSplit then rows else apportion amount weights

-- | Already-validated nonnegative weights with positive sum. The result preserves
-- caller order; UUID text, never input order, breaks largest-remainder ties.
-- No Shares-specific upper bound applies to these generic ratio weights.
apportion :: MinorAmount -> [(UserId, Integer)] -> [(UserId, MinorAmount)]
apportion (MinorAmount total) weights =
  let denominator = sum (map snd weights)
      divided = [(user, weight, (total * weight) `divMod` denominator) | (user, weight) <- weights]
      -- Floors leave fewer missing units than positive weights, so each winner needs at most one.
      missing = total - sum [quotient | (_, _, (quotient, _)) <- divided]
      ranked =
        -- Zero weights cannot receive a residual unit, even when their remainder ties at zero.
        sortOn
          -- UUID ties keep the extra unit stable when clients reorder otherwise identical split rows.
          (\(user, _, (_, remainder)) -> (Down remainder, uuidText user))
          [row | row@(_, weight, _) <- divided, weight > 0]
      winners = Set.fromList [user | (user, _, _) <- take (fromInteger missing) ranked]
   in [ (user, MinorAmount (quotient + if Set.member user winners then 1 else 0))
      | (user, _, (quotient, _)) <- divided
      ]
  where
    uuidText (UserId uuid) = UUID.toText uuid

-- | Allocate each currency independently from logical submitted ratios, not the
-- rounded source allocations (important when source minor units are coarse).
allocateConvertedSplits ::
  SplitMode ->
  MinorAmount ->
  MinorAmount ->
  [(UserId, MinorAmount)] ->
  Either Text [(UserId, MinorAmount, MinorAmount)]
allocateConvertedSplits mode original converted rows = do
  sourceRows <- allocateSplits mode original rows
  _ <- validateRecordAmount converted
  -- Reusing rounded source shares would erase small weights before the target currency sees them.
  let targetRows = Map.fromList $ apportion converted [(user, value) | (user, MinorAmount value) <- rows]
  pure [(user, sourceAmount, targetRows Map.! user) | (user, sourceAmount) <- sourceRows]
