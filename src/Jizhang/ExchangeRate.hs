{-# LANGUAGE OverloadedStrings #-}

-- | Fetch and validate daily reference rates without converting or mutating ledger records.
module Jizhang.ExchangeRate
  ( ExchangeRateError (..),
    ReferenceRate (..),
    fetchReferenceRate,
    decodeReferenceRate,
  )
where

import Control.Exception (catch)
import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecode, withObject, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, defaultTimeLocale, formatTime, getCurrentTime, parseTimeM, utctDay)
import Jizhang.API.Types.Money
  ( CurrencyCode,
    DecimalRate,
    currencyCodeText,
    decimalRateFromScientific,
  )
import Network.HTTP.Client
  ( BodyReader,
    HttpException,
    Manager,
    Request (..),
    brRead,
    defaultRequest,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
    withResponse,
  )
import Network.HTTP.Types.Status (statusCode)

-- | Distinguish transport/HTTP failure from an oversized or invalid successful response.
data ExchangeRateError = ProviderUnavailable | InvalidProviderResponse
  deriving (Show, Eq)

-- | Target major units per source major unit, with the provider's reference day.
-- This day is not necessarily today or the transaction date.
data ReferenceRate = ReferenceRate
  { rate :: !DecimalRate,
    rateDate :: !Day
  }
  deriving (Show, Eq)

-- Bound both streamed HTTP responses and direct decoder input before JSON parsing.
maxBodyBytes :: Int
maxBodyBytes = 64 * 1024

-- | Latest available daily reference, never a historical-date lookup. The caller
-- supplies a shared TLS manager with automatic connection retries disabled.
fetchReferenceRate :: Manager -> CurrencyCode -> CurrencyCode -> IO (Either ExchangeRateError ReferenceRate)
fetchReferenceRate manager source target = fetch `catch` unavailable
  where
    request =
      defaultRequest
        { secure = True,
          host = "api.frankfurter.dev",
          port = 443,
          path = "/v2/rate/" <> lowerCode source <> "/" <> lowerCode target,
          redirectCount = 0,
          responseTimeout = responseTimeoutMicro (8 * 1000000),
          checkResponse = \_ _ -> pure (),
          requestHeaders = [("Accept", "application/json")]
        }
    lowerCode = TE.encodeUtf8 . T.toLower . currencyCodeText
    fetch = withResponse request manager $ \response -> do
      let status = statusCode (responseStatus response)
      if status < 200 || status >= 300
        then pure (Left ProviderUnavailable)
        else do
          body <- readBoundedBody (responseBody response)
          today <- utctDay <$> getCurrentTime
          pure (body >>= decodeReferenceRate source target today)
    unavailable :: HttpException -> IO (Either ExchangeRateError ReferenceRate)
    unavailable _ = pure (Left ProviderUnavailable)

-- Stop at the first oversized chunk; never retain or decode an unbounded body.
-- withResponse closes the connection without draining the remainder on failure.
readBoundedBody :: BodyReader -> IO (Either ExchangeRateError BL.ByteString)
readBoundedBody reader = go maxBodyBytes []
  where
    go remaining chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then pure (Right (BL.fromChunks (reverse chunks)))
        else
          if BS.length chunk > remaining
            then pure (Left InvalidProviderResponse)
            else go (remaining - BS.length chunk) (chunk : chunks)

-- | Validate the provider's numeric JSON exactly, without a Double intermediate.
-- The explicit UTC day makes future-date validation deterministic in tests.
decodeReferenceRate :: CurrencyCode -> CurrencyCode -> Day -> BL.ByteString -> Either ExchangeRateError ReferenceRate
decodeReferenceRate source target today body
  | BL.length (BL.take (fromIntegral maxBodyBytes + 1) body) > fromIntegral maxBodyBytes = Left InvalidProviderResponse
  | otherwise = case eitherDecode body >>= parseEither parseRate of
      Left _ -> Left InvalidProviderResponse
      Right result -> Right result
  where
    parseRate = withObject "Frankfurter reference rate" $ \obj -> do
      unless (KM.size obj == 4) (fail "Expected only date, base, quote and rate")
      base <- obj .: "base"
      quote <- obj .: "quote"
      unless (base == source && quote == target) (fail "Unexpected currency pair")
      dateText <- obj .: "date"
      dateValue <- case parseTimeM True defaultTimeLocale "%F" (T.unpack dateText) of
        Just day | T.pack (formatTime defaultTimeLocale "%F" day) == dateText -> pure day
        _ -> fail "Invalid reference date"
      unless (dateValue <= today) (fail "Future reference date")
      numericRate <- obj .: "rate"
      rateValue <- case numericRate of
        Number value -> either (fail . T.unpack) pure (decimalRateFromScientific value)
        _ -> fail "Expected a numeric reference rate"
      pure (ReferenceRate rateValue dateValue)
