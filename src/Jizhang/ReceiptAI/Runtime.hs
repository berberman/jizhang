{-# LANGUAGE OverloadedStrings #-}

module Jizhang.ReceiptAI.Runtime
  ( ReceiptAIKey,
    parseReceiptAIKey,
    receiptAIKeyText,
    ReceiptAIConfig,
    parseReceiptAIConfig,
    defaultReceiptAIConfig,
    receiptAIEnvironmentKey,
    receiptAIEncryptionKey,
    ReceiptAIKeyError (..),
    effectiveReceiptAIKey,
    receiptAIKeyAad,
  )
where

import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Jizhang.Auth.Security (SecretKey, decryptSecret, parseSecretKey)

newtype ReceiptAIKey = ReceiptAIKey Text

data ReceiptAIConfig = ReceiptAIConfig
  { receiptAIEnvironmentKey :: !(Maybe ReceiptAIKey),
    receiptAIEncryptionKey :: !(Maybe SecretKey)
  }

data ReceiptAIKeyError = KeyMissing | StoredKeyUnreadable
  deriving (Eq, Show)

parseReceiptAIKey :: Text -> Either Text ReceiptAIKey
parseReceiptAIKey value
  | T.null value || T.length value > 512 || T.any (\c -> isSpace c || isControl c) value =
      Left "OPENAI_API_KEY must be nonempty, at most 512 characters, and contain no whitespace or control characters"
  | otherwise = Right $ ReceiptAIKey value

receiptAIKeyText :: ReceiptAIKey -> Text
receiptAIKeyText (ReceiptAIKey value) = value

parseReceiptAIConfig :: Maybe Text -> Maybe Text -> Either Text ReceiptAIConfig
parseReceiptAIConfig environment encryption = do
  environmentKey <- traverse parseReceiptAIKey $ environment >>= nonempty
  encryptionKey <- traverse parseEncryptionKey encryption
  pure $ ReceiptAIConfig environmentKey encryptionKey
  where
    nonempty value = if T.null value then Nothing else Just value
    parseEncryptionKey value = case parseSecretKey value of
      Right key -> Right key
      Left _ -> Left "RECEIPT_AI_SECRET_KEY must be canonical standard base64 encoding of exactly 32 bytes"

defaultReceiptAIConfig :: ReceiptAIConfig
defaultReceiptAIConfig = ReceiptAIConfig Nothing Nothing

receiptAIKeyAad :: BS.ByteString
receiptAIKeyAad = "receipt_ai_settings:1:api_key"

effectiveReceiptAIKey :: ReceiptAIConfig -> Maybe Text -> Either ReceiptAIKeyError ReceiptAIKey
effectiveReceiptAIKey config stored = case receiptAIEnvironmentKey config of
  Just key -> Right key
  Nothing -> case stored of
    Nothing -> Left KeyMissing
    Just ciphertext -> case receiptAIEncryptionKey config of
      Nothing -> Left StoredKeyUnreadable
      Just key -> case decryptSecret key receiptAIKeyAad ciphertext >>= parseReceiptAIKey of
        Left _ -> Left StoredKeyUnreadable
        Right credential -> Right credential
