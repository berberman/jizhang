{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jizhang.ReceiptAI.Contract
  ( ReceiptAIContract (..),
    loadReceiptAIContract,
    validateReceiptAIContract,
    receiptAIContractHash,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Paths_jizhang (getDataFileName)

-- This is deployment data, never a request-supplied prompt or destination.
data ReceiptAIContract = ReceiptAIContract
  { contractPromptVersion :: !Text,
    contractModel :: !Text,
    contractReasoningEffort :: !Text,
    contractInstructions :: !Text,
    contractSchema :: !Value
  }
  deriving (Eq, Generic)

instance ToJSON ReceiptAIContract where
  toJSON contract =
    object
      [ "promptVersion" .= contractPromptVersion contract,
        "model" .= contractModel contract,
        "reasoningEffort" .= contractReasoningEffort contract,
        "instructions" .= contractInstructions contract,
        "schema" .= contractSchema contract
      ]

instance FromJSON ReceiptAIContract where
  parseJSON = withObject "ReceiptAIContract" $ \fields -> do
    unless (Set.fromList (KM.keys fields) == Set.fromList ["promptVersion", "model", "reasoningEffort", "instructions", "schema"]) $
      fail "Receipt extraction contract has missing or unknown fields"
    contract <- ReceiptAIContract <$> fields .: "promptVersion" <*> fields .: "model" <*> fields .: "reasoningEffort" <*> fields .: "instructions" <*> fields .: "schema"
    either (fail . T.unpack) pure (validateReceiptAIContract contract)

validateReceiptAIContract :: ReceiptAIContract -> Either Text ReceiptAIContract
validateReceiptAIContract contract = do
  unless (contractPromptVersion contract == "receipt-v2" && contractModel contract == "gpt-5.4-mini-2026-03-17" && contractReasoningEffort contract == "low") $
    Left "Unsupported receipt extraction contract version, model or reasoning effort"
  unless (not (T.null $ T.strip $ contractInstructions contract) && T.length (contractInstructions contract) <= 32768 && not (T.any (== '\0') $ contractInstructions contract)) $
    Left "Invalid receipt extraction instructions"
  properties <- strictSchema ["title", "evidence"] (contractSchema contract)
  unless
    ( KM.lookup "title" properties
        == Just
          ( object
              [ "anyOf"
                  .= [ object ["type" .= String "string", "minLength" .= (1 :: Int), "maxLength" .= (512 :: Int), "pattern" .= String "[^\\s]"],
                       object ["type" .= String "null"]
                     ]
              ]
          )
    )
    $ Left "Receipt extraction schema does not match the title contract"
  case KM.lookup "evidence" properties of
    Just evidence -> do
      _ <- strictSchema ["document_kind", "merchant", "transaction_date", "currency", "items", "adjustments", "totals", "suggested_tips", "issues"] evidence
      pure ()
    Nothing -> Left "Receipt extraction schema is missing evidence"
  pure contract
  where
    strictSchema keys (Object schema) = do
      unless (KM.lookup "type" schema == Just (String "object") && KM.lookup "additionalProperties" schema == Just (Bool False)) $
        Left "Receipt extraction schema must be a strict object"
      case (KM.lookup "properties" schema, KM.lookup "required" schema) of
        (Just (Object properties), Just (Array required)) -> do
          let expected = Set.fromList keys
          unless
            ( Set.fromList (KM.keys properties) == expected
                && length required == length keys
                && Set.fromList (foldr (:) [] required) == Set.map (String . Key.toText) expected
            )
            $ Left "Receipt extraction schema has incorrect required keys"
          pure properties
        _ -> Left "Receipt extraction schema is missing properties or required keys"
    strictSchema _ _ = Left "Receipt extraction schema must be an object"

receiptAIContractHash :: ReceiptAIContract -> Text
receiptAIContractHash contract = T.pack $ show (hash (LBS.toStrict $ encode contract) :: Digest SHA256)

loadReceiptAIContract :: IO ReceiptAIContract
loadReceiptAIContract = do
  path <- getDataFileName "data/receipt-extraction.json"
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case result of
    Left _ -> fail "Cannot load packaged receipt extraction contract"
    Right bytes -> case eitherDecodeStrict' bytes of
      Left _ -> fail "Invalid packaged receipt extraction contract"
      Right contract -> pure contract
