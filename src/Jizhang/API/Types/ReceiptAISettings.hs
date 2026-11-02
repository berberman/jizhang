{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jizhang.API.Types.ReceiptAISettings where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Jizhang.ReceiptAI.Runtime (parseReceiptAIKey)

options :: Options
options = defaultOptions {fieldLabelModifier = drop 3, rejectUnknownFields = True}

requireFields :: [Text] -> Swagger.NamedSchema -> Swagger.NamedSchema
requireFields fields (Swagger.NamedSchema name schema) = Swagger.NamedSchema name schema {Swagger._schemaRequired = fields}

schemaOptions :: Swagger.SchemaOptions
schemaOptions = Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = drop 3}

data ReceiptAISettings = ReceiptAISettings
  { rasrevision :: Int64,
    rasenabled :: Bool,
    rasavailable :: Bool,
    rasunavailableReason :: Maybe Text,
    raskeyConfigured :: Bool,
    raskeySource :: Text,
    rascanStoreKey :: Bool,
    rasmodel :: Text,
    rasreasoningEffort :: Text,
    raspricing :: ReceiptAIPricing
  } deriving (Eq, Show, Generic)

instance ToJSON ReceiptAISettings where toJSON = genericToJSON options
instance ToSchema ReceiptAISettings where
  declareNamedSchema proxy = requireFields ["revision", "enabled", "available", "unavailableReason", "keyConfigured", "keySource", "canStoreKey", "model", "reasoningEffort", "pricing"] <$> genericDeclareNamedSchema schemaOptions proxy
instance FromJSON ReceiptAISettings where parseJSON = genericParseJSON options

data ReceiptAIPricing = ReceiptAIPricing
  { rapversion :: Text,
    rapcurrency :: Text,
    rapscale :: Int,
    rapinputNanoUsdPerToken :: Text,
    rapcachedInputNanoUsdPerToken :: Text,
    rapoutputNanoUsdPerToken :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ReceiptAIPricing where toJSON = genericToJSON options
instance FromJSON ReceiptAIPricing where parseJSON = genericParseJSON options
instance ToSchema ReceiptAIPricing where declareNamedSchema = genericDeclareNamedSchema schemaOptions

receiptAIPricing :: ReceiptAIPricing
receiptAIPricing = ReceiptAIPricing "gpt-5.4-mini-2026-03-17-v1" "USD" 9 "750" "75" "4500"

data ReceiptAISettingsRequest = ReceiptAISettingsRequest
  { rsrrevision :: Int64,
    rsrenabled :: Bool,
    rsrkeyAction :: Text,
    rsrapiKey :: Maybe Text
  } deriving (Generic)

instance ToJSON ReceiptAISettingsRequest where toJSON = genericToJSON options
instance ToSchema ReceiptAISettingsRequest where
  declareNamedSchema proxy = requireFields ["revision", "enabled", "keyAction", "apiKey"] <$> genericDeclareNamedSchema schemaOptions proxy
instance FromJSON ReceiptAISettingsRequest where
  parseJSON = withObject "ReceiptAISettingsRequest" $ \fields -> do
    unless (length fields == 4 && all (`KM.member` fields) ["revision", "enabled", "keyAction", "apiKey"]) $
      fail "Receipt AI settings have missing or unknown fields"
    revision <- fields .: "revision"
    unless (revision >= (0 :: Int64)) $ fail "Invalid settings revision"
    enabled <- fields .: "enabled"
    action <- fields .: "keyAction"
    secret <- fields .: "apiKey"
    case (action :: Text, secret) of
      ("keep", Nothing) -> pure ()
      ("clear", Nothing) -> pure ()
      ("replace", Just value) -> either (const $ fail "Invalid API key") (const $ pure ()) (parseReceiptAIKey value)
      _ -> fail "Invalid API key action"
    pure $ ReceiptAISettingsRequest revision enabled action secret
