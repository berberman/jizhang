{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Admin.ReceiptAI.Settings
  ( ReceiptAISettingsAPI,
    receiptAISettingsServer,
  ) where

import Control.Monad (unless, when)
import Control.Monad.Reader (asks, liftIO)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Jizhang.API.App
import Jizhang.API.Receipt.Source (receiptImageByteLimit)
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.API.Types.Admin (AuthAdmin (..))
import Jizhang.API.Types.ReceiptAISettings
import Jizhang.Auth.Security (encryptSecret)
import qualified Jizhang.Database.ReceiptAI as D
import qualified Jizhang.Database.Schema as S
import Jizhang.ReceiptAI.Contract (ReceiptAIContract (..))
import Jizhang.ReceiptAI.Runtime
import Jizhang.RequestProtection (Principal (..), requestProtectionConfig)
import Servant

type ReceiptAISettingsAPI =
  "admin" :> "receipt-ai" :> "settings" :>
    (Get '[JSON] ReceiptAISettings
      :<|> ReqBody '[JSON] ReceiptAISettingsRequest :> Put '[JSON] ReceiptAISettings)

receiptAISettingsServer :: AuthAdmin -> MyServer ReceiptAISettingsAPI
receiptAISettingsServer admin = getSettings :<|> (\request -> do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId admin)
  putSettings request)

settingsResponse :: S.ReceiptAiSettings -> MyHandler ReceiptAISettings
settingsResponse row = do
  config <- asks appReceiptAIConfig
  contract <- asks appReceiptAIContract
  maximumBytes <- asks (receiptImageByteLimit . requestProtectionConfig . appRequestProtection)
  let stored = S._receiptAiSettingsApiKeyEncrypted row
      environmentOwned = isJust (receiptAIEnvironmentKey config)
      reason
        | maximumBytes <= 0 = Just "upload_limit"
        | not (S._receiptAiSettingsEnabled row) = Just "disabled"
        | otherwise = case effectiveReceiptAIKey config stored of
            Left KeyMissing -> Just "key_missing"
            Left StoredKeyUnreadable -> Just "stored_key_unreadable"
            Right _ -> Nothing
      source | environmentOwned = "environment"
             | isJust stored = "database"
             | otherwise = "none"
  pure $ ReceiptAISettings
    (S._receiptAiSettingsRevision row)
    (S._receiptAiSettingsEnabled row)
    (not $ isJust reason)
    reason
    (environmentOwned || isJust stored)
    source
    (isJust (receiptAIEncryptionKey config) && not environmentOwned)
    (contractModel contract)
    (contractReasoningEffort contract)
    receiptAIPricing

getSettings :: MyHandler ReceiptAISettings
getSettings = runDBPrivate D.getReceiptAiSettings >>= settingsResponse

putSettings :: ReceiptAISettingsRequest -> MyHandler ReceiptAISettings
putSettings request = do
  current <- runDBPrivate D.getReceiptAiSettings
  let revision = rsrrevision request
      conflict = throwError $ err409 {errBody = "Receipt AI settings changed; reload before saving"}
      conflict :: MyHandler a
  unless (revision == S._receiptAiSettingsRevision current) conflict
  when (revision < 0) $ throwError $ err400 {errBody = "Invalid settings revision"}
  when (revision == (maxBound :: Int64)) $ throwError $ err409 {errBody = "Receipt AI settings revision exhausted"}
  config <- asks appReceiptAIConfig
  when (isJust (receiptAIEnvironmentKey config) && rsrkeyAction request /= "keep") $
    throwError $ err409 {errBody = "Receipt AI API key is managed by the environment"}
  encrypted <- case (rsrkeyAction request, rsrapiKey request) of
    ("keep", Nothing) -> pure $ S._receiptAiSettingsApiKeyEncrypted current
    ("clear", Nothing) -> pure Nothing
    ("replace", Just value) -> do
      _ <- either (const $ throwError $ err400 {errBody = "Invalid API key"}) pure (parseReceiptAIKey value)
      key <- maybe (throwError $ err503 {errBody = "Configure RECEIPT_AI_SECRET_KEY before saving an API key"}) pure (receiptAIEncryptionKey config)
      Just <$> liftIO (encryptSecret key receiptAIKeyAad value)
    _ -> throwError $ err400 {errBody = "Invalid API key action"}
  saved <- runDBPrivateTransaction $ D.saveReceiptAiSettings revision (rsrenabled request) encrypted
  maybe conflict settingsResponse saved
