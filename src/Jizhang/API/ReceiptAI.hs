{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.ReceiptAI
  ( ReceiptExtractionAPI,
    AdminReceiptExtractionAPI,
    receiptExtractionServer,
    adminReceiptExtractionServer,
  )
where

import Control.Exception (SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Control (liftBaseWith, restoreM)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Clock (getMonotonicTimeNSec)
import Jizhang.API.App
import Jizhang.API.Internal.Resource (ensureGroupExists, ensureGroupMember)
import Jizhang.API.Receipt.Source (receiptImageByteLimit)
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.API.Types.User (AuthUser (..))
import Jizhang.API.Types.Admin (AuthAdmin (..))
import Jizhang.API.Types.Group (GroupId (..))
import Jizhang.API.Types.ReceiptAI
import qualified Jizhang.Database.ReceiptAI as D
import qualified Jizhang.Database.Schema as S
import Jizhang.ReceiptAI.Contract
import Jizhang.ReceiptAI.Image
import Jizhang.ReceiptAI.Provider
import Jizhang.ReceiptAI.Review (validateReceiptEvidence)
import Jizhang.ReceiptAI.Runtime
import Jizhang.RequestProtection (Principal (..), requestProtectionConfig)
import Servant
import System.Timeout (timeout)

-- The enclosing bearer/admin-cookie server owns authentication and CSRF.
type ReceiptExtractionAPI =
  "groups" :> Capture "groupId" GroupId :> "receipt-extractions" :>
    ("options" :> Get '[JSON] ReceiptExtractionOptions
      :<|> ReqBody '[JSON] ReceiptImageInput :> Post '[JSON] ReceiptExtractionResponse)

type AdminReceiptExtractionAPI =
  "admin" :> "groups" :> Capture "groupId" GroupId :> "receipt-extractions" :>
    ("options" :> Get '[JSON] ReceiptExtractionOptions
      :<|> ReqBody '[JSON] ReceiptImageInput :> Post '[JSON] ReceiptExtractionResponse)

receiptExtractionServer :: AuthUser -> MyServer ReceiptExtractionAPI
receiptExtractionServer actor group =
  extractionOptions principal group :<|> extractImage principal group
  where principal = UserPrincipal $ authUserId actor

adminReceiptExtractionServer :: AuthAdmin -> MyServer AdminReceiptExtractionAPI
adminReceiptExtractionServer actor group =
  extractionOptions principal group :<|> extractImage principal group
  where principal = AdminPrincipal $ authAdminId actor

authorizeGroup :: Principal -> UUID -> MyHandler ()
authorizeGroup principal group = do
  ensureGroupExists group
  case principal of
    UserPrincipal user -> ensureGroupMember user group
    AdminPrincipal _ -> pure ()

extractionOptions :: Principal -> GroupId -> MyHandler ReceiptExtractionOptions
extractionOptions principal (GroupId group) = do
  authorizeGroup principal group
  settings <- runDBPrivate D.getReceiptAiSettings
  configuration <- asks appReceiptAIConfig
  contract <- asks appReceiptAIContract
  maximumBytes <- asks (receiptImageByteLimit . requestProtectionConfig . appRequestProtection)
  let configured = case effectiveReceiptAIKey configuration (S._receiptAiSettingsApiKeyEncrypted settings) of
        Right _ -> True
        Left _ -> False
  pure $ ReceiptExtractionOptions
    (maximumBytes > 0 && S._receiptAiSettingsEnabled settings && configured)
    (contractModel contract) (contractReasoningEffort contract) maximumBytes ["image/jpeg"]

extractImage :: Principal -> GroupId -> ReceiptImageInput -> MyHandler ReceiptExtractionResponse
extractImage principal (GroupId group) input = do
  authorizeGroup principal group
  enforceExpensiveLimit principal
  settings <- runDBPrivate D.getReceiptAiSettings
  configuration <- asks appReceiptAIConfig
  maximumBytes <- asks (receiptImageByteLimit . requestProtectionConfig . appRequestProtection)
  unless (maximumBytes > 0) $ throwError $ err503 {errBody = "Receipt image import is disabled by the server upload limit"}
  unless (S._receiptAiSettingsEnabled settings) $ throwError $ err503 {errBody = "Receipt extraction is disabled; manual entry remains available"}
  key <- either (const unavailable) pure $ effectiveReceiptAIKey configuration (S._receiptAiSettingsApiKeyEncrypted settings)
  validated <- liftIO $ validateReceiptImage maximumBytes input
  image <- case validated of
    Left TooLarge -> throwError $ err413 {errBody = "Receipt image exceeds the server upload limit"}
    Left UnsupportedMedia -> throwError $ err415 {errBody = "Only normalized JPEG receipt images are supported"}
    Left InvalidImage -> throwError $ err400 {errBody = "Invalid receipt JPEG image"}
    Right value -> pure value
  contract <- asks appReceiptAIContract
  callback <- asks appExtractReceipt
  callId <- liftIO nextRandom
  started <- liftIO getCurrentTime
  let row = initialUsage principal group callId started settings contract image
  -- No SQL lock spans provider I/O. Cancellation still attempts one terminal write.
  attempt <- liftBaseWith (\run -> mask $ \restore -> do
    inserted <- run $ runDBPrivate (D.insertReceiptAiUsage row)
    case inserted of
      Left failure -> pure $ Left failure
      Right () -> do
        outboundStart <- getMonotonicTimeNSec
        outcome <- try $ restore (callback key image)
        outboundEnd <- getMonotonicTimeNSec
        finished <- max started <$> getCurrentTime
        let duration = fromInteger $ min (toInteger (maxBound :: Int64)) $
              max 0 (toInteger outboundEnd - toInteger outboundStart) `div` 1000000
            result = case outcome of
              Right value -> value {attemptDurationMs = duration}
              Left failure -> ReceiptAIAttempt
                (if isAsync failure then ReceiptAIInterrupted else ReceiptAITransportError)
                Nothing Nothing Nothing Nothing Nothing Nothing duration
            terminal = finishUsage finished row result
        committed <- try (timeout 5000000 $ run $ runDBPrivate (D.finishReceiptAiUsage terminal))
          :: IO (Either SomeException (Maybe (Either ServerError Bool)))
        case outcome of
          Left failure | isAsync failure -> throwIO failure
          _ -> case committed of
            Left failure | isAsync failure -> throwIO failure
            Right (Just (Right True)) -> pure $ Right result
            _ -> pure $ Left finalizationFailure) >>= restoreM
  authorizeGroup principal group
  case (attemptOutcome attempt, attemptEvidence attempt) of
    (ReceiptAICompleted, Just evidence) -> do
      catalog <- asks appCurrencies
      pure $ ReceiptExtractionResponse callId
        (maybe (contractModel contract) id $ attemptActualModel attempt)
        (contractPromptVersion contract) (attemptTitle attempt) evidence (validateReceiptEvidence catalog evidence)
    (ReceiptAITimeout, _) -> throwError $ err504 {errBody = "Receipt extraction timed out; check usage before retrying"}
    (ReceiptAIHttpError, _) -> unavailable
    (ReceiptAITransportError, _) -> unavailable
    (ReceiptAIInterrupted, _) -> unavailable
    (ReceiptAIRefused, _) -> throwError $ err422 {errBody = "The provider could not extract this receipt; correct the image or enter expenses manually"}
    _ -> throwError $ err422 {errBody = "Receipt extraction returned invalid or incomplete evidence; retry explicitly or enter expenses manually"}
  where
    unavailable = throwError $ err503 {errBody = "Receipt extraction is unavailable; retry explicitly or use manual entry"}
    isAsync :: SomeException -> Bool
    isAsync failure = case fromException failure :: Maybe SomeAsyncException of
      Just _ -> True
      Nothing -> False
    finalizationFailure = err503
      { errBody = "Extraction could not be recorded; the provider may have charged this attempt. Check usage before retrying.",
        errHeaders = [("Cache-Control", "no-store")]
      }

initialUsage :: Principal -> UUID -> UUID -> UTCTime -> S.ReceiptAiSettings -> ReceiptAIContract -> ValidatedReceiptImage -> S.ReceiptAiUsage
initialUsage principal group callId started settings contract image = S.ReceiptAiUsage
  { S._receiptAiUsageId = callId,
    S._receiptAiUsageStartedAt = started,
    S._receiptAiUsageFinishedAt = Nothing,
    S._receiptAiUsagePrincipalKind = case principal of UserPrincipal _ -> "user"; AdminPrincipal _ -> "admin",
    S._receiptAiUsageUser = S.UserId $ case principal of UserPrincipal user -> Just user; _ -> Nothing,
    S._receiptAiUsageAdmin = S.AdminId $ case principal of AdminPrincipal admin -> Just admin; _ -> Nothing,
    S._receiptAiUsageGroup = S.GroupId $ Just group,
    S._receiptAiUsageRequestedModel = contractModel contract,
    S._receiptAiUsageActualModel = Nothing,
    S._receiptAiUsagePromptVersion = contractPromptVersion contract,
    S._receiptAiUsagePromptSha256 = receiptAIContractHash contract,
    S._receiptAiUsageImageSha256 = validatedImageSha256 image,
    S._receiptAiUsageEvidenceSha256 = Nothing,
    S._receiptAiUsageSettingsRevision = S._receiptAiSettingsRevision settings,
    S._receiptAiUsageStatus = "in_flight",
    S._receiptAiUsageProviderRequestId = Nothing,
    S._receiptAiUsageHttpStatus = Nothing,
    S._receiptAiUsageInputTokens = Nothing,
    S._receiptAiUsageCachedInputTokens = Nothing,
    S._receiptAiUsageOutputTokens = Nothing,
    S._receiptAiUsageReasoningTokens = Nothing,
    S._receiptAiUsageDurationMs = Nothing,
    S._receiptAiUsagePricingVersion = "gpt-5.4-mini-2026-03-17-v1",
    S._receiptAiUsageInputNanoUsdPerToken = 750,
    S._receiptAiUsageCachedInputNanoUsdPerToken = 75,
    S._receiptAiUsageOutputNanoUsdPerToken = 4500,
    S._receiptAiUsageEstimatedCostNanoUsd = Nothing
  }

finishUsage :: UTCTime -> S.ReceiptAiUsage -> ReceiptAIAttempt -> S.ReceiptAiUsage
finishUsage finished row attempt = row
  { S._receiptAiUsageFinishedAt = Just finished,
    S._receiptAiUsageActualModel = attemptActualModel attempt,
    S._receiptAiUsageEvidenceSha256 = case attemptOutcome attempt of
      ReceiptAICompleted -> digest <$> attemptEvidence attempt
      _ -> Nothing,
    S._receiptAiUsageStatus = case (attemptOutcome attempt, attemptEvidence attempt) of
      (ReceiptAICompleted, Just _) -> "completed"
      (ReceiptAICompleted, Nothing) -> "invalid_response"
      (ReceiptAIInvalidResponse, _) -> "invalid_response"
      (ReceiptAIRefused, _) -> "refused"
      (ReceiptAIHttpError, _) -> "http_error"
      (ReceiptAITransportError, _) -> "transport_error"
      (ReceiptAITimeout, _) -> "timeout"
      (ReceiptAIInterrupted, _) -> "interrupted",
    S._receiptAiUsageProviderRequestId = attemptProviderRequestId attempt,
    S._receiptAiUsageHttpStatus = fromIntegral <$> attemptHttpStatus attempt,
    S._receiptAiUsageInputTokens = attemptUsage attempt >>= usageInputTokens,
    S._receiptAiUsageCachedInputTokens = attemptUsage attempt >>= usageCachedInputTokens,
    S._receiptAiUsageOutputTokens = attemptUsage attempt >>= usageOutputTokens,
    S._receiptAiUsageReasoningTokens = attemptUsage attempt >>= usageReasoningTokens,
    S._receiptAiUsageDurationMs = Just $ attemptDurationMs attempt,
    S._receiptAiUsageEstimatedCostNanoUsd = estimateReceiptAICost (attemptActualModel attempt) (attemptUsage attempt)
  }
  where digest evidence = T.pack $ show (hash (canonicalReceiptEvidence evidence) :: Digest SHA256)
