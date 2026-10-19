{-# LANGUAGE OverloadedStrings #-}

-- | Explicitly fetch and persist group-scoped reference quotes. Provider I/O happens
-- outside transactions; persistence rechecks currency policy and authorization.
module Jizhang.API.ExchangeRate
  ( createExchangeQuote,
    createAdminExchangeQuote,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (UserPrincipal, AdminPrincipal))
import Jizhang.API.Types
import qualified Jizhang.API.Types.ExchangeRate as Q
import Jizhang.API.Internal.Error (fromStoredData, fetchOrFail)
import Jizhang.API.Group.Projection (storedGroupSettings)
import Jizhang.API.Record.Input (inputCurrency)
import Jizhang.API.Internal.Resource (ensureGroupMember)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.ExchangeRate as DQ
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.ExchangeRate as Provider
import Log.Class (logAttention_)
import Servant

-- | Charge expensive-operation capacity before requiring the member's group access.
createExchangeQuote :: AuthUser -> GroupId -> Q.ExchangeQuoteRequest -> MyHandler Q.ExchangeQuote
createExchangeQuote actor gid request = do
  enforceExpensiveLimit (UserPrincipal $ authUserId actor)
  createQuote (Just $ authUserId actor) gid request

-- Admin authentication, cookie and XSRF checks are supplied by the enclosing API.
createAdminExchangeQuote :: AuthAdmin -> GroupId -> Q.ExchangeQuoteRequest -> MyHandler Q.ExchangeQuote
createAdminExchangeQuote actor gid request = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId actor)
  createQuote Nothing gid request

-- | Only cross-currency single-currency groups need quotes; a missing actor denotes
-- operator access. Saving a quote alone does not lock the group's currency settings.
createQuote :: Maybe UUID -> GroupId -> Q.ExchangeQuoteRequest -> MyHandler Q.ExchangeQuote
createQuote actorId (GroupId gid) (Q.ExchangeQuoteRequest sourceCode) = do
  group <- fst <$> fetchOrFail "Group" gid (runDB $ D.getGroupWithMembers gid)
  mapM_ (`ensureGroupMember` gid) actorId
  catalog <- asks appCurrencies
  sourceInfo <- inputCurrency catalog sourceCode
  (mode, targetInfo@(CurrencyInfo targetCode _ _)) <-
    fromStoredData $ storedGroupSettings catalog group
  unless (mode == SingleCurrency && sourceCode /= targetCode) $
    throwError $
      err400 {errBody = "This group does not require conversion for this currency"}
  fetchRate <- asks appFetchReferenceRate
  -- External I/O never holds a database connection or row lock.
  fetched <- liftIO $ fetchRate sourceCode targetCode
  reference <- case fetched of
    Right referenceRate -> pure referenceRate
    Left failure -> do
      let status = case failure of
            Provider.ProviderUnavailable -> "unavailable"
            Provider.InvalidProviderResponse -> "invalid-response"
      logAttention_ $ "Reference rate " <> currencyCodeText sourceCode <> "/" <> currencyCodeText targetCode <> " " <> status
      throwError $ err503 {errBody = "Automatic rate unavailable; retry or enter a manual rate"}
  fetchedTime <- liftIO getCurrentTime
  let Provider.ReferenceRate quoteRate@(DecimalRate numericRate) effectiveDate = reference
  -- Lock and re-read policy and membership together, then persist before either
  -- row lock is released. Quote requests must not set currency_locked themselves.
  result <- runDBTransaction $ do
    (currentGroup, authorized) <- DQ.lockExchangeQuoteContext gid actorId
    case currentGroup of
      Nothing -> pure $ Left err404 {errBody = "Group not found"}
      Just current
        | not authorized -> pure $ Left err403 {errBody = "You are not a member of this group"}
        | S._currencyMode current /= S._currencyMode group
            || S._defaultCurrency current /= S._defaultCurrency group ->
            pure $ Left err409 {errBody = "Group currency settings changed; reload and try again"}
        | otherwise ->
            Right
              <$> DQ.insertExchangeQuote
                gid
                (currencyCodeText sourceCode)
                (currencyCodeText targetCode)
                numericRate
                effectiveDate
                fetchedTime
  quote <- either throwError pure result
  pure $
    Q.ExchangeQuote
      (ExchangeQuoteId $ S._quoteId quote)
      sourceInfo
      targetInfo
      quoteRate
      "frankfurter"
      effectiveDate
      fetchedTime
