{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTTP and persistence regressions for immutable exchange snapshots, explicit fetches
-- and authorization or currency-policy changes while a provider request is in flight.
module ExchangeQuoteTest (exchangeQuoteTests) where

import APITest
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.Aeson (object, toJSON, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Only (..), query_)
import Jizhang.API.App (AppEnv (appFetchReferenceRate))
import Jizhang.API.Types
import qualified Jizhang.API.Types.ExchangeRate as Q
import qualified Jizhang.ExchangeRate as FX
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status
import Servant.Client
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

-- | Inject deterministic providers into the real app; reads and saves must not fetch rates.
exchangeQuoteTests :: TestTree
exchangeQuoteTests =
  testGroup
    "Exchange quotes"
    [ testCase "refresh persists new quotes; saving and editing preserve explicit snapshots" $ do
        calls <- newIORef (0 :: Int)
        firstRate <- either (assertFailure . T.unpack) pure (parseDecimalRate "1.2")
        nextRate <- either (assertFailure . T.unpack) pure (parseDecimalRate "1.5")
        let provider _ _ = do
              n <- atomicModifyIORef' calls (\n -> (n + 1, n))
              pure $ Right $ FX.ReferenceRate (if n == 0 then firstRate else nextRate) (read "2020-01-02")
        withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") (\env -> env {appFetchReferenceRate = provider}) $ \env _ _ conn -> do
          token <- setupUser env "owner"
          ownerId <- fixtureUserId env "owner"
          adminToken <- setupAdmin env "admin" "adminpass123"
          let tc = mkTC token
              admin = mkAdminTC adminToken
          group <- runClient env $ cCreateGroup tc "Snapshots"
          let gid = grpId group
          first <- runClient env $ cExchangeQuote tc gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          second <- runClient env $ cExchangeQuote tc gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          assertBool "refresh creates a distinct immutable quote" (first.quoteId /= second.quoteId)
          first.rate @?= firstRate
          second.rate @?= nextRate
          first.sourceCurrency.code @?= CurrencyCode "EUR"
          first.targetCurrency.code @?= CurrencyCode "USD"
          first.provider @?= "frankfurter"
          unlocked <- runClient env $ cGetGroupById tc gid
          assertEqual "quotes alone do not lock" False unlocked.currencyLocked
          let expense title currency exchange = ExpenseRecordRequest title Nothing (MinorAmount 10000) currency ownerId (read "2020-01-01") EqualSplit [RecordSplitRequest ownerId (MinorAmount 1)] (Just exchange)
              original = expense "Old visible snapshot" (CurrencyCode "EUR") (ReferenceRateInput first.quoteId)
          saved <- runClient env $ cAddExpense tc gid original
          case saved of
            ExpenseRecord {settlementAmountMinor = amount, exchangeRate = Just provenance} -> do
              assertEqual "old snapshot still saves" (MinorAmount 12000) amount
              provenance @?= ExchangeRate "reference" firstRate (Just $ read "2020-01-02") (Just first.fetchedAt) (Just first.quoteId)
            _ -> assertFailure "expected converted expense with reference provenance"
          edited <- runClient env $ cUpdateExpense tc gid (extractRecordId saved) (expense "Renamed without refresh" (CurrencyCode "EUR") (ReferenceRateInput first.quoteId))
          reloaded <- runClient env $ cGetRecord tc gid (extractRecordId edited)
          assertEqual "edit persists identical immutable projection" edited reloaded
          _ <- runClient env $ cAddExpense tc gid (expense "Manual" (CurrencyCode "EUR") (ManualRateInput nextRate))
          other <- runClient env $ cCreateGroup tc "Other quote scope"
          runClientExpectStatus env status404 $ cAddExpense tc (grpId other) original
          runClientExpectStatus env status400 $ cAddExpense tc gid (expense "Wrong pair" (CurrencyCode "GBP") (ReferenceRateInput first.quoteId))
          runClientExpectStatus env status404 $ cAddExpense tc gid (expense "Missing quote" (CurrencyCode "EUR") (ReferenceRateInput $ ExchangeQuoteId UUID.nil))
          assertEqual "saves reads and edits never fetch" 2 =<< readIORef calls
          adminQuote <- runClient env $ cAdminExchangeQuote admin gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          adminQuote.rate @?= nextRate
          assertEqual "explicit admin fetch" 3 =<< readIORef calls
          persisted <- query_ conn "SELECT count(*) FROM exchange_quotes" :: IO [Only Int]
          persisted @?= [Only 3],
      testCase "authorization and non-conversion requests fail before external I/O" $ do
        calls <- newIORef (0 :: Int)
        let provider _ _ = atomicModifyIORef' calls (\n -> (n + 1, Left FX.ProviderUnavailable))
        withTestAppServer Nothing (\env -> env {appFetchReferenceRate = provider}) $ \env port manager _ -> do
          token <- setupUser env "owner"
          outsiderToken <- setupUser env "outsider"
          let tc = mkTC token
          group <- runClient env $ cCreateGroup tc "Authorization"
          let gid@(GroupId uuid) = grpId group
              path = "/groups/" <> show uuid <> "/exchange-quotes"
          runClientExpectStatus env status403 $ cExchangeQuote (mkTC outsiderToken) gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          runClientExpectStatus env status401 $ cExchangeQuote (mkTC "invalid") gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          runClientExpectStatus env status404 $ cExchangeQuote tc (GroupId UUID.nil) (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          forM_ ["USD", "ZZZ"] $ \code -> runClientExpectStatus env status400 $ cExchangeQuote tc gid (Q.ExchangeQuoteRequest $ CurrencyCode code)
          forM_ [object ["currency" .= ("EUR" :: T.Text), "rate" .= ("1.2" :: T.Text)], object [], object ["currency" .= ("eur" :: T.Text)]] $ \body -> do
            response <- identityRequest port manager (Just token) "POST" path body
            HC.responseStatus response @?= status400
          changed <- identityRequest port manager (Just token) "PUT" ("/groups/" <> show uuid) (toJSON $ UpdateGroupRequest "Multi" MultiCurrency (CurrencyCode "USD"))
          HC.responseStatus changed @?= status200
          runClientExpectStatus env status400 $ cExchangeQuote tc gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
          assertEqual "no invalid request reaches provider" 0 =<< readIORef calls,
      testCase "provider failures expose actionable 503 without persistence or policy lock" $
        forM_ [FX.ProviderUnavailable, FX.InvalidProviderResponse] $ \failure ->
          withTestAppServer Nothing (\env -> env {appFetchReferenceRate = \_ _ -> pure (Left failure)}) $ \env _ _ conn -> do
            token <- setupUser env "owner"
            let tc = mkTC token
            group <- runClient env $ cCreateGroup tc "Unavailable"
            response <- runClientM (cExchangeQuote tc (grpId group) (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")) env
            case response of
              Left (FailureResponse _ result) -> do
                responseStatusCode result @?= status503
                responseBody result @?= "Automatic rate unavailable; retry or enter a manual rate"
              _ -> assertFailure "expected provider failure"
            persisted <- query_ conn "SELECT count(*) FROM exchange_quotes" :: IO [Only Int]
            persisted @?= [Only 0]
            current <- runClient env $ cGetGroupById tc (grpId group)
            assertEqual "failed lookup does not lock" False current.currencyLocked,
      quoteRaceTest False False,
      quoteRaceTest True False,
      quoteRaceTest False True
    ]

-- | Pause external I/O while membership or base currency changes, then require revalidation.
-- Flags select the admin route and member-removal race without relying on timing sleeps.
quoteRaceTest :: Bool -> Bool -> TestTree
quoteRaceTest asAdmin removeMember = testCase name $ do
  started <- newEmptyMVar
  release <- newEmptyMVar
  rate <- either (assertFailure . T.unpack) pure (parseDecimalRate "1.2")
  let provider _ _ = do
        putMVar started ()
        takeMVar release
        pure $ Right $ FX.ReferenceRate rate (read "2020-01-02")
  withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") (\env -> env {appFetchReferenceRate = provider}) $ \env port manager conn -> do
    ownerToken <- setupUser env "owner"
    memberToken <- setupUser env "member"
    memberId <- fixtureUserId env "member"
    adminToken <- setupAdmin env "admin" "adminpass123"
    let owner = mkTC ownerToken
        member = mkTC memberToken
        admin = mkAdminTC adminToken
    group <- runClient env $ cCreateGroup owner "Quote race"
    let gid@(GroupId uuid) = grpId group
    _ <- runClient env $ cAddMember owner gid memberId
    done <- newEmptyMVar
    let request = (if asAdmin then cAdminExchangeQuote admin else cExchangeQuote member) gid (Q.ExchangeQuoteRequest $ CurrencyCode "EUR")
    _ <- forkIO $ runClientM request env >>= putMVar done
    entered <- timeout 5000000 (takeMVar started)
    assertEqual "provider reached without DB lock" (Just ()) entered
    ( if removeMember
        then void $ runClient env $ cDeleteMember owner gid memberId
        else do
          changed <- identityRequest port manager (Just ownerToken) "PUT" ("/groups/" <> show uuid) (toJSON $ UpdateGroupRequest "Changed while fetching" SingleCurrency (CurrencyCode "GBP"))
          HC.responseStatus changed @?= status200
      )
      `finally` putMVar release ()
    result <- timeout 5000000 (takeMVar done)
    case result of
      Just (Left (FailureResponse _ response)) -> responseStatusCode response @?= (if removeMember then status403 else status409)
      _ -> assertFailure "expected race rejection after provider response"
    persisted <- query_ conn "SELECT count(*) FROM exchange_quotes" :: IO [Only Int]
    assertEqual "stale response never persists" [Only 0] persisted
  where
    name = (if asAdmin then "admin" else "user") <> (if removeMember then " removal during lookup rejects response" else " policy change during lookup rejects stale pair")
