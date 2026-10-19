-- | Test executable and subprocess OIDC-provider entrypoint.
-- Ordinary arguments belong to Tasty; provider mode bypasses the suite entirely.
module Main where

import APITest (apiTests)
import AdminAuthenticationTest (adminAuthenticationTests)
import AuthAccountTest (authAccountTests)
import AuthLogTest (authLogTests)
import AuthSessionTest (authSessionTests)
import ConfigTest (configTests)
import CurrencyTest (currencyTests)
import DatabaseRecoveryTest (databaseRecoveryTests)
import EnvironmentAuthenticationTest (environmentAuthenticationTests)
import ExchangeQuoteTest (exchangeQuoteTests)
import ExchangeRateTest (exchangeRateTests)
import LogoutTest (logoutTests)
import MoneyTest (moneyTests)
import OIDCDtoTest (oidcDtoTests)
import OIDCFlowTest (oidcFlowTests)
import OIDCProvider (runOidcProvider)
import OIDCTest (oidcTests)
import RecordQueryTest (recordQueryTests)
import ReportTest (reportTests)
import RequestProtectionTest (requestProtectionTests)
import StatisticsTest (statisticsTests)
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Text.Read (readMaybe)

-- | Run the provider only for an exact flag/valid-port pair; malformed provider mode fails.
-- Other invocations retain all arguments for Tasty's filtering and runner options.
main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--oidc-provider", portText] | Just port <- readMaybe portText, port > 0 && port <= 65535 -> runOidcProvider port
    "--oidc-provider" : _ -> fail "Usage: --oidc-provider PORT (1-65535)"
    _ -> withArgs arguments $ defaultMain $ testGroup "Jizhang" [requestProtectionTests, configTests, environmentAuthenticationTests, moneyTests, currencyTests, exchangeRateTests, recordQueryTests, reportTests, statisticsTests, apiTests, databaseRecoveryTests, exchangeQuoteTests, oidcTests, oidcDtoTests, authLogTests, authSessionTests, oidcFlowTests, authAccountTests, logoutTests, adminAuthenticationTests]
