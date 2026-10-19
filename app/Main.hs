{-# LANGUAGE OverloadedStrings #-}

-- | Backend process setup: packaged data, environment policy, owned database connection and Warp.
module Main where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Text as T
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (Serializable), ReadWriteMode (ReadWrite), TransactionMode (..), withTransactionMode)
import GHC.Clock (getMonotonicTimeNSec)
import Jizhang.API
import Jizhang.API.App (AppEnv (..))
import Jizhang.Config (parseAdminBootstrap, parseEnvironmentAuthentication, parseJWTSettings, parseRequestProtectionConfig)
import Jizhang.Currency (loadCurrencyCatalog)
import qualified Jizhang.Database.Auth as AuthDB
import Jizhang.Database.Connection (closeConnectionManager, newConnectionManager, withConnection)
import Jizhang.Database.Init (createTables)
import Jizhang.ExchangeRate (fetchReferenceRate)
import Jizhang.OIDC (AuthRuntime (authClock), newAuthRuntime)
import Jizhang.RequestProtection (newRequestProtection)
import Log
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Client (ManagerSettings (managerRetryableException), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setHost, setInstallShutdownHandler, setOnException, setPort, setTimeout)
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import System.Environment (lookupEnv)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Fail startup on invalid configuration or schema setup; close the connection on shutdown.
-- Signal handlers stop Warp gracefully before the connection manager leaves its bracket.
main :: IO ()
main =
  newManager tlsManagerSettings {managerRetryableException = const False} >>= \manager -> withStdOutLogger $ \logger -> do
    currencies <- loadCurrencyCatalog
    mCookieSecure <- lookupEnv "ADMIN_COOKIE_SECURE"
    cookieSecure <- case mCookieSecure of
      Nothing -> pure True
      Just "true" -> pure True
      Just "false" -> pure False
      Just _ -> fail "ADMIN_COOKIE_SECURE must be true or false"
    clientOrigins <- maybe [] (filter (not . T.null) . map T.strip . T.splitOn "," . T.pack) <$> lookupEnv "CLIENT_ORIGINS"
    when ("*" `elem` clientOrigins) $
      fail "CLIENT_ORIGINS must contain explicit origins"
    publicApiUrl <- fmap T.pack <$> lookupEnv "PUBLIC_API_URL"
    frontendUrl <- fmap T.pack <$> lookupEnv "FRONTEND_URL"
    oidcSecretKey <- fmap T.pack <$> lookupEnv "OIDC_SECRET_KEY"
    authRuntime <- newAuthRuntime publicApiUrl frontendUrl oidcSecretKey clientOrigins
    -- A generated signing key lasts only this process; a configured key preserves signatures.
    signingKey <- fmap T.pack <$> lookupEnv "JWT_SECRET_KEY"
    jwtCfg <- maybe (defaultJWTSettings <$> generateKey) (either (fail . T.unpack) pure . parseJWTSettings) signingKey
    connStr <- maybe "host=localhost dbname=jizhang" pack <$> liftIO (lookupEnv "DATABASE_URL")
    -- Direct launches retain Warp's all-interface default; the local launcher sets loopback.
    bindHost <- maybe "*" id <$> lookupEnv "BIND_HOST"
    mAdminUsername <- liftIO $ lookupEnv "ADMIN_USERNAME"
    mAdminPassword <- liftIO $ lookupEnv "ADMIN_PASSWORD"
    adminBootstrap <- either (fail . T.unpack) pure $ parseAdminBootstrap (T.pack <$> mAdminUsername) (T.pack <$> mAdminPassword)
    authenticationValues <-
      fmap (M.fromList . catMaybes) $
        mapM
          (\key -> fmap ((,) key . T.pack) <$> lookupEnv (T.unpack key))
          [ "AUTH_CONFIG_SOURCE",
            "OIDC_ENABLED",
            "LOCAL_REGISTRATION_ENABLED",
            "SSO_REGISTRATION_ENABLED",
            "OIDC_PROVIDER_NAME",
            "OIDC_ISSUER",
            "OIDC_CLIENT_ID",
            "OIDC_CLIENT_SECRET"
          ]
    environmentAuthentication <- either (fail . T.unpack) pure $ parseEnvironmentAuthentication authenticationValues
    protectionValues <-
      fmap (M.fromList . catMaybes) $
        mapM
          (\key -> fmap ((,) key . T.pack) <$> lookupEnv (T.unpack key))
          [ "RATE_LIMIT_IP_PER_MINUTE",
            "RATE_LIMIT_AUTH_PER_IP_PER_MINUTE",
            "RATE_LIMIT_ACCOUNT_PER_MINUTE",
            "RATE_LIMIT_EXPENSIVE_PER_ACCOUNT_PER_MINUTE",
            "RATE_LIMIT_CREDENTIAL_PER_MINUTE",
            "REQUEST_AUTH_BODY_MAX_BYTES",
            "REQUEST_BODY_MAX_BYTES",
            "REQUEST_BULK_BODY_MAX_BYTES",
            "REQUEST_MAX_IN_FLIGHT",
            "REQUEST_MAX_EXPENSIVE_IN_FLIGHT",
            "RATE_LIMIT_MAX_KEYS",
            "TRUSTED_PROXY_CIDRS"
          ]
    protectionCfg <- either (fail . T.unpack) pure $ parseRequestProtectionConfig protectionValues
    protection <- newRequestProtection protectionCfg getMonotonicTimeNSec
    bracket (newConnectionManager connStr) closeConnectionManager $ \connections -> do
      withConnection connections $ \conn -> do
        createTables conn
        authNow <- authClock authRuntime
        -- Startup cleanup is transactional and is not repeated during connection recovery.
        withTransactionMode (TransactionMode Serializable ReadWrite) conn $
          runBeamPostgres conn (AuthDB.cleanupExpiredAuth authNow)
      let appEnv =
            AppEnv
              { appConn = connections,
                appJWTSettings = jwtCfg,
                appAuthRuntime = authRuntime,
                appRequestProtection = protection,
                appAdminBootstrap = adminBootstrap,
                appEnvironmentAuthentication = environmentAuthentication,
                appAdminCookieSecure = cookieSecure,
                appClientOrigins = clientOrigins,
                appCurrencies = currencies,
                appFetchReferenceRate = fetchReferenceRate manager
              }
          installShutdown stop = do
            _ <- installHandler sigTERM (Catch stop) Nothing
            _ <- installHandler sigINT (Catch stop) Nothing
            pure ()
          settings =
            setInstallShutdownHandler installShutdown $
              setTimeout 30 $
                setGracefulShutdownTimeout (Just 25) $
                  setOnException (\_ _ -> runLogT "warp" logger maxBound $ logAttention_ "HTTP request failed") $
                    setHost (fromString bindHost) $
                      setPort 8080 defaultSettings
      app' <- runLogT "jizhang" logger LogInfo $ app appEnv
      runSettings settings app'
