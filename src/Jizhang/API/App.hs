{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Handler runtime and database boundaries. Ordinary ledger actions may log SQL;
-- authentication actions use unlogged persistence and serializable snapshot checks.
module Jizhang.API.App
  ( AppEnv (..),
    MyHandler (..),
    MyServer,
    runDB,
    runDBTransaction,
    runDBSensitive,
    runDBSensitiveTransaction,
  )
where

import Control.Exception (throwIO, try)
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson.Types (emptyObject)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Beam.Postgres (Pg, runBeamPostgres, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (Connection, SqlError (sqlState), withTransaction)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (Serializable), ReadWriteMode (ReadWrite), TransactionMode (..), withTransactionMode)
import Jizhang.API.Types.Admin (AdminBootstrap)
import Jizhang.API.Types.Money (CurrencyCode)
import Jizhang.Config (EnvironmentAuthentication)
import Jizhang.Currency (CurrencyCatalog)
import Jizhang.Database.Auth (AuthTransactionConflict (..))
import Jizhang.Database.Connection (ConnectionManager, DatabaseUnavailable (..), withConnection)
import Jizhang.ExchangeRate (ExchangeRateError, ReferenceRate)
import Jizhang.OIDC (AuthRuntime)
import Jizhang.RequestProtection (RequestProtection)
import Log.Class (MonadLog (getLoggerEnv), logAttention_)
import Log.Data (LogLevel (..))
import Log.Monad (LogT, LoggerEnv (leMaxLogLevel), getLoggerIO)
import Servant
import Servant.Auth.Server (JWTSettings)

-- | Application environment available to all handlers
data AppEnv = AppEnv
  { appConn :: !ConnectionManager,
    appJWTSettings :: !JWTSettings,
    appAuthRuntime :: !AuthRuntime,
    appRequestProtection :: !RequestProtection,
    appAdminBootstrap :: !(Maybe AdminBootstrap),
    appEnvironmentAuthentication :: !(Maybe EnvironmentAuthentication),
    appAdminCookieSecure :: !Bool,
    appClientOrigins :: ![T.Text],
    appCurrencies :: !CurrencyCatalog,
    appFetchReferenceRate :: !(CurrencyCode -> CurrencyCode -> IO (Either ExchangeRateError ReferenceRate))
  }

-- | Interpret a Servant route tree in the shared environment and logger.
type MyServer k = ServerT k MyHandler

-- | HTTP failures remain in @Handler@; environment access does not acquire a connection.
newtype MyHandler a = MyHandler
  { runMyHandler :: ReaderT AppEnv (LogT Handler) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError, MonadLog, MonadBase IO, MonadFail)

instance MonadBaseControl IO MyHandler where
  type StM MyHandler a = Either ServerError a
  liftBaseWith f = MyHandler $ liftBaseWith $ \runInBase -> f (runInBase . runMyHandler)
  restoreM = MyHandler . restoreM

-- | Serialize each complete Beam action on the application's connection.
runDB :: Pg a -> MyHandler a
runDB = runDBWith (\_ -> id)

-- | Run a complete Beam action atomically while holding the connection lock.
runDBTransaction :: Pg a -> MyHandler a
runDBTransaction = runDBWith withTransaction

-- | Secret-bearing persistence must never use SQL parameter logging.
runDBSensitive :: Pg a -> MyHandler a
runDBSensitive = runDBSensitiveWith (\_ -> id)

-- | Commit secret-bearing changes serializably; conflicts become 409 without retry.
runDBSensitiveTransaction :: Pg a -> MyHandler a
runDBSensitiveTransaction = runDBSensitiveWith (withTransactionMode (TransactionMode Serializable ReadWrite))

runDBSensitiveWith :: (Connection -> IO a -> IO a) -> Pg a -> MyHandler a
runDBSensitiveWith scope action = do
  connections <- asks appConn
  -- Catch outside scope so a conflict rolls back before it is translated into an HTTP response.
  result <- databaseAction $ try $ try $ withConnection connections $ \conn ->
    scope conn $ runBeamPostgres conn action
  case result of
    Left AuthTransactionConflict -> conflict
    Right (Right value) -> pure value
    Right (Left sqlError)
      -- Retrying only the SQL would reuse proofs and decisions made against an older auth snapshot.
      | sqlState sqlError `elem` ["40001", "40P01"] -> conflict
      | Just response <- constraintError sqlError -> throwError response
      | otherwise -> do
          logAttention_ "Authentication persistence failed"
          throwError $ err500 {errBody = "Authentication persistence failed"}
  where
    conflict = throwError $ err409 {errBody = "Account changed; retry the operation"}

runDBWith :: (Connection -> IO a -> IO a) -> Pg a -> MyHandler a
runDBWith scope action = do
  connections <- asks appConn
  env <- getLoggerEnv
  runBeam <- if leMaxLogLevel env < LogTrace
    then pure runBeamPostgres
    else do
      logger <- getLoggerIO
      pure $ \conn query -> runBeamPostgresDebug
        (\sql -> getCurrentTime >>= \now -> logger now LogTrace (T.pack sql) emptyObject) conn query
  result <- databaseAction $ try $ withConnection connections $ \conn ->
    scope conn $ runBeam conn action
  case result of
    Right value -> pure value
    Left sqlError -> maybe (liftIO $ throwIO sqlError) throwError (constraintError sqlError)

-- | Connection failure becomes a non-cacheable 503; writes are never replayed here.
databaseAction :: IO a -> MyHandler a
databaseAction action = do
  result <- liftIO $ try action
  case result of
    Right value -> pure value
    Left DatabaseUnavailable -> do
      logAttention_ "Database unavailable"
      throwError $ err503
        { errBody = "Database unavailable; operation was not retried",
          errHeaders = [("Cache-Control", "no-store")]
        }

constraintError :: SqlError -> Maybe ServerError
constraintError sqlError = case sqlState sqlError of
  "23505" -> Just $ err409 {errBody = "An item with this key already exists"}
  "23503" -> Just $ err409 {errBody = "This change conflicts with existing group or expense history"}
  _ -> Nothing
