{-# LANGUAGE OverloadedStrings #-}

-- | Exclusive access to one recoverable PostgreSQL connection, not a pool.
-- Checkout serializes callbacks but does not create or replay transactions.
module Jizhang.Database.Connection
  ( ConnectionManager,
    DatabaseUnavailable (..),
    newConnectionManager,
    closeConnectionManager,
    withConnection,
  )
where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception
  ( Exception,
    IOException,
    SomeAsyncException,
    SomeException,
    finally,
    fromException,
    mask,
    mask_,
    onException,
    throwIO,
    try,
  )
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple (Connection, SqlError (..))
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Internal as Internal
import System.IO.Error (ioeGetLocation)
import System.Timeout (timeout)

-- The empty slot belongs to exactly one checkout or shutdown operation.
data ConnectionManager = ConnectionManager !ByteString !(MVar ConnectionState)

data ConnectionState
  = Disconnected
  | Connected !Connection
  | Closed

-- | A closed manager, failed connection attempt or unusable returned connection.
-- This deliberately avoids exposing connection details through its Show instance.
data DatabaseUnavailable = DatabaseUnavailable

instance Show DatabaseUnavailable where
  show DatabaseUnavailable = "DatabaseUnavailable"

instance Exception DatabaseUnavailable

-- | Connect eagerly with a five-second connection/setup deadline. Failure leaves
-- no manager; later recovery applies only after a successful construction.
newConnectionManager :: ByteString -> IO ConnectionManager
newConnectionManager connectionString = mask $ \restore -> do
  connection <- establishConnection restore connectionString
  slot <- newMVar (Connected connection)
    `onException` void (tryClose (PG.close connection))
  pure (ConnectionManager connectionString slot)

-- | Wait for the current checkout, then permanently close the manager.
-- Later checkouts fail rather than reconnecting; repeated shutdown is harmless.
closeConnectionManager :: ConnectionManager -> IO ()
closeConnectionManager (ConnectionManager _ slot) = mask_ $ do
  state <- takeMVar slot
  case state of
    Connected connection -> PG.close connection `finally` putMVar slot Closed
    _ -> putMVar slot Closed

-- | Lend the connection exclusively; do not retain it beyond the callback.
-- Callers must finish commit/rollback before return: only healthy idle handles survive.
-- Actions are never replayed; after a handle is discarded, a later checkout reconnects.
withConnection :: ConnectionManager -> (Connection -> IO a) -> IO a
withConnection (ConnectionManager connectionString slot) action = mask $ \restore -> do
  state <- takeMVar slot
  connection <- case state of
    Closed -> do
      putMVar slot Closed
      throwIO DatabaseUnavailable
    Connected current -> pure current
    Disconnected ->
      establishConnection restore connectionString
        `onException` putMVar slot Disconnected
  let discard = tryClose (PG.close connection) `finally` putMVar slot Disconnected
      failWith exception = do
        _ <- discard
        throwIO (exception :: SomeException)
      failUnavailable = do
        closed <- discard
        throwUnavailable closed
      retain = putMVar slot (Connected connection)
  -- Only the caller's action is restored here; ownership transfers stay masked.
  outcome <- try (restore (action connection))
  case outcome of
    Left exception
      -- Cancellation may interrupt libpq with a query still in flight; never lend that handle again.
      | isAsyncException exception -> failWith exception
      | isConnectionFailure exception -> failUnavailable
      | otherwise -> do
          health <- try (inspectConnection connection) :: IO (Either SomeException (Bool, Bool))
          case health of
            Right (True, True) -> retain >> throwIO exception
            Right (False, _) -> failUnavailable
            _ -> failWith exception
    Right result -> do
      -- A lost acknowledgement may hide a committed mutation; discard the handle, not replay the action.
      health <- try (inspectConnection connection)
      case health of
        Right (True, True) -> retain >> pure result
        Right _ -> failUnavailable
        Left exception
          | isConnectionFailure exception -> failUnavailable
          | otherwise -> failWith exception

-- Keep the raw handle reachable before unmasking polling or session setup.
establishConnection :: (IO () -> IO ()) -> ByteString -> IO Connection
establishConnection restore connectionString = do
  owned <- newIORef Nothing
  outcome <- try $ timeout 5000000 $ do
    raw <- PQ.connectStart connectionString
    writeIORef owned (Just raw)
    connection <- wrapConnection raw
    restore $ do
      pollConnection raw
      initializeConnection raw connection
    pure connection
  let discard = do
        raw <- readIORef owned
        maybe (pure (Right ())) (tryClose . PQ.finish) raw
  -- Timeout and cancellation share the same owner, including failures before wrapping completes.
  case outcome of
    Left exception -> do
      _ <- discard
      if isConnectionFailure exception
        then throwIO DatabaseUnavailable
        else throwIO (exception :: SomeException)
    Right Nothing -> discard >>= throwUnavailable
    Right (Just connection) -> pure connection

wrapConnection :: PQ.Connection -> IO Connection
wrapConnection raw =
  Internal.Connection
    <$> newMVar raw
    <*> newMVar IntMap.empty
    <*> newIORef 0

pollConnection :: PQ.Connection -> IO ()
pollConnection raw = do
  status <- PQ.connectPoll raw
  case status of
    PQ.PollingFailed -> throwIO DatabaseUnavailable
    PQ.PollingOk -> pure ()
    PQ.PollingReading -> waitForSocket threadWaitRead
    PQ.PollingWriting -> waitForSocket threadWaitWrite
  where
    waitForSocket wait = do
      socket <- PQ.socket raw
      case socket of
        Nothing -> throwIO DatabaseUnavailable
        Just descriptor -> wait descriptor >> pollConnection raw

-- Match postgresql-simple's session setup without rerunning application bootstrap.
initializeConnection :: PQ.Connection -> Connection -> IO ()
initializeConnection raw connection = do
  status <- PQ.status raw
  unless (status == PQ.ConnectionOk) (throwIO DatabaseUnavailable)
  version <- PQ.serverVersion raw
  let settings
        | version < 80200 = "SET datestyle TO ISO;SET client_encoding TO UTF8"
        | otherwise = "SET datestyle TO ISO;SET client_encoding TO UTF8;SET standard_conforming_strings TO on"
  void (PG.execute_ connection settings)

-- | Inspect libpq status without issuing a probe query. An open or failed
-- transaction is not safe to hand to the next callback, even on a healthy socket.
inspectConnection :: Connection -> IO (Bool, Bool)
inspectConnection connection = Internal.withConnection connection $ \raw -> do
  status <- PQ.status raw
  transaction <- PQ.transactionStatus raw
  pure (status == PQ.ConnectionOk, transaction == PQ.TransIdle)

-- | Classify connection-level failures separately from ordinary SQL rejections,
-- which may leave an idle connection reusable after caller-managed rollback.
isConnectionFailure :: SomeException -> Bool
isConnectionFailure exception
  | Just DatabaseUnavailable <- fromException exception = True
  | Just sqlError <- fromException exception =
      BS.take 2 (sqlState sqlError) == "08"
        || sqlState sqlError `elem` ["57P01", "57P02", "57P03"]
        || (BS.null (sqlState sqlError) && sqlExecStatus sqlError == PQ.FatalError)
  | Just ioFailure <- fromException exception :: Maybe IOException =
      ioeGetLocation ioFailure == "libpq"
        || "Database.PostgreSQL.Simple." `isPrefixOf` ioeGetLocation ioFailure
  | otherwise = False

isAsyncException :: SomeException -> Bool
isAsyncException exception = case fromException exception :: Maybe SomeAsyncException of
  Just _ -> True
  Nothing -> False

tryClose :: IO () -> IO (Either SomeException ())
tryClose = try

throwUnavailable :: Either SomeException () -> IO a
throwUnavailable (Left exception)
  | isAsyncException exception = throwIO exception
throwUnavailable _ = throwIO DatabaseUnavailable
