{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Jizhang.API
import Log
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = withStdOutLogger $ \logger -> do
  connStr <- maybe "host=localhost dbname=jizhang" pack <$> liftIO (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL connStr
  createTables conn
  app' <- runLogT "jizhang" logger LogInfo $ app conn
  run 8080 app'
  close conn

createTables :: Connection -> IO ()
createTables conn = do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, username TEXT NOT NULL UNIQUE, password_hash TEXT NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS groups (id TEXT PRIMARY KEY, name TEXT NOT NULL, owner__id TEXT NOT NULL REFERENCES users (id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS group_members (user__id TEXT REFERENCES users (id) ON DELETE CASCADE, group__id TEXT REFERENCES groups (id) ON DELETE CASCADE, PRIMARY KEY (user__id, group__id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS records (id TEXT PRIMARY KEY, group__id TEXT NOT NULL REFERENCES groups (id) ON DELETE CASCADE, title TEXT NOT NULL, amount DOUBLE PRECISION NOT NULL, by__id TEXT NOT NULL REFERENCES users (id), to__id TEXT REFERENCES users (id), date DATE NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS record_splits (record__id TEXT REFERENCES records (id) ON DELETE CASCADE, user__id TEXT REFERENCES users (id), share SMALLINT NOT NULL CHECK (share >= 0), PRIMARY KEY (record__id, user__id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS sessions (token TEXT PRIMARY KEY, user__id TEXT NOT NULL REFERENCES users (id) ON DELETE CASCADE, expires_at TIMESTAMPTZ NOT NULL, token_type TEXT NOT NULL);"
  pure ()
