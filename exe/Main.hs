{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Jizhang.API
import Jizhang.API.Types (AppEnv (..))
import Log
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import System.Environment (lookupEnv)

main :: IO ()
main = withStdOutLogger $ \logger -> do
  connStr <- maybe "host=localhost dbname=jizhang" pack <$> liftIO (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL connStr
  createTables conn
  jwk <- generateKey
  let jwtCfg = defaultJWTSettings jwk
      appEnv = AppEnv conn jwtCfg
  app' <- runLogT "jizhang" logger LogInfo $ app appEnv
  run 8080 app'
  close conn

createTables :: Connection -> IO ()
createTables conn = do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS users (id UUID PRIMARY KEY, username TEXT NOT NULL UNIQUE, password_hash TEXT NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS groups (id UUID PRIMARY KEY, name TEXT NOT NULL, owner__id UUID NOT NULL REFERENCES users (id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS group_members (user__id UUID REFERENCES users (id) ON DELETE CASCADE, group__id UUID REFERENCES groups (id) ON DELETE CASCADE, active BOOLEAN NOT NULL DEFAULT TRUE, PRIMARY KEY (user__id, group__id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS receipts (id UUID PRIMARY KEY, group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE, uploaded_by__id UUID NOT NULL REFERENCES users (id), note TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS records (id UUID PRIMARY KEY, group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE, title TEXT NOT NULL, amount DOUBLE PRECISION NOT NULL, by__id UUID NOT NULL REFERENCES users (id), to__id UUID REFERENCES users (id), date DATE NOT NULL, created_at TIMESTAMPTZ NOT NULL, receipt__id UUID REFERENCES receipts (id) ON DELETE SET NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS record_splits (record__id UUID REFERENCES records (id) ON DELETE CASCADE, user__id UUID REFERENCES users (id), share SMALLINT NOT NULL CHECK (share >= 0), PRIMARY KEY (record__id, user__id));"
  pure ()
