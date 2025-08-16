{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Jizhang.API
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withStdOutLogger $ \logger -> do
  conn <- liftIO $ open "jizhang.db"
  execute_ conn "PRAGMA foreign_keys = ON;"
  createTables conn
  run 8080 $ app conn logger
  close conn

createTables :: Connection -> IO ()
createTables conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS users (username TEXT PRIMARY KEY);"
  execute_ conn "CREATE TABLE IF NOT EXISTS groups (id TEXT PRIMARY KEY, name TEXT NOT NULL);"
  execute_ conn "CREATE TABLE IF NOT EXISTS group_members (user__username TEXT, group__id TEXT, PRIMARY KEY (user__username, group__id), FOREIGN KEY (user__username) REFERENCES users (username) ON DELETE CASCADE, FOREIGN KEY (group__id) REFERENCES groups (id) ON DELETE CASCADE);"
  execute_ conn "CREATE TABLE IF NOT EXISTS records (id TEXT PRIMARY KEY, group__id TEXT NOT NULL, title TEXT NOT NULL, amount REAL NOT NULL, by__username TEXT NOT NULL, to__username TEXT, at TEXT NOT NULL, FOREIGN KEY (group__id) REFERENCES groups (id) ON DELETE CASCADE, FOREIGN KEY (by__username) REFERENCES users (username), FOREIGN KEY (to__username) REFERENCES users (username));"
  execute_ conn "CREATE TABLE IF NOT EXISTS record_splits (record__id TEXT, user__username TEXT, percentage INT NOT NULL CHECK (percentage >= 0 AND percentage <= 100), amount REAL NOT NULL, PRIMARY KEY (record__id, user__username), FOREIGN KEY (record__id) REFERENCES records (id) ON DELETE CASCADE, FOREIGN KEY (user__username) REFERENCES users (username));"
