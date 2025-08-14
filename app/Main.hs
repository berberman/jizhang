{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Control.Monad.IO.Class
import DB
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Network.Wai.Handler.Warp (run)
import Schema

main :: IO ()
main = do
  createTables
  conn <- open "jizhang.db"
  execute_ conn "PRAGMA foreign_keys = ON;"
  run 8080 $ app conn
  -- runBeamSqlite conn $ do
  --   insertUser "admin"
  --   insertUser "user1"
  --   insertUser "user2"
  --   group1 <- insertGroup "group1"
  --   group2 <- insertGroup "group2"
  --   getAllUsers >>= mapM_ (liftIO . putStrLn . T.unpack . _username)
  --   addGroupMember "admin" $ _groupId group1
  --   addGroupMember "user1" $ _groupId group1
  --   addGroupMember "user1" $ _groupId group2
  --   addGroupMember "user2" $ _groupId group2
  --   deleteUser "user2"
  --   getAllGroups >>= mapM_ (liftIO . print)
  --   getAllGroupWithMembers >>= mapM_ (liftIO . print)
  --   record1 <- insertRecord "Expense 1" 100.0 "admin" Nothing (_groupId group1) =<< liftIO getCurrentTime
  --   insertRecordSplit (_recordId record1) "admin" 50
  --   insertRecordSplit (_recordId record1) "user1" 50
  --   record2 <- insertRecord "Expense 2" 200.0 "user1" Nothing (_groupId group1) =<< liftIO getCurrentTime
  --   insertRecordSplit (_recordId record2) "admin" 100
  --   insertRecordSplit (_recordId record2) "user1" 0
  --   getAllRecordsInGroup (_groupId group1) >>= mapM_ (liftIO . print)
  --   getRecordsWithSplitsForGroup (_groupId group1) >>= mapM_ (liftIO . print)
  --   getRecordSplitsForRecord (_recordId record1) >>= mapM_ (liftIO . print)
  --   getGroupsForUser "user1" >>= mapM_ (liftIO . print)

  close conn

createTables :: IO ()
createTables = do
  conn <- open "jizhang.db"
  execute_ conn "PRAGMA foreign_keys = ON;"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (username TEXT PRIMARY KEY);"
  execute_ conn "CREATE TABLE IF NOT EXISTS groups (id TEXT PRIMARY KEY, name TEXT NOT NULL);"
  execute_ conn "CREATE TABLE IF NOT EXISTS group_members (user__username TEXT, group__id TEXT, PRIMARY KEY (user__username, group__id), FOREIGN KEY (user__username) REFERENCES users (username) ON DELETE CASCADE, FOREIGN KEY (group__id) REFERENCES groups (id) ON DELETE CASCADE);"
  execute_ conn "CREATE TABLE IF NOT EXISTS records (id TEXT PRIMARY KEY, group__id TEXT NOT NULL, title TEXT NOT NULL, amount REAL NOT NULL, by__username TEXT NOT NULL, to__username TEXT, at TEXT NOT NULL, FOREIGN KEY (group__id) REFERENCES groups (id) ON DELETE CASCADE, FOREIGN KEY (by__username) REFERENCES users (username), FOREIGN KEY (to__username) REFERENCES users (username));"
  execute_ conn "CREATE TABLE IF NOT EXISTS record_splits (record__id TEXT, user__username TEXT, percentage INT NOT NULL CHECK (percentage >= 0 AND percentage <= 100), amount REAL, PRIMARY KEY (record__id, user__username), FOREIGN KEY (record__id) REFERENCES records (id) ON DELETE CASCADE, FOREIGN KEY (user__username) REFERENCES users (username));"
  close conn
