module Jizhang.Database.Init
  ( createTables,
    dropTables,
  ) where

import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (Connection, execute_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Paths_jizhang (getDataFileName)

createTables :: Connection -> IO ()
createTables = executeSqlFile "sql/schema.sql"

dropTables :: Connection -> IO ()
dropTables = executeSqlFile "sql/drop-schema.sql"

executeSqlFile :: FilePath -> Connection -> IO ()
executeSqlFile relPath conn = do
  sqlPath <- getDataFileName relPath
  sql <- readFile sqlPath
  executeStatement conn sql

executeStatement :: Connection -> String -> IO ()
executeStatement conn statement = do
  _ <- execute_ conn (Query (BS8.pack statement))
  pure ()
