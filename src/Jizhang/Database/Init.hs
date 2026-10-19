-- | Execute the packaged schema scripts on a caller-owned connection.
-- This module does not open connections, choose isolation or run migrations.
module Jizhang.Database.Init
  ( createTables,
    dropTables,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (Connection, execute_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Paths_jizhang (getDataFileName)

-- | Run the installed schema SQL verbatim; idempotence and constraints are
-- properties of that script, not checks performed by this wrapper.
createTables :: Connection -> IO ()
createTables = executeSqlFile "sql/schema.sql"

-- | Run the destructive drop script. The caller owns authorization, transaction
-- handling and any recovery or backup required before removing persisted data.
dropTables :: Connection -> IO ()
dropTables = executeSqlFile "sql/drop-schema.sql"

-- | Resolve Cabal's installed data path rather than the working directory.
-- File and SQL errors propagate; no partial execution is retried here.
executeSqlFile :: FilePath -> Connection -> IO ()
executeSqlFile relPath conn = do
  sqlPath <- getDataFileName relPath
  sql <- readFile sqlPath
  executeStatement conn sql

executeStatement :: Connection -> String -> IO ()
executeStatement conn statement = do
  _ <- execute_ conn (Query (BS8.pack statement))
  pure ()
