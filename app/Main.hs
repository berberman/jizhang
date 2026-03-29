{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Jizhang.API
import Jizhang.API.Types (AppEnv (..))
import Jizhang.Database.Init (createTables)
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
