{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API where

import Control.Monad.Reader (ReaderT (..))
import Data.Swagger (Swagger)
import Database.SQLite.Simple (Connection)
import Jizhang.API.Group
import Jizhang.API.Record
import Jizhang.API.Report
import Jizhang.API.Types
import Jizhang.API.User
import Log (LogT, LoggerEnv (leLogger), MonadLog (getLoggerEnv), logExceptions, runLogT)
import Network.Wai.Log (mkLogMiddleware)
import Servant
import Servant.Swagger
import Jizhang.API.Import

jizhangServer :: MyServer JizhangAPI
jizhangServer =
  userServer
    :<|> groupServer
    :<|> recordServer
    :<|> reportServer
    :<|> importServer

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI

type API = JizhangAPI :<|> SwaggerAPI

swaggerServer :: MyServer SwaggerAPI
swaggerServer = pure $ toSwagger (Proxy :: Proxy JizhangAPI)

api :: Proxy API
api = Proxy

server :: MyServer API
server = jizhangServer :<|> swaggerServer

app :: Connection -> LogT IO Application
app conn = do
  env <- getLoggerEnv
  let s =
        serve api $
          hoistServer
            api
            ( runLogT
                "jizhang"
                (leLogger env)
                maxBound
                . logExceptions
                . flip runReaderT conn
                . runMyHandler
            )
            server
  middleware <- mkLogMiddleware
  pure $ middleware $ const s
