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
import Log (logExceptions, runLogT)
import Log.Logger (Logger)
import Servant
import Servant.Swagger

jizhangServer :: MyServer JizhangAPI
jizhangServer =
  userServer
    :<|> groupServer
    :<|> recordServer
    :<|> reportServer

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI

type API = JizhangAPI :<|> SwaggerAPI

swaggerServer :: MyServer SwaggerAPI
swaggerServer = pure $ toSwagger (Proxy :: Proxy JizhangAPI)

api :: Proxy API
api = Proxy

server :: MyServer API
server = jizhangServer :<|> swaggerServer

app :: Connection -> Logger -> Application
app conn logger =
  serve api $
    hoistServer
      api
      ( runLogT
          "jizhang"
          logger
          maxBound
          . logExceptions
          . flip runReaderT conn
          . runMyHandler
      )
      server
