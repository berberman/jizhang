{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API where

import Control.Monad.Reader (ReaderT (..))
import Database.SQLite.Simple (Connection)
import Jizhang.API.Group
import Jizhang.API.Record
import Jizhang.API.Report
import Jizhang.API.Types
import Jizhang.API.User
import Log (logExceptions, runLogT)
import Log.Logger (Logger)
import Servant

jizhangServer :: MyServer JizhangAPI
jizhangServer =
  userServer
    :<|> groupServer
    :<|> recordServer
    :<|> reportServer

type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI

jizhangAPI :: Proxy JizhangAPI
jizhangAPI = Proxy

app :: Connection -> Logger -> Application
app conn logger =
  serve jizhangAPI $
    hoistServer
      jizhangAPI
      ( runLogT
          "jizhang"
          logger
          maxBound
          . logExceptions
          . flip runReaderT conn
          . runMyHandler
      )
      jizhangServer
