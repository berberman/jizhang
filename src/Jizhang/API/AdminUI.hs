{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.AdminUI
  ( AdminUIAPI,
    adminUIServer,
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Jizhang.API.Types (MyHandler)
import Network.HTTP.Media ((//))
import Paths_jizhang (getDataDir)
import Servant
import System.FilePath ((</>))

data HTML

data CSS

data JavaScript

instance Accept HTML where
  contentType _ = "text" // "html"

instance Accept CSS where
  contentType _ = "text" // "css"

instance Accept JavaScript where
  contentType _ = "application" // "javascript"

instance MimeRender HTML LBS.ByteString where
  mimeRender _ = id

instance MimeRender CSS LBS.ByteString where
  mimeRender _ = id

instance MimeRender JavaScript LBS.ByteString where
  mimeRender _ = id

type AdminUIAPI =
  "admin-ui" :> Get '[HTML] LBS.ByteString
    :<|> "admin-ui" :> "styles.css" :> Get '[CSS] LBS.ByteString
    :<|> "admin-ui" :> "app.js" :> Get '[JavaScript] LBS.ByteString

adminUIServer :: ServerT AdminUIAPI MyHandler
adminUIServer =
  serveUIFile "index.html"
    :<|> serveUIFile "styles.css"
    :<|> serveUIFile "app.js"

serveUIFile :: FilePath -> MyHandler LBS.ByteString
serveUIFile name = do
  dataDir <- liftIO getDataDir
  liftIO $ LBS.readFile (dataDir </> "ui" </> name)
