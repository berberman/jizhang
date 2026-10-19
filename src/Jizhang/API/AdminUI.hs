{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Serve the packaged operator web client and brand asset. These routes deliver
-- static bytes; authorization of operator actions belongs to the JSON API.
module Jizhang.API.AdminUI
  ( AdminUIAPI,
    adminUIServer,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Version (showVersion)
import Jizhang.API.App (MyHandler)
import Network.HTTP.Media ((//))
import Paths_jizhang (getDataDir, getDataFileName, version)
import Servant
import System.FilePath ((</>))

data HTML

data CSS

data JavaScript

data SVG

instance Accept HTML where
  contentType _ = "text" // "html"

instance Accept CSS where
  contentType _ = "text" // "css"

instance Accept JavaScript where
  contentType _ = "application" // "javascript"

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender HTML LBS.ByteString where
  mimeRender _ = id

instance MimeRender CSS LBS.ByteString where
  mimeRender _ = id

instance MimeRender JavaScript LBS.ByteString where
  mimeRender _ = id

instance MimeRender SVG LBS.ByteString where
  mimeRender _ = id

-- | Expose only the listed assets, not an arbitrary request-selected filesystem path.
type AdminUIAPI =
  "admin-ui" :> Get '[HTML] LBS.ByteString
    :<|> "admin-ui" :> "styles.css" :> Get '[CSS] LBS.ByteString
    :<|> "admin-ui" :> "app.js" :> Get '[JavaScript] LBS.ByteString
    :<|> "admin-ui" :> "money.js" :> Get '[JavaScript] LBS.ByteString
    :<|> "admin-ui" :> "statistics.js" :> Get '[JavaScript] LBS.ByteString
    :<|> "admin-ui" :> "jizhang-mark.svg" :> Get '[SVG] LBS.ByteString

adminUIServer :: ServerT AdminUIAPI MyHandler
adminUIServer =
  serveIndex
    :<|> serveUIFile "styles.css"
    :<|> serveUIFile "app.js"
    :<|> serveUIFile "money.js"
    :<|> serveUIFile "statistics.js"
    :<|> serveMark

adminVersion :: LT.Text
adminVersion = LT.pack (showVersion version)

-- | Substitute the package version into the shipped UTF-8 HTML template.
serveIndex :: MyHandler LBS.ByteString
serveIndex = do
  html <- serveUIFile "index.html"
  pure $ LTE.encodeUtf8 $ LT.replace "__JIZHANG_VERSION__" adminVersion (LTE.decodeUtf8 html)

-- | Read a server-selected filename beneath the installed ui data directory.
serveUIFile :: FilePath -> MyHandler LBS.ByteString
serveUIFile name = do
  dataDir <- liftIO getDataDir
  liftIO $ LBS.readFile (dataDir </> "ui" </> name)

serveMark :: MyHandler LBS.ByteString
serveMark = do
  path <- liftIO $ getDataFileName "client/web/jizhang-mark.svg"
  liftIO $ LBS.readFile path
