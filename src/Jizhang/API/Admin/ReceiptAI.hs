{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Admin.ReceiptAI
  ( AdminReceiptAIAPI,
    adminReceiptAIServer,
  )
where

import Jizhang.API.Admin.ReceiptAI.Settings
import Jizhang.API.Admin.ReceiptAI.Usage
import Jizhang.API.App (MyServer)
import Jizhang.API.Types.Admin (AuthAdmin)
import Servant ((:<|>) (..))

type AdminReceiptAIAPI = ReceiptAISettingsAPI :<|> ReceiptAIUsageAPI

adminReceiptAIServer :: AuthAdmin -> MyServer AdminReceiptAIAPI
adminReceiptAIServer admin = receiptAISettingsServer admin :<|> receiptAIUsageServer admin
