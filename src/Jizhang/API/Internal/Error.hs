{-# LANGUAGE OverloadedStrings #-}

-- | Distinguish invalid requests, missing resources and corrupt persisted finance data.
-- Stored-data details go to the application log rather than the HTTP response.
module Jizhang.API.Internal.Error
  ( textToLBS,
    fromInput,
    fromStoredData,
    fetchOrFail
  )
where

import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.UUID (UUID, toText)
import Jizhang.API.App (MyHandler)
import Log.Class (logAttention_)
import Servant (ServerError (errBody), err400, err404, err500, throwError)

-- | Encode error bodies as UTF-8 without changing the supplied message.
textToLBS :: Text -> LazyByteString
textToLBS = LBS.fromStrict . T.encodeUtf8

-- | Caller mistakes and corrupt stored data deliberately have different status paths.
fromInput :: Either Text a -> MyHandler a
fromInput = either (\message -> throwError $ err400 {errBody = textToLBS message}) pure

-- | Reject corrupt persisted values with a generic 500 and log the diagnostic.
fromStoredData :: Either Text a -> MyHandler a
fromStoredData = either invalid pure
  where
    invalid :: Text -> MyHandler a
    invalid message = do
      logAttention_ $ "Stored financial data is invalid: " <> message
      throwError $ err500 {errBody = "Stored financial data is invalid"}

-- | Turn an absent lookup into a labelled 404; authorization stays with the caller.
fetchOrFail :: Text -> UUID -> MyHandler (Maybe a) -> MyHandler a
fetchOrFail label uuid action = do
  result <- action
  case result of
    Just x -> pure x
    Nothing -> throwError $ err404 {errBody = textToLBS (label <> " with ID " <> toText uuid <> " not found")}
