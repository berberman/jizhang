{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Jizhang.API.Import where

import Control.Monad (forM)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Coerce (coerce)
import Data.Csv hiding (Record)
import Data.Data (Typeable)
import Data.Map.Strict (fromListWith)
import Data.Swagger (NamedSchema (..), ToSchema (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.IsList (toList)
import Jizhang.API.Record
import Jizhang.API.Types
import Jizhang.API.Utils
import Network.HTTP.Media ((//))
import Servant

type ImportAPI = "groups" :> Capture "groupId" GroupId :> "import" :> ReqBody '[CSV] CSVData :> Post '[JSON] [Record]

data CSV deriving (Typeable)

newtype CSVData = CSVData ByteString
  deriving (Show, Eq, Ord, Generic)

instance ToSchema CSVData where
  declareNamedSchema _ = pure $ NamedSchema (Just "CSV") mempty

instance Accept CSV where
  contentType _ = "text" // "csv" -- /: ("charset", "utf-8")

instance MimeRender CSV CSVData where
  mimeRender _ = coerce

instance MimeUnrender CSV CSVData where
  mimeUnrender _ = Right . coerce

newtype SpreadSheetSplit = SpreadSheetSplit [Text]
  deriving (Show, Eq, Ord, Generic)

instance FromField SpreadSheetSplit where
  parseField v = case decodeUtf8' v of
    Left _ -> fail "Invalid UTF-8 encoding"
    Right txt -> return $ SpreadSheetSplit (T.splitOn "," txt)

data SpreadSheetRecord = SpreadSheetRecord
  { title :: !Text,
    amount :: !Double,
    paidBy :: !Text,
    split :: !SpreadSheetSplit,
    date :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromRecord SpreadSheetRecord where
  parseRecord v
    | length v == 5 = SpreadSheetRecord <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
    | otherwise = fail "Expected exactly 5 fields"

parseRecords :: ByteString -> MyHandler [ExpenseRecordRequest]
parseRecords raw = case decode HasHeader (removeComments raw) of
  Left err ->
    throwError $
      err400
        { errBody = textToLBS $ "Failed to parse CSV: " <> T.pack err
        }
  Right (toList -> records) -> forM records $ \SpreadSheetRecord {..} -> do
    parsedDate <- iso8601ParseM $ T.unpack date
    let splits' = [RecordSplitRequest username share | (username, share) <- toList $ fromListWith (+) [(s, 1) | s <- coerce split]]
    pure $ ExpenseRecordRequest title amount (coerce paidBy) parsedDate splits'
  where
    removeComments = BS.unlines . filter (not . BS.isPrefixOf "#") . BS.lines

importServer :: MyServer ImportAPI
importServer groupId (coerce -> csvData) =
  parseRecords csvData >>= \req ->
    forM req $ addExpenseRecord groupId
