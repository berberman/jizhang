{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Jizhang.API.Types.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.List (sortOn)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data PageInfo = PageInfo
  { offset :: !Int,
    limit :: !Int,
    total :: !Int,
    hasNext :: !Bool,
    hasPrev :: !Bool
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaginatedResponse a = PaginatedResponse
  { items :: ![a],
    pageInfo :: !PageInfo,
    appliedQuery :: !(Maybe Text),
    appliedSort :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

data BulkRequest a = BulkRequest
  { itemsToProcess :: ![a]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ToSchema a => ToSchema (PaginatedResponse a)

deriving anyclass instance ToSchema a => ToSchema (BulkRequest a)

data PaginationParams = PaginationParams
  { paginationQuery :: !(Maybe Text),
    paginationOffset :: !(Maybe Int),
    paginationLimit :: !(Maybe Int),
    paginationSort :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

defaultPaginationParams :: PaginationParams
defaultPaginationParams = PaginationParams Nothing Nothing Nothing Nothing

toPaginationArgs :: PaginationParams -> (Maybe Text, Maybe Int, Maybe Int, Maybe Text)
toPaginationArgs PaginationParams {..} =
  (paginationQuery, paginationOffset, paginationLimit, paginationSort)

matchesQuery :: Text -> Text -> Bool
matchesQuery queryText value = T.toCaseFold queryText `T.isInfixOf` T.toCaseFold value

applySort :: Ord b => Text -> (a -> b) -> [a] -> [a]
applySort sortText fieldAccessor xs =
  case T.stripPrefix (T.pack "-") sortText of
    Just _ -> reverse $ sortOn fieldAccessor xs
    Nothing -> sortOn fieldAccessor xs

paginateResponse :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ([a] -> Maybe Text -> [a]) -> [a] -> PaginatedResponse a
paginateResponse mOffset mLimit mQuery mSort sorter xs =
  let offsetValue = max 0 $ maybe 0 id mOffset
      limitValue = max 1 $ min 100 $ maybe 20 id mLimit
      sorted = sorter xs mSort
      totalCount = length sorted
      paged = take limitValue $ drop offsetValue sorted
   in PaginatedResponse paged (PageInfo offsetValue limitValue totalCount (offsetValue + limitValue < totalCount) (offsetValue > 0)) mQuery mSort
