{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | JSON pagination and bulk envelopes, plus in-memory query helpers. Record keys
-- retain their Haskell spelling; nullable fields encode as null, not omitted keys.
-- Generic decoders accept extra keys and do not validate envelope consistency.
module Jizhang.API.Types.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.List (sortOn)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Effective page bounds and count after sorting, before slicing. The decoder
-- accepts the supplied numbers and flags without checking their consistency.
data PageInfo = PageInfo
  { -- | Zero-based number of entries skipped.
    offset :: !Int,
    -- | Effective page size, not the number of returned items.
    limit :: !Int,
    -- | Total entries available before this page is selected.
    total :: !Int,
    -- | Whether the effective page end precedes the total.
    hasNext :: !Bool,
    -- | Whether the effective offset is greater than zero.
    hasPrev :: !Bool
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Page contents with the query and sort text echoed verbatim. Missing or null
-- appliedQuery/appliedSort decode to Nothing; decoding does not paginate items.
data PaginatedResponse a = PaginatedResponse
  { -- | Selected page entries; order is the order produced by the supplied sorter.
    items :: ![a],
    -- | Effective bounds and total, not a cursor for a later database snapshot.
    pageInfo :: !PageInfo,
    -- | Query supplied by the caller, or null when unspecified.
    appliedQuery :: !(Maybe Text),
    -- | Sort supplied by the caller, or null when unspecified.
    appliedSort :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | An itemsToProcess array; decoding imposes no size or non-emptiness limit.
data BulkRequest a = BulkRequest
  { itemsToProcess :: ![a]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance (ToSchema a) => ToSchema (PaginatedResponse a)

deriving anyclass instance (ToSchema a) => ToSchema (BulkRequest a)

-- | Optional pagination inputs using the literal pagination-prefixed JSON keys.
-- Missing or null fields mean unspecified; bounds are applied by 'paginateResponse'.
data PaginationParams = PaginationParams
  { -- | Search text; this container does not apply filtering.
    paginationQuery :: !(Maybe Text),
    -- | Requested skip count, before normalization.
    paginationOffset :: !(Maybe Int),
    -- | Requested page size, before normalization.
    paginationLimit :: !(Maybe Int),
    -- | Endpoint-specific sort expression, not validated by this container.
    paginationSort :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Leave every pagination choice unspecified so endpoint defaults can apply.
defaultPaginationParams :: PaginationParams
defaultPaginationParams = PaginationParams Nothing Nothing Nothing Nothing

-- | Preserve query, offset, limit and sort order for endpoint argument lists.
toPaginationArgs :: PaginationParams -> (Maybe Text, Maybe Int, Maybe Int, Maybe Text)
toPaginationArgs PaginationParams {..} =
  (paginationQuery, paginationOffset, paginationLimit, paginationSort)

-- | Unicode case-folded substring search, without trimming or tokenization.
matchesQuery :: Text -> Text -> Bool
matchesQuery queryText value = T.toCaseFold queryText `T.isInfixOf` T.toCaseFold value

-- | A leading minus reverses ascending key order, including equal-key order.
-- The rest of the sort text is not interpreted or checked against the accessor.
applySort :: (Ord b) => Text -> (a -> b) -> [a] -> [a]
applySort sortText fieldAccessor xs =
  case T.stripPrefix (T.pack "-") sortText of
    Just _ -> reverse $ sortOn fieldAccessor xs
    Nothing -> sortOn fieldAccessor xs

-- | Sort then slice: offset defaults to 0 and is clamped nonnegative; limit
-- defaults to 20 and is clamped to 1..100. Query text is echoed, not applied here.
paginateResponse :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ([a] -> Maybe Text -> [a]) -> [a] -> PaginatedResponse a
paginateResponse mOffset mLimit mQuery mSort sorter xs =
  let offsetValue = max 0 $ maybe 0 id mOffset
      limitValue = max 1 $ min 100 $ maybe 20 id mLimit
      sorted = sorter xs mSort
      totalCount = length sorted
      paged = take limitValue $ drop offsetValue sorted
   in PaginatedResponse paged (PageInfo offsetValue limitValue totalCount (offsetValue + limitValue < totalCount) (offsetValue > 0)) mQuery mSort
