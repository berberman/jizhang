{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared optional list parameters. Route types only decode these values;
-- individual handlers choose filtering, ordering and pagination policy.
module Jizhang.API.Common
  ( WithPagination,
    PaginatedGetAPI,
  )
where

import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Jizhang.API.Types.Common (PaginatedResponse)
import Servant (Get, JSON, QueryParam, (:>))

-- | Keep query, offset, limit and sort optional so handlers can apply their defaults.
type WithPagination api =
  QueryParam "query" Text
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> QueryParam "sort" Text
    :> api

-- | A named JSON collection whose envelope carries the selected page metadata.
type PaginatedGetAPI (path :: Symbol) a =
  path :> WithPagination (Get '[JSON] (PaginatedResponse a))
