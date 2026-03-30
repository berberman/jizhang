{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Common
  ( WithPagination,
    PaginatedGetAPI,
  )
where

import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Jizhang.API.Types.Common (PaginatedResponse)
import Servant (Get, JSON, QueryParam, (:>))

type WithPagination api =
  QueryParam "query" Text
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> QueryParam "sort" Text
    :> api

type PaginatedGetAPI (path :: Symbol) a =
  path :> WithPagination (Get '[JSON] (PaginatedResponse a))
