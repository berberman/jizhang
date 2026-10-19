{-# LANGUAGE DuplicateRecordFields #-}

-- | Shared ledger request and response vocabulary. Money travels as exact strings;
-- public 'User' values exclude account email and authentication identity details.
-- Import OIDC and exchange-quote contracts from their dedicated modules.
module Jizhang.API.Types
  ( module Jizhang.API.Types.Auth,
    module Jizhang.API.Types.Admin,
    module Jizhang.API.Types.Common,
    module Jizhang.API.Types.Group,
    module Jizhang.API.Types.Money,
    module Jizhang.API.Types.Receipt,
    module Jizhang.API.Types.Record,
    module Jizhang.API.Types.Report,
    module Jizhang.API.Types.Statistics,
    module Jizhang.API.Types.User,
  )
where

import Jizhang.API.Types.Admin
import Jizhang.API.Types.Auth
import Jizhang.API.Types.Common
import Jizhang.API.Types.Group
import Jizhang.API.Types.Money
import Jizhang.API.Types.Receipt
import Jizhang.API.Types.Record
import Jizhang.API.Types.Report
import Jizhang.API.Types.Statistics
import Jizhang.API.Types.User
