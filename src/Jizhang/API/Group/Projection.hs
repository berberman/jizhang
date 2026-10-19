{-# LANGUAGE OverloadedStrings #-}

-- | Project stored group policy and user references without supplying fallback values.
-- The user map must include the owner and every supplied membership row.
module Jizhang.API.Group.Projection
  ( storedGroupSettings,
    groupToApiGroup
  )
where

import Data.Coerce (coerce)
import Data.Text (Text)
import Jizhang.API.Types.Group (Group (Group), GroupId (GroupId))
import Jizhang.API.Types.Money (CurrencyCode (CurrencyCode), CurrencyInfo, CurrencyMode (..))
import Jizhang.API.User.Projection (UserMap, storedUser)
import Jizhang.Currency (CurrencyCatalog, lookupCurrency)
import qualified Jizhang.Database.Schema as S

-- | Reject unknown modes and currencies rather than silently changing ledger policy.
storedGroupSettings :: CurrencyCatalog -> S.Group -> Either Text (CurrencyMode, CurrencyInfo)
storedGroupSettings catalog group = do
  mode <- case S._currencyMode group of
    "single" -> Right SingleCurrency
    "multi" -> Right MultiCurrency
    _ -> Left "Invalid stored currency mode"
  info <- lookupCurrency catalog (CurrencyCode $ S._defaultCurrency group)
  pure (mode, info)

-- | Project exactly the supplied member rows; active-member selection belongs upstream.
groupToApiGroup :: CurrencyCatalog -> UserMap -> S.Group -> [S.GroupMember] -> Either Text Group
groupToApiGroup catalog users group groupMembers = do
  (mode, base) <- storedGroupSettings catalog group
  ownerUser <- storedUser users (S.unUserId $ S._groupOwner group)
  memberUsers <- traverse (storedUser users . S.unUserId . S._gmUser) groupMembers
  pure $ Group (coerce $ S._groupId group) (S._groupName group) ownerUser memberUsers mode base (S._currencyLocked group)
