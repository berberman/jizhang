{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Member-facing group lifecycle, membership and import routes. Owners manage policy
-- and membership; financial history keeps inactive participants available to projection.
module Jizhang.API.Group
  ( GroupAPI,
    groupServer,
    getGroup,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Jizhang.API.Common (WithPagination)
import Jizhang.API.ExchangeRate (createExchangeQuote)
import Jizhang.API.GroupImport
import Jizhang.API.Record.Service (addExpenseRecords)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (UserPrincipal))
import Jizhang.API.Types
import qualified Jizhang.API.Types.ExchangeRate as Q
import Jizhang.API.Internal.Error (textToLBS, fromStoredData, fetchOrFail)
import Jizhang.API.Group.Projection (groupToApiGroup)
import Jizhang.API.Internal.Resource (lookupUserById, getGroupUserMap, matchesUserQuery, ensureGroupExists, ensureGroupOwner, ensureGroupMember, validateGroupName)
import Jizhang.Currency (lookupCurrency)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

-- | Group reads and imports require membership; policy and membership changes require
-- ownership, except that a non-owner may leave their own active membership.
type GroupAPI =
  -- Create a new group
  "groups" :> ReqBody '[JSON] CreateGroupRequest :> Post '[JSON] Group
    -- Get a specific group (requires membership)
    :<|> "groups" :> Capture "groupId" GroupId :> Get '[JSON] Group
    -- Update a specific group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> ReqBody '[JSON] UpdateGroupRequest :> Put '[JSON] Group
    -- Delete a specific group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> Delete '[JSON] NoContent
    -- Add a member to a group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> ReqBody '[JSON] UserId :> Post '[JSON] Group
    -- Delete a member from a group (requires ownership or self-deletion)
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> Capture "userId" UserId :> Delete '[JSON] NoContent
    -- Transfer group ownership (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> "owner" :> ReqBody '[JSON] UserId :> Put '[JSON] Group
    -- Import expense records from CSV (requires membership)
    :<|> "groups" :> Capture "groupId" GroupId :> "import" :> QueryParam' '[Required, Strict] "currency" CurrencyCode :> QueryParam "exchangeQuoteId" ExchangeQuoteId :> QueryParam "manualRate" DecimalRate :> ReqBody '[CSV] CSVData :> Post '[JSON] [Record]
    -- List eligible member choices (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> "member-candidates" :> WithPagination (Get '[JSON] (PaginatedResponse User))
    -- Fetch and persist an explicitly requested reference quote (requires membership)
    :<|> "groups" :> Capture "groupId" GroupId :> "exchange-quotes" :> ReqBody '[JSON] Q.ExchangeQuoteRequest :> Post '[JSON] Q.ExchangeQuote

groupServer :: AuthUser -> MyServer GroupAPI
groupServer authUser =
  createGroup authUser
    :<|> getGroupEndpoint authUser
    :<|> updateGroup authUser
    :<|> deleteGroup authUser
    :<|> addGroupMember authUser
    :<|> deleteGroupMember authUser
    :<|> transferOwnership authUser
    :<|> importGroupCSV authUser
    :<|> getMemberCandidates authUser
    :<|> createExchangeQuote authUser

-- | Get a group by ID, checking that the authenticated user is a member
getGroupEndpoint :: AuthUser -> GroupId -> MyHandler Group
getGroupEndpoint authUser (GroupId gId) = do
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  getGroup (GroupId gId)

-- | Offer directory users outside the active membership, including former members.
getMemberCandidates :: AuthUser -> GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse User)
getMemberCandidates authUser (GroupId gId) mQuery mOffset mLimit mSort = do
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  (directory, activeMembers) <- runDB $ (,) <$> D.getUserDirectory <*> D.getAllMembersInGroup gId
  let activeIds = Set.fromList [S.unUserId $ S._gmUser member | member <- activeMembers]
      candidates =
        [ User (UserId uid) handle name
        | (uid, handle, name) <- directory,
          Set.notMember uid activeIds
        ]
      filtered = maybe candidates (\query -> filter (matchesUserQuery query) candidates) mQuery
      handleOf (User _ handle _) = handle
      sortCandidates users Nothing = sortOn handleOf users
      sortCandidates users (Just sortText) = applySort sortText handleOf users
  pure $ paginateResponse mOffset mLimit mQuery mSort sortCandidates filtered

-- | Create the group and its owner's active membership in one transaction.
createGroup :: AuthUser -> CreateGroupRequest -> MyHandler Group
createGroup authUser CreateGroupRequest {..} = do
  logInfo_ $ "Creating group: " <> groupName
  validateGroupName groupName
  currencies <- asks appCurrencies
  _ <-
    either (\message -> throwError $ err400 {errBody = textToLBS message}) pure $
      lookupCurrency currencies defaultCurrency
  let ownerUid = authUserId authUser
  group <- runDBTransaction $ do
    group <- D.insertGroup groupName ownerUid currencyMode defaultCurrency
    _ <- D.addGroupMember ownerUid (S._groupId group)
    pure group
  getGroup (coerce $ S._groupId group)

-- | Shared projection helper with no authorization check; protect it at the route boundary.
getGroup :: GroupId -> MyHandler Group
getGroup (GroupId gId) = do
  logInfo_ $ "Fetching group with ID: " <> T.pack (show gId)
  (g, ms) <- fetchOrFail "Group" gId $ runDB $ D.getGroupWithMembers gId
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  fromStoredData $ groupToApiGroup currencies um g ms

-- | Currency policy changes fail with 409 once financial history has locked settings.
updateGroup :: AuthUser -> GroupId -> UpdateGroupRequest -> MyHandler Group
updateGroup authUser (GroupId gId) UpdateGroupRequest {..} = do
  logInfo_ $ "Updating group " <> toText gId
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  validateGroupName groupName
  currencies <- asks appCurrencies
  _ <-
    either (\message -> throwError $ err400 {errBody = textToLBS message}) pure $
      lookupCurrency currencies defaultCurrency
  updated <- runDB $ D.updateGroupSettings gId groupName currencyMode defaultCurrency
  case updated of
    Nothing -> throwError $ err409 {errBody = "Currency settings are locked after the first record"}
    Just _ -> getGroup (GroupId gId)

deleteGroup :: AuthUser -> GroupId -> MyHandler NoContent
deleteGroup authUser (GroupId gId) = do
  logInfo_ $ "Deleting group with ID: " <> toText gId
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  _ <- runDB $ D.deleteGroup gId
  pure NoContent

addGroupMember :: AuthUser -> GroupId -> UserId -> MyHandler Group
addGroupMember authUser (GroupId gId) (UserId u) = do
  logInfo_ $ "Adding user " <> toText u <> " to group with ID: " <> toText gId
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  sUser <- lookupUserById u
  activeMember <- runDB $ D.isUserInGroup (S._userId sUser) gId
  when activeMember $ throwError $ err409 {errBody = "User is already a member of the group"}
  -- Re-activate if there is an existing inactive membership, otherwise insert new
  existingRow <- runDB $ D.isUserInGroupIncludingInactive (S._userId sUser) gId
  if existingRow
    then runDB $ D.reactivateGroupMember (S._userId sUser) gId
    else void $ runDB $ D.addGroupMember (S._userId sUser) gId
  getGroup (GroupId gId)

-- | Deactivate rather than erase membership; owners must transfer ownership before leaving.
deleteGroupMember :: AuthUser -> GroupId -> UserId -> MyHandler NoContent
deleteGroupMember authUser (GroupId gId) (UserId u) = do
  logInfo_ $ "Deactivating user " <> toText u <> " from group with ID: " <> toText gId
  ensureGroupExists gId
  sUser <- lookupUserById u
  let targetUid = S._userId sUser
  -- Allow the owner to deactivate any member, or a member to leave on their own
  isOwner <- runDB $ D.isGroupOwner (authUserId authUser) gId
  let isSelf = authUserId authUser == targetUid
  unless (isOwner || isSelf) $
    throwError $
      err403 {errBody = "Only the group owner or the member themselves can perform this action"}
  -- The owner cannot be deactivated
  targetIsOwner <- runDB $ D.isGroupOwner targetUid gId
  when targetIsOwner $
    throwError $
      err400 {errBody = "Cannot deactivate the group owner; transfer ownership first"}
  isMember <- runDB $ D.isUserInGroup targetUid gId
  unless isMember $
    throwError $
      err404 {errBody = "User is not an active member of the group"}
  runDB $ D.deactivateGroupMember targetUid gId
  pure NoContent

transferOwnership :: AuthUser -> GroupId -> UserId -> MyHandler Group
transferOwnership authUser (GroupId gId) (UserId u) = do
  logInfo_ $ "Transferring ownership of group " <> toText gId <> " to user " <> toText u
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  sUser <- lookupUserById u
  isMember <- runDB $ D.isUserInGroup (S._userId sUser) gId
  unless isMember $ throwError $ err400 {errBody = "New owner must be a member of the group"}
  runDB $ D.updateGroupOwner gId (S._userId sUser)
  getGroup (GroupId gId)

-- | Rate-limit imports and resolve the whole CSV before the shared atomic write.
importGroupCSV :: AuthUser -> GroupId -> CurrencyCode -> Maybe ExchangeQuoteId -> Maybe DecimalRate -> CSVData -> MyHandler [Record]
importGroupCSV authUser (GroupId rawGroupId) currencyCode quote manual (CSVData csvData) = do
  enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
  group <- fst <$> (fetchOrFail "Group" rawGroupId $ runDB (D.getGroupWithMembers rawGroupId))
  ensureGroupMember (authUserId authUser) rawGroupId
  (info, rateInput) <- resolveImportInput group currencyCode quote manual
  parseRecords info rateInput csvData >>= addExpenseRecords group
