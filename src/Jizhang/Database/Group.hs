{-# LANGUAGE OverloadedStrings #-}

-- | Group policy and membership storage. Active-member queries exclude departed
-- users, while retained membership rows let historical records resolve their users.
module Jizhang.Database.Group where

import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runUpdateReturningList)
import Database.Beam.Postgres
import Jizhang.API.Types.Money (CurrencyCode, CurrencyMode (..), currencyCodeText)
import Jizhang.Database.Schema

currencyModeText :: CurrencyMode -> Text
currencyModeText SingleCurrency = "single"
currencyModeText MultiCurrency = "multi"

-- | Create an unlocked currency policy without adding the owner as a member.
-- Callers must insert any required membership in the same transaction.
insertGroup :: Text -> UUID -> CurrencyMode -> CurrencyCode -> Pg Group
insertGroup groupName ownerUid mode currency = do
  newUUID <- liftIO nextRandom
  let group = Group newUUID groupName (UserId ownerUid) (currencyModeText mode) (currencyCodeText currency) False
  runInsert $ insert (_groups jizhangDb) $ insertValues [group]
  pure group

getAllMembersInGroup :: UUID -> Pg [GroupMember]
getAllMembersInGroup groupId = runSelectReturningList $ select $ do
  gm <- all_ (_groupMembers jizhangDb)
  guard_ (_gmGroup gm ==. val_ (GroupId groupId))
  guard_ (_gmActive gm ==. val_ True)
  pure gm

getAllGroupWithMembers :: Pg [(Group, Maybe GroupMember)]
getAllGroupWithMembers = runSelectReturningList $ select $ do
  group <- all_ (_groups jizhangDb)
  members <- leftJoin_ (all_ $ _groupMembers jizhangDb) (\gm -> _gmGroup gm ==. primaryKey group &&. _gmActive gm ==. val_ True)
  pure (group, members)

-- | Read policy and active membership in separate statements. A consistent
-- snapshot across both reads requires a suitable caller-owned transaction.
getGroupWithMembers :: UUID -> Pg (Maybe (Group, [GroupMember]))
getGroupWithMembers groupId = do
  group <- runSelectReturningOne $ select $ do
    g <- all_ (_groups jizhangDb)
    guard_ (_groupId g ==. val_ groupId)
    pure g
  case group of
    Just g -> do
      members <- getAllMembersInGroup groupId
      pure $ Just (g, members)
    Nothing -> pure Nothing

-- | Conditional update is atomic with first-record currency locking. A failed
-- policy change does not rename the group either.
updateGroupSettings :: UUID -> Text -> CurrencyMode -> CurrencyCode -> Pg (Maybe Group)
updateGroupSettings groupId newName mode currency =
  listToMaybe
    <$> runUpdateReturningList
      ( update
          (_groups jizhangDb)
          ( \group ->
              mconcat
                [ _groupName group <-. val_ newName,
                  _currencyMode group <-. val_ (currencyModeText mode),
                  _defaultCurrency group <-. val_ (currencyCodeText currency)
                ]
          )
          ( \group ->
              _groupId group
                ==. val_ groupId
                &&. ( not_ (_currencyLocked group)
                        ||. ( _currencyMode group
                                ==. val_ (currencyModeText mode)
                                &&. _defaultCurrency group
                                ==. val_ (currencyCodeText currency)
                            )
                    )
          )
      )

-- | Must run in the same transaction as the financial mutation. Updating the
-- row serializes against settings changes, even when it was already locked.
lockGroupCurrency :: UUID -> CurrencyMode -> CurrencyCode -> Pg Bool
lockGroupCurrency groupId mode currency =
  not . null
    <$> runUpdateReturningList
      ( update
          (_groups jizhangDb)
          (\group -> _currencyLocked group <-. val_ True)
          ( \group ->
              -- Reject policy drift since validation, rather than locking a different currency pair.
              _groupId group
                ==. val_ groupId
                &&. _currencyMode group
                ==. val_ (currencyModeText mode)
                &&. _defaultCurrency group
                ==. val_ (currencyCodeText currency)
          )
      )

-- | Reassign ownership without checking membership or the acting user's rights.
-- Callers enforce those rules before committing this update.
updateGroupOwner :: UUID -> UUID -> Pg ()
updateGroupOwner groupId newOwnerUid =
  runUpdate $
    update
      (_groups jizhangDb)
      (\group -> _groupOwner group <-. val_ (UserId newOwnerUid))
      (\group -> _groupId group ==. val_ groupId)

deleteGroup :: UUID -> Pg ()
deleteGroup groupId = runDelete $ delete (_groups jizhangDb) (\group -> _groupId group ==. val_ groupId)

checkGroupExists :: UUID -> Pg Bool
checkGroupExists groupId =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          group <- all_ (_groups jizhangDb)
          guard_ (_groupId group ==. val_ groupId)
          pure (_groupId group)
      )

isGroupOwner :: UUID -> UUID -> Pg Bool
isGroupOwner userId groupId =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          group <- all_ (_groups jizhangDb)
          guard_ (_groupId group ==. val_ groupId)
          guard_ (_groupOwner group ==. val_ (UserId userId))
          pure (_groupId group)
      )

getGroupOwner :: UUID -> Pg (Maybe UUID)
getGroupOwner groupId = do
  result <- runSelectReturningOne $ select $ do
    group <- all_ (_groups jizhangDb)
    guard_ (_groupId group ==. val_ groupId)
    pure (unUserId $ _groupOwner group)
  pure result

-- | Insert a fresh membership, not an upsert. Reactivate an existing inactive
-- row instead so the composite user/group identity remains unchanged.
addGroupMember :: UUID -> UUID -> Pg GroupMember
addGroupMember userId groupId = let gm = GroupMember (UserId userId) (GroupId groupId) True in runInsert (insert (_groupMembers jizhangDb) $ insertValues [gm]) >> pure gm

-- | Retain the membership key for historical allocations while removing access
-- through active-member queries; this does not delete financial history.
deactivateGroupMember :: UUID -> UUID -> Pg ()
deactivateGroupMember userId groupId =
  runUpdate $
    update
      (_groupMembers jizhangDb)
      (\gm -> _gmActive gm <-. val_ False)
      (\gm -> _gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))

reactivateGroupMember :: UUID -> UUID -> Pg ()
reactivateGroupMember userId groupId =
  runUpdate $
    update
      (_groupMembers jizhangDb)
      (\gm -> _gmActive gm <-. val_ True)
      (\gm -> _gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))

-- | Check whether the user is an active member of the group
isUserInGroup :: UUID -> UUID -> Pg Bool
isUserInGroup userId groupId =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          gm <- all_ (_groupMembers jizhangDb)
          guard_ (_gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))
          guard_ (_gmActive gm ==. val_ True)
          pure (unUserId $ _gmUser gm)
      )

-- | Check whether the user has a membership row (active or inactive)
isUserInGroupIncludingInactive :: UUID -> UUID -> Pg Bool
isUserInGroupIncludingInactive userId groupId =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          gm <- all_ (_groupMembers jizhangDb)
          guard_ (_gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))
          pure (unUserId $ _gmUser gm)
      )

-- | Get all users who are (or were) members of a group, for FK resolution.
-- Includes inactive members since they may still have records in the group.
getUsersForGroup :: UUID -> Pg [User]
getUsersForGroup groupId = runSelectReturningList $ select $ do
  gm <- all_ (_groupMembers jizhangDb)
  guard_ (_gmGroup gm ==. val_ (GroupId groupId))
  related_ (_users jizhangDb) (_gmUser gm)

getGroupsForUser :: UUID -> Pg [Group]
getGroupsForUser userId = runSelectReturningList $ select $ do
  gm <- all_ (_groupMembers jizhangDb)
  guard_ (_gmUser gm ==. val_ (UserId userId))
  guard_ (_gmActive gm ==. val_ True)
  related_ (_groups jizhangDb) (_gmGroup gm)
