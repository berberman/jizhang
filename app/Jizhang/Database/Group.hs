module Jizhang.Database.Group where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import Jizhang.Database.Schema

insertGroup :: Text -> UUID -> Pg Group
insertGroup groupName ownerUid = do
  newUUID <- liftIO nextRandom
  let group = Group newUUID groupName (UserId ownerUid)
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

updateGroup :: UUID -> Maybe Text -> Pg ()
updateGroup groupId newName =
  runUpdate $
    update
      (_groups jizhangDb)
      ( \group ->
          mconcat $
            [_groupName group <-. val_ x | Just x <- [newName]]
      )
      (\group -> _groupId group ==. val_ groupId)

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
checkGroupExists groupId = do
  groups <- runSelectReturningList $ select $ do
    group <- all_ (_groups jizhangDb)
    guard_ (_groupId group ==. val_ groupId)
    pure group
  pure $ not (null groups)

isGroupOwner :: UUID -> UUID -> Pg Bool
isGroupOwner userId groupId = do
  result <- runSelectReturningList $ select $ do
    group <- all_ (_groups jizhangDb)
    guard_ (_groupId group ==. val_ groupId)
    guard_ (_groupOwner group ==. val_ (UserId userId))
    pure group
  pure $ not (null result)

getGroupOwner :: UUID -> Pg (Maybe UUID)
getGroupOwner groupId = do
  result <- runSelectReturningOne $ select $ do
    group <- all_ (_groups jizhangDb)
    guard_ (_groupId group ==. val_ groupId)
    pure (unUserId $ _groupOwner group)
  pure result

addGroupMember :: UUID -> UUID -> Pg GroupMember
addGroupMember userId groupId = let gm = GroupMember (UserId userId) (GroupId groupId) True in runInsert (insert (_groupMembers jizhangDb) $ insertValues [gm]) >> pure gm

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
isUserInGroup userId groupId = do
  members <- runSelectReturningList $ select $ do
    gm <- all_ (_groupMembers jizhangDb)
    guard_ (_gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))
    guard_ (_gmActive gm ==. val_ True)
    pure gm
  pure $ not (null members)

-- | Check whether the user has a membership row (active or inactive)
isUserInGroupIncludingInactive :: UUID -> UUID -> Pg Bool
isUserInGroupIncludingInactive userId groupId = do
  members <- runSelectReturningList $ select $ do
    gm <- all_ (_groupMembers jizhangDb)
    guard_ (_gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))
    pure gm
  pure $ not (null members)

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
