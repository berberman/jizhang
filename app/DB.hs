{-# LANGUAGE FlexibleContexts #-}

module DB where

import Data.Int (Int8)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Sqlite.Connection
import MyUUID
import Schema

insertUser :: Username -> SqliteM User
insertUser username = let user = User username in (runInsert (insert (_users jizhangDb) $ insertValues [user]) >> pure user)

deleteUser :: Username -> SqliteM ()
deleteUser username = runDelete $ delete (_users jizhangDb) (\user -> _username user ==. val_ username)

insertGroup :: Text -> SqliteM Group
insertGroup groupName = do
  newUUID <- liftIO randomMyUUID
  let group = Group newUUID groupName
  runInsert $ insert (_groups jizhangDb) $ insertValues [group]
  pure group

getGroupWithMembers :: MyUUID -> SqliteM (Maybe (Group, [GroupMember]))
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

checkUserExists :: Username -> SqliteM Bool
checkUserExists username = do
  users <- runSelectReturningList $ select $ do
    user <- all_ (_users jizhangDb)
    guard_ (_username user ==. val_ username)
    pure user
  pure $ not (null users)

updateGroup :: MyUUID -> Maybe Text -> SqliteM ()
updateGroup groupId newName =
  runUpdate $
    update
      (_groups jizhangDb)
      ( \group ->
          mconcat $
            [_groupName group <-. val_ x | Just x <- [newName]]
      )
      (\group -> _groupId group ==. val_ groupId)

deleteGroup :: MyUUID -> SqliteM ()
deleteGroup groupId = runDelete $ delete (_groups jizhangDb) (\group -> _groupId group ==. val_ groupId)

checkGroupExists :: MyUUID -> SqliteM Bool
checkGroupExists groupId = do
  groups <- runSelectReturningList $ select $ do
    group <- all_ (_groups jizhangDb)
    guard_ (_groupId group ==. val_ groupId)
    pure group
  pure $ not (null groups)

addGroupMember :: Username -> MyUUID -> SqliteM GroupMember
addGroupMember userId groupId = let gm = GroupMember (UserId userId) (GroupId groupId) in runInsert (insert (_groupMembers jizhangDb) $ insertValues [gm]) >> pure gm

deleteGroupMember :: Username -> MyUUID -> SqliteM ()
deleteGroupMember userId groupId = runDelete $ delete (_groupMembers jizhangDb) (\gm -> _gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))

isUserInGroup :: Username -> MyUUID -> SqliteM Bool
isUserInGroup userId groupId = do
  members <- runSelectReturningList $ select $ do
    gm <- all_ (_groupMembers jizhangDb)
    guard_ (_gmUser gm ==. val_ (UserId userId) &&. _gmGroup gm ==. val_ (GroupId groupId))
    pure gm
  pure $ not (null members)

getAllUsers :: SqliteM [User]
getAllUsers = runSelectReturningList $ select $ all_ (_users jizhangDb)

getAllGroups :: SqliteM [Group]
getAllGroups = runSelectReturningList $ select $ all_ (_groups jizhangDb)

getAllMembersInGroup :: MyUUID -> SqliteM [GroupMember]
getAllMembersInGroup groupId = runSelectReturningList $ select $ do
  gm <- all_ (_groupMembers jizhangDb)
  guard_ (_gmGroup gm ==. val_ (GroupId groupId))
  pure gm

getAllGroupWithMembers :: SqliteM [(Group, GroupMember)]
getAllGroupWithMembers = runSelectReturningList $ select $ do
  group <- all_ (_groups jizhangDb)
  members <- oneToMany_ (_groupMembers jizhangDb) _gmGroup group
  pure (group, members)

insertRecord :: Text -> Double -> Username -> Maybe Username -> MyUUID -> UTCTime -> SqliteM Record
insertRecord title amount paidBy transferTo groupId recordTime = do
  newUUID <- liftIO randomMyUUID
  let record =
        Record
          { _recordId = newUUID,
            _recordGroup = GroupId groupId,
            _title = title,
            _amount = amount,
            _paidBy = UserId paidBy,
            _transferTo = maybe nothing_ (just_ . UserId) transferTo,
            _createdAt = recordTime
          }
  runInsert $ insert (_records jizhangDb) $ insertValues [record]
  pure record

getAllRecordsInGroup :: MyUUID -> SqliteM [Record]
getAllRecordsInGroup groupId = runSelectReturningList $ select $ do
  record <- all_ (_records jizhangDb)
  guard_ (_recordGroup record ==. val_ (GroupId groupId))
  pure record

deleteRecord :: MyUUID -> SqliteM ()
deleteRecord recordId = runDelete $ delete (_records jizhangDb) (\record -> _recordId record ==. val_ recordId)

updateRecord :: MyUUID -> Maybe Text -> Maybe Double -> Maybe Username -> Maybe (Maybe Username) -> Maybe UTCTime -> SqliteM ()
updateRecord recordId title amount paidBy transferTo recordTime = do
  runUpdate $
    update
      (_records jizhangDb)
      ( \record ->
          mconcat $
            [_title record <-. val_ x | Just x <- [title]]
              <> [_amount record <-. val_ x | Just x <- [amount]]
              <> [_paidBy record <-. val_ (UserId x) | Just x <- [paidBy]]
              <> [_transferTo record <-. val_ (maybe nothing_ (just_ . UserId) x) | Just x <- [transferTo]]
              <> [_createdAt record <-. val_ x | Just x <- [recordTime]]
      )
      (\record -> _recordId record ==. val_ recordId)

getAllRecords :: SqliteM [Record]
getAllRecords = runSelectReturningList $ select $ all_ (_records jizhangDb)

insertRecordSplit :: MyUUID -> Username -> Int8 -> SqliteM RecordSplit
insertRecordSplit recordId userId percentage = do
  let split = RecordSplit (RecordId recordId) (UserId userId) percentage Nothing
  runInsert (insert (_recordSplits jizhangDb) $ insertValues [split]) >> pure split

deleteRecordSplitsForRecord :: MyUUID -> SqliteM ()
deleteRecordSplitsForRecord recordId = runDelete $ delete (_recordSplits jizhangDb) (\rs -> _rsRecord rs ==. val_ (RecordId recordId))

updateRecordSplit :: MyUUID -> Username -> Maybe Int8 -> Maybe (Maybe Double) -> SqliteM ()
updateRecordSplit recordId userId percentage splitAmount = do
  runUpdate $
    update
      (_recordSplits jizhangDb)
      ( \rs ->
          mconcat $
            [_percentage rs <-. val_ x | Just x <- [percentage]]
              <> [_splitAmount rs <-. val_ x | Just x <- [splitAmount]]
      )
      (\rs -> _rsRecord rs ==. val_ (RecordId recordId) &&. _rsUser rs ==. val_ (UserId userId))

getRecordSplitsForRecord :: MyUUID -> SqliteM [RecordSplit]
getRecordSplitsForRecord recordId = runSelectReturningList $ select $ do
  rs <- all_ (_recordSplits jizhangDb)
  guard_ (_rsRecord rs ==. val_ (RecordId recordId))
  pure rs

getRecordsWithSplitsForGroup :: MyUUID -> SqliteM [(Record, RecordSplit)]
getRecordsWithSplitsForGroup groupId = runSelectReturningList $ select $ do
  record <- all_ (_records jizhangDb)
  guard_ (_recordGroup record ==. val_ (GroupId groupId))
  splits <- oneToMany_ (_recordSplits jizhangDb) _rsRecord record
  pure (record, splits)

getGroupsForUser :: Username -> SqliteM [Group]
getGroupsForUser userId = runSelectReturningList $ select $ do
  gm <- all_ (_groupMembers jizhangDb)
  guard_ (_gmUser gm ==. val_ (UserId userId))
  related_ (_groups jizhangDb) (_gmGroup gm)
