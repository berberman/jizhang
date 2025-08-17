{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Jizhang.Database.Schema where

import Data.Int (Int8)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (Day)
import Database.Beam
import Jizhang.Common.MyUUID

type Username = Text

newtype UserT f = User
  { _username :: C f Username
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Eq User

deriving instance Show User

deriving instance Ord User

instance Table UserT where
  data PrimaryKey UserT f = UserId {unUserId :: C f Username} deriving (Generic, Beamable)
  primaryKey = UserId . _username

type UserKey = PrimaryKey UserT Identity

deriving instance Eq UserKey

deriving instance Show UserKey

deriving instance Ord UserKey

data GroupT f = Group
  { _groupId :: C f MyUUID,
    _groupName :: C f Text
  }
  deriving (Generic, Beamable)

instance Table GroupT where
  data PrimaryKey GroupT f = GroupId {unGroupId :: C f MyUUID} deriving (Generic, Beamable)
  primaryKey = GroupId . _groupId

type Group = GroupT Identity

deriving instance Eq Group

deriving instance Show Group

deriving instance Ord Group

type GroupKey = PrimaryKey GroupT Identity

deriving instance Eq GroupKey

deriving instance Show GroupKey

deriving instance Ord GroupKey

data GroupMemberT f = GroupMember
  { _gmUser :: PrimaryKey UserT f,
    _gmGroup :: PrimaryKey GroupT f
  }
  deriving (Generic, Beamable)

type GroupMember = GroupMemberT Identity

instance Table GroupMemberT where
  data PrimaryKey GroupMemberT f = GroupMemberId {gmUnUser :: PrimaryKey UserT f, gmUnGroup :: PrimaryKey GroupT f} deriving (Generic, Beamable)
  primaryKey = GroupMemberId <$> _gmUser <*> _gmGroup

type GroupMemberKey = PrimaryKey GroupMemberT Identity

deriving instance Eq GroupMemberKey

deriving instance Show GroupMemberKey

deriving instance Ord GroupMemberKey

deriving instance Eq GroupMember

deriving instance Show GroupMember

deriving instance Ord GroupMember

data RecordT f = Record
  { _recordId :: C f MyUUID,
    _recordGroup :: PrimaryKey GroupT f,
    _title :: C f Text,
    _amount :: C f Double,
    _paidBy :: PrimaryKey UserT f,
    -- | Whether this record is a transfer to another user instead of a group expense
    _transferTo :: PrimaryKey UserT (Nullable f),
    _date :: C f Day
  }
  deriving (Generic, Beamable)

isTransferRecord :: Record -> Bool
isTransferRecord record = isJust $ unUserId (_transferTo record)

type Record = RecordT Identity

instance Table RecordT where
  data PrimaryKey RecordT f = RecordId {unRecordId :: C f MyUUID} deriving (Generic, Beamable)
  primaryKey = RecordId . _recordId

type RecordKey = PrimaryKey RecordT Identity

deriving instance Eq RecordKey

deriving instance Show RecordKey

deriving instance Ord RecordKey

deriving instance Eq (PrimaryKey UserT (Nullable Identity))

deriving instance Show (PrimaryKey UserT (Nullable Identity))

deriving instance Ord (PrimaryKey UserT (Nullable Identity))

deriving instance Eq Record

deriving instance Show Record

deriving instance Ord Record

data RecordSplitT f = RecordSplit
  { _rsRecord :: PrimaryKey RecordT f,
    _rsUser :: PrimaryKey UserT f,
    _share :: C f Int8,
    -- | Calculated from the share. Should be updated when the record is updated.
    _splitAmount :: C f Double
  }
  deriving (Generic, Beamable)

type RecordSplit = RecordSplitT Identity

type RecordSplitKey = PrimaryKey RecordSplitT Identity

deriving instance Eq (PrimaryKey RecordSplitT Identity)

deriving instance Show (PrimaryKey RecordSplitT Identity)

deriving instance Ord (PrimaryKey RecordSplitT Identity)

deriving instance Eq RecordSplit

deriving instance Show RecordSplit

deriving instance Ord RecordSplit

instance Table RecordSplitT where
  data PrimaryKey RecordSplitT f = RecordSplitId {rsUnRecordId :: PrimaryKey RecordT f, rsUnUserId :: PrimaryKey UserT f} deriving (Generic, Beamable)
  primaryKey :: RecordSplitT column -> PrimaryKey RecordSplitT column
  primaryKey = RecordSplitId <$> _rsRecord <*> _rsUser

data JizhangDb f = JizhangDb
  { _users :: f (TableEntity UserT),
    _groups :: f (TableEntity GroupT),
    _groupMembers :: f (TableEntity GroupMemberT),
    _records :: f (TableEntity RecordT),
    _recordSplits :: f (TableEntity RecordSplitT)
  }
  deriving (Generic, Database be)

jizhangDb :: DatabaseSettings be JizhangDb
jizhangDb =
  defaultDbSettings
    `withDbModification` JizhangDb
      { _users = setEntityName "users",
        _groups = setEntityName "groups",
        _groupMembers = setEntityName "group_members",
        _records = setEntityName "records",
        _recordSplits = setEntityName "record_splits"
      }
