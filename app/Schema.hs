{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Int (Int8)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Beam
import MyUUID

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
  data PrimaryKey UserT f = UserId (C f Username) deriving (Generic, Beamable)
  primaryKey = UserId . _username

type UserKey = PrimaryKey UserT Identity

deriving instance Eq (PrimaryKey UserT Identity)

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Ord (PrimaryKey UserT Identity)

data GroupT f = Group
  { _groupId :: C f MyUUID,
    _groupName :: C f Text,
    _description :: C f Text
  }
  deriving (Generic, Beamable)

instance Table GroupT where
  data PrimaryKey GroupT f = GroupId (C f MyUUID) deriving (Generic, Beamable)
  primaryKey = GroupId . _groupId

type Group = GroupT Identity

deriving instance Eq Group

deriving instance Show Group

deriving instance Ord Group

type GroupKey = PrimaryKey GroupT Identity

deriving instance Eq (PrimaryKey GroupT Identity)

deriving instance Show (PrimaryKey GroupT Identity)

deriving instance Ord (PrimaryKey GroupT Identity)

data GroupMemberT f = GroupMember
  { _gmUser :: PrimaryKey UserT f,
    _gmGroup :: PrimaryKey GroupT f
  }
  deriving (Generic, Beamable)

type GroupMember = GroupMemberT Identity

instance Table GroupMemberT where
  data PrimaryKey GroupMemberT f = GroupMemberId (PrimaryKey UserT f) (PrimaryKey GroupT f) deriving (Generic, Beamable)
  primaryKey = GroupMemberId <$> _gmUser <*> _gmGroup

type GroupMemberKey = PrimaryKey GroupMemberT Identity

deriving instance Eq (PrimaryKey GroupMemberT Identity)

deriving instance Show (PrimaryKey GroupMemberT Identity)

deriving instance Ord (PrimaryKey GroupMemberT Identity)

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
    _createdAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type Record = RecordT Identity

instance Table RecordT where
  data PrimaryKey RecordT f = RecordId (C f MyUUID) deriving (Generic, Beamable)
  primaryKey = RecordId . _recordId

type RecordKey = PrimaryKey RecordT Identity

deriving instance Eq (PrimaryKey RecordT Identity)

deriving instance Show (PrimaryKey RecordT Identity)

deriving instance Ord (PrimaryKey RecordT Identity)

deriving instance Eq (PrimaryKey UserT (Nullable Identity))

deriving instance Show (PrimaryKey UserT (Nullable Identity))

deriving instance Ord (PrimaryKey UserT (Nullable Identity))

deriving instance Eq Record

deriving instance Show Record

deriving instance Ord Record

data RecordSplitT f = RecordSplit
  { _rsRecord :: PrimaryKey RecordT f,
    _rsUser :: PrimaryKey UserT f,
    _percentage :: C f Int8,
    -- | Calculated from the percentage
    -- May be null if the split amount is not yet calculated
    _splitAmount :: C f (Maybe Double)
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
  data PrimaryKey RecordSplitT f = RecordSplitId (PrimaryKey RecordT f) (PrimaryKey UserT f) deriving (Generic, Beamable)
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
