{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Jizhang.Database.Schema where

import Data.Int (Int16)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Beam

type Username = Text

data UserT f = User
  { _userId :: C f UUID,
    _username :: C f Username,
    _passwordHash :: C f Text
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Eq User

deriving instance Show User

deriving instance Ord User

instance Table UserT where
  data PrimaryKey UserT f = UserId {unUserId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = UserId . _userId

type UserKey = PrimaryKey UserT Identity

deriving instance Eq UserKey

deriving instance Show UserKey

deriving instance Ord UserKey

data AdminT f = Admin
  { _adminId :: C f UUID,
    _adminUsername :: C f Text,
    _adminPasswordHash :: C f Text
  }
  deriving (Generic, Beamable)

type Admin = AdminT Identity

deriving instance Eq Admin

deriving instance Show Admin

deriving instance Ord Admin

instance Table AdminT where
  data PrimaryKey AdminT f = AdminId {unAdminId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = AdminId . _adminId

type AdminKey = PrimaryKey AdminT Identity

deriving instance Eq AdminKey

deriving instance Show AdminKey

deriving instance Ord AdminKey

data GroupT f = Group
  { _groupId :: C f UUID,
    _groupName :: C f Text,
    _groupOwner :: PrimaryKey UserT f
  }
  deriving (Generic, Beamable)

instance Table GroupT where
  data PrimaryKey GroupT f = GroupId {unGroupId :: C f UUID} deriving (Generic, Beamable)
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
    _gmGroup :: PrimaryKey GroupT f,
    _gmActive :: C f Bool
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

data ReceiptT f = Receipt
  { _receiptId :: C f UUID,
    _receiptGroup :: PrimaryKey GroupT f,
    _receiptUploadedBy :: PrimaryKey UserT f,
    _receiptNote :: C f Text,
    _receiptCreatedAt :: C f UTCTime
  }
  deriving (Generic, Beamable)

type Receipt = ReceiptT Identity

deriving instance Eq Receipt

deriving instance Show Receipt

deriving instance Ord Receipt

instance Table ReceiptT where
  data PrimaryKey ReceiptT f = ReceiptId {unReceiptId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = ReceiptId . _receiptId

type ReceiptKey = PrimaryKey ReceiptT Identity

deriving instance Eq ReceiptKey

deriving instance Show ReceiptKey

deriving instance Ord ReceiptKey

data RecordT f = Record
  { _recordId :: C f UUID,
    _recordGroup :: PrimaryKey GroupT f,
    _title :: C f Text,
    _amount :: C f Double,
    _paidBy :: PrimaryKey UserT f,
    -- | Whether this record is a transfer to another user instead of a group expense
    _transferTo :: PrimaryKey UserT (Nullable f),
    _date :: C f Day,
    _createdAt :: C f UTCTime,
    -- | Optional receipt this record belongs to
    _recordReceipt :: PrimaryKey ReceiptT (Nullable f)
  }
  deriving (Generic, Beamable)

isTransferRecord :: Record -> Bool
isTransferRecord record = isJust $ unUserId (_transferTo record)

type Record = RecordT Identity

instance Table RecordT where
  data PrimaryKey RecordT f = RecordId {unRecordId :: C f UUID} deriving (Generic, Beamable)
  primaryKey = RecordId . _recordId

type RecordKey = PrimaryKey RecordT Identity

deriving instance Eq RecordKey

deriving instance Show RecordKey

deriving instance Ord RecordKey

deriving instance Eq (PrimaryKey UserT (Nullable Identity))

deriving instance Show (PrimaryKey UserT (Nullable Identity))

deriving instance Ord (PrimaryKey UserT (Nullable Identity))

deriving instance Eq (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Show (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Ord (PrimaryKey ReceiptT (Nullable Identity))

deriving instance Eq Record

deriving instance Show Record

deriving instance Ord Record

data RecordSplitT f = RecordSplit
  { _rsRecord :: PrimaryKey RecordT f,
    _rsUser :: PrimaryKey UserT f,
    _share :: C f Int16
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
    _admins :: f (TableEntity AdminT),
    _groups :: f (TableEntity GroupT),
    _groupMembers :: f (TableEntity GroupMemberT),
    _receipts :: f (TableEntity ReceiptT),
    _records :: f (TableEntity RecordT),
    _recordSplits :: f (TableEntity RecordSplitT)
  }
  deriving (Generic, Database be)

jizhangDb :: DatabaseSettings be JizhangDb
jizhangDb =
  defaultDbSettings
    `withDbModification` JizhangDb
      { _users =
          setEntityName "users"
            <> modifyTableFields tableModification {_passwordHash = "password_hash"},
        _admins =
          setEntityName "admins"
            <> modifyTableFields tableModification {_adminPasswordHash = "password_hash"},
        _groups =
          setEntityName "groups"
            <> modifyTableFields tableModification {_groupOwner = UserId "owner__id"},
        _groupMembers =
          setEntityName "group_members"
            <> modifyTableFields tableModification {_gmActive = "active"},
        _receipts =
          setEntityName "receipts"
            <> modifyTableFields
              tableModification
                { _receiptUploadedBy = UserId "uploaded_by__id",
                  _receiptCreatedAt = "created_at"
                },
        _records =
          setEntityName "records"
            <> modifyTableFields tableModification {_createdAt = "created_at"},
        _recordSplits = setEntityName "record_splits"
      }
