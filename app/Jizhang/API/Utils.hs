{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jizhang.API.Utils where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Char8 as LBS
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID, toText)
import Jizhang.API.Types
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Servant

-- | Map from user UUID to API User for resolving FK references
type UserMap = M.Map UUID User

textToLBS :: Text -> LazyByteString
textToLBS = LBS.fromStrict . T.encodeUtf8

-- | Ensure a user with the given username exists, or throw 404
ensureUserExists :: Text -> MyHandler ()
ensureUserExists u = do
  exists <- runDB $ D.checkUserExists u
  unless exists $ throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

-- | Look up a user by username, returning the schema User or 404
lookupUser :: Text -> MyHandler S.User
lookupUser u = do
  mUser <- runDB $ D.getUserByUsername u
  case mUser of
    Just user -> pure user
    Nothing -> throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

-- | Look up a user by UUID, returning the schema User or 404
lookupUserById :: UUID -> MyHandler S.User
lookupUserById uid = do
  mUser <- runDB $ D.getUserById uid
  case mUser of
    Just user -> pure user
    Nothing -> throwError $ err404 {errBody = "User with ID " <> textToLBS (toText uid) <> " not found"}

-- | Build a UserMap from members of a specific group (including inactive members)
getGroupUserMap :: UUID -> MyHandler UserMap
getGroupUserMap gId = do
  users <- runDB $ D.getUsersForGroup gId
  pure $ M.fromList [(S._userId u, schemaUserToApiUser u) | u <- users]

-- | Convert a schema User to an API User
schemaUserToApiUser :: S.User -> User
schemaUserToApiUser u = User (coerce $ S._userId u) (S._username u)

-- | Resolve a user UUID using a UserMap
resolveUser :: UserMap -> UUID -> User
resolveUser um uid = um M.! uid

-- | Ensure a group with the given ID exists, or throw 404
ensureGroupExists :: UUID -> MyHandler ()
ensureGroupExists gId = do
  exists <- runDB $ D.checkGroupExists gId
  unless exists $ throwError $ err404 {errBody = "Group with ID " <> textToLBS (toText gId) <> " not found"}

-- | Ensure the authenticated user is the owner of the group, or throw 403
ensureGroupOwner :: UUID -> UUID -> MyHandler ()
ensureGroupOwner userId groupId = do
  isOwner <- runDB $ D.isGroupOwner userId groupId
  unless isOwner $ throwError $ err403 {errBody = "Only the group owner can perform this action"}

-- | Ensure the authenticated user is a member of the group, or throw 403
ensureGroupMember :: UUID -> UUID -> MyHandler ()
ensureGroupMember userId groupId = do
  isMember <- runDB $ D.isUserInGroup userId groupId
  unless isMember $ throwError $ err403 {errBody = "You are not a member of this group"}

validateUsername :: Text -> MyHandler ()
validateUsername u = do
  when (T.null u) $ throwError $ err400 {errBody = "Username cannot be empty"}
  when (T.length u > 50) $ throwError $ err400 {errBody = "Username is too long"}

validateGroupName :: Text -> MyHandler ()
validateGroupName gname = do
  when (T.null gname) $ throwError $ err400 {errBody = "Group name cannot be empty"}
  when (T.length gname > 100) $ throwError $ err400 {errBody = "Group name is too long"}

-- | Ensure a user with the given username is an active member of the group, or throw 400
ensureGroupMemberByUsername :: UUID -> Text -> MyHandler ()
ensureGroupMemberByUsername gId u = do
  mUser <- runDB $ D.getUserByUsername u
  case mUser of
    Nothing -> throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}
    Just user -> do
      isMember <- runDB $ D.isUserInGroup (S._userId user) gId
      unless isMember $
        throwError $ err400 {errBody = "User " <> textToLBS u <> " is not a member of the group"}

validateExpenseRecordRequest :: UUID -> ExpenseRecordRequest -> MyHandler ()
validateExpenseRecordRequest gId ExpenseRecordRequest {..} = do
  when (T.null title) $ throwError $ err400 {errBody = "Title cannot be empty"}
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  ensureGroupMemberByUsername gId (coerce byUsername)
  forM_ splits $ \RecordSplitRequest {..} -> do
    ensureGroupMemberByUsername gId (coerce username :: Text)
    when (share < 0) $
      throwError $
        err400 {errBody = "Share must be non-negative"}

validateTransferRecordRequest :: UUID -> TransferRecordRequest -> MyHandler ()
validateTransferRecordRequest gId TransferRecordRequest {..} = do
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  ensureGroupMemberByUsername gId (coerce byUsername)
  ensureGroupMemberByUsername gId (coerce toUsername)
  when (byUsername == toUsername) $
    throwError $
      err400 {errBody = "Transfer cannot be made to the same user"}

recordToExpenseRecord :: UserMap -> S.Record -> [S.RecordSplit] -> Record
recordToExpenseRecord um record ssplits =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      paidBy = resolveUser um (S.unUserId $ S._paidBy record)
      date = S._date record
      createdAt = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
      totalShares = sum [S._share s | s <- ssplits]
      splits =
        [ RecordSplit
            (resolveUser um (S.unUserId $ S._rsUser s))
            (S._share s)
            (amount * fromIntegral (S._share s) / fromIntegral totalShares)
        | s <- ssplits
        ]
   in ExpenseRecord {..}

recordToTransferRecord :: UserMap -> S.Record -> Record
recordToTransferRecord um record =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      paidBy = resolveUser um (S.unUserId $ S._paidBy record)
      transferTo = resolveUser um (fromJust . S.unUserId $ S._transferTo record)
      date = S._date record
      createdAt = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
   in TransferRecord {..}

ensureRecordExists :: UUID -> MyHandler ()
ensureRecordExists rId = do
  exists <- runDB $ D.checkRecordExists rId
  unless exists $ throwError $ err404 {errBody = "Record with ID " <> textToLBS (toText rId) <> " not found"}

ensureReceiptExists :: UUID -> MyHandler ()
ensureReceiptExists rId = do
  exists <- runDB $ D.getReceipt rId
  case exists of
    Just _ -> pure ()
    Nothing -> throwError $ err404 {errBody = "Receipt with ID " <> textToLBS (toText rId) <> " not found"}

fetchOrFail :: Text -> UUID -> MyHandler (Maybe a) -> MyHandler a
fetchOrFail label uuid action = do
  result <- action
  case result of
    Just x -> pure x
    Nothing -> throwError $ err404 {errBody = textToLBS (label <> " with ID " <> toText uuid <> " not found")}
