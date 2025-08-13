{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MyUUID where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Backend

newtype MyUUID = MyUUID {getMyUUID :: UUID}
  deriving (Show, Eq, Ord)

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be MyUUID where
  fromBackendRow = MyUUID . fromJust . fromText <$> fromBackendRow

instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be MyUUID where
  sqlValueSyntax (MyUUID uuid) = sqlValueSyntax (toText uuid)

instance (BeamSqlBackend be) => HasSqlEqualityCheck be MyUUID

randomMyUUID :: IO MyUUID
randomMyUUID = MyUUID <$> nextRandom
