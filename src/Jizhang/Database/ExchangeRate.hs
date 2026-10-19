{-# LANGUAGE OverloadedStrings #-}

-- | Group-scoped exchange observations. Callers validate rates and own the
-- transaction that keeps quote-context row locks through quote insertion.
module Jizhang.Database.ExchangeRate
  ( insertExchangeQuote,
    getExchangeQuote,
    lockExchangeQuoteContext,
  )
where

import Data.Maybe (listToMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runUpdateReturningList)
import Database.Beam.Postgres
import Jizhang.Database.Schema

-- | Persist one immutable provider observation. There is intentionally no update
-- operation: existing drafts and saved records retain their chosen snapshot.
insertExchangeQuote :: UUID -> Text -> Text -> Scientific -> Day -> UTCTime -> Pg ExchangeQuote
insertExchangeQuote groupId source target rate effectiveDate fetchedAt = do
  quoteId <- liftIO nextRandom
  let quote = ExchangeQuote quoteId (GroupId groupId) source target rate "frankfurter" effectiveDate fetchedAt
  runInsert $ insert (_exchangeQuotes jizhangDb) $ insertValues [quote]
  pure quote

-- | Resolve a quote only within its group; this does not check membership,
-- freshness or compatibility with a record's currencies.
getExchangeQuote :: UUID -> UUID -> Pg (Maybe ExchangeQuote)
getExchangeQuote groupId quoteId = runSelectReturningOne $ select $ do
  quote <- all_ (_exchangeQuotes jizhangDb)
  guard_ (_quoteGroup quote ==. val_ (GroupId groupId) &&. _quoteId quote ==. val_ quoteId)
  pure quote

-- | Hold these rows until quote insertion commits. No-op updates acquire row
-- locks without locking the group's currency policy as a financial write would.
-- The membership row lock also serializes against deactivation/removal.
-- The caller must use Nothing only for an already-authorized operator path;
-- it bypasses the membership check, not the group existence check.
lockExchangeQuoteContext :: UUID -> Maybe UUID -> Pg (Maybe Group, Bool)
lockExchangeQuoteContext groupId actorId = do
  -- A plain read could let policy or membership change while the fetched quote is being committed.
  group <-
    listToMaybe
      <$> runUpdateReturningList
        ( update
            (_groups jizhangDb)
            (\g -> _groupName g <-. current_ (_groupName g))
            (\g -> _groupId g ==. val_ groupId)
        )
  case group of
    Nothing -> pure (Nothing, False)
    Just _ -> do
      authorized <- case actorId of
        Nothing -> pure True
        Just userId ->
          not . null
            <$> runUpdateReturningList
              ( update
                  (_groupMembers jizhangDb)
                  (\gm -> _gmActive gm <-. current_ (_gmActive gm))
                  ( \gm ->
                      _gmGroup gm
                        ==. val_ (GroupId groupId)
                        &&. _gmUser gm
                        ==. val_ (UserId userId)
                        &&. _gmActive gm
                        ==. val_ True
                  )
              )
      pure (group, authorized)
