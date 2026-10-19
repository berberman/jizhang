{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Record-only query grammar. Parsing never needs a user lookup or ledger data.
module Jizhang.RecordQuery
  ( RecordQuery,
    RecordSort,
    parseRecordQuery,
    matchesRecordQuery,
    parseRecordSort,
    sortRecords,
  )
where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime, defaultTimeLocale, parseTimeM, utctDay)
import qualified Data.UUID as UUID
import Jizhang.API.Types hiding (name, participants, recordType, records, value)
import Jizhang.Currency (CurrencyCatalog, lookupCurrency)
import Jizhang.Money (parseMajorAmount)

-- | Parsed terms combined with AND; constructors stay private to prevent malformed queries.
newtype RecordQuery = RecordQuery [Term]

data Term
  = Bare Text
  | Title Text
  | Note Text
  | Kind Text
  | Identity Role Identity
  | TransactionDate (Comparison Day)
  | CreatedDate (Comparison Day)
  | Currency CurrencyCode
  | Amount (Comparison MinorAmount)

data Role = Payer | Payee | Member

data Identity = Id UserId | Handle Text | Name Text

data Comparison a = Equal a | Below a | AtMost a | Above a | AtLeast a | Between a a

data SortField = ByTitle | ByDate | ByCreated | ByType | ByPayer | ByPayee | ByCurrency | ByAmount

-- | Validated ordering, separate from generic pagination sorts used by other resources.
newtype RecordSort = RecordSort (SortField, Bool)

-- The colon index is recorded before removing quotes, so quoted literal colons
-- can never accidentally become qualifiers. Escapes are special only in quotes.
data Token = Token Text (Maybe Int) Bool

tokens :: Text -> Either Text [Token]
tokens = go . T.unpack
  where
    go input = case dropWhile isSpace input of
      [] -> Right []
      rest -> do
        (token, remaining) <- scan [] Nothing False False rest
        (token :) <$> go remaining
    scan acc colon quoted inQuote []
      | inQuote = Left "unterminated quote"
      | otherwise = Right (Token (T.pack $ reverse acc) colon quoted, [])
    scan acc colon quoted inQuote (c : cs)
      | inQuote = case c of
          '"' -> scan acc colon quoted False cs
          '\\' -> case cs of
            -- Consume the escaped character here so an escaped quote cannot close the quoted value.
            e : es | e == '"' || e == '\\' -> scan (e : acc) colon quoted True es
            _ -> Left "only \\\" and \\\\ escapes are allowed in quotes"
          _ -> scan (c : acc) colon quoted True cs
      | isSpace c = Right (Token (T.pack $ reverse acc) colon quoted, cs)
      | c == '"' = case cs of
          '"' : _ -> Left "empty quoted term or value"
          _ -> scan acc colon True True cs
      | c == ':' && colon == Nothing = scan (c : acc) (Just $ length acc) quoted False cs
      | otherwise = scan (c : acc) colon quoted False cs

-- | Parse quoted terms and qualifiers, rejecting malformed input rather than ignoring it.
-- Amount comparisons require one distinct catalog currency and use its minor-unit precision.
parseRecordQuery :: CurrencyCatalog -> Text -> Either Text RecordQuery
parseRecordQuery catalog raw = first ("Invalid record query: " <>) $ do
  ts <- tokens raw
  -- Resolve currency across the whole AND query: amount precision must not depend on term order.
  -- Repeated identical filters are harmless, but conflicting currencies cannot define one amount scale.
  currencyCodes <- traverse parseCurrency [T.drop (n + 1) text | Token text (Just n) _ <- ts, T.take n text == "currency"]
  let distinct = Set.toList $ Set.fromList currencyCodes
      amountCurrency = case distinct of
        [c] -> lookupCurrency catalog c
        [] -> Left "amount requires exactly one currency filter"
        _ -> Left "amount has conflicting currency filters"
  RecordQuery <$> traverse (parseTerm amountCurrency) ts
  where
    parseCurrency value = do
      let c = CurrencyCode (T.toUpper value)
      _ <- first (const "currency: unknown catalog code") $ lookupCurrency catalog c
      pure c
    parseTerm _ (Token value Nothing _) = nonempty "term" value >> pure (Bare $ T.toCaseFold value)
    parseTerm amountCurrency (Token text (Just n) quoted) = do
      let field = T.take n text
          value = T.drop (n + 1) text
          folded = T.toCaseFold value
      nonempty field value
      first ((field <> ": ") <>) $ case field of
        "title" -> Right $ Title folded
        "note" -> Right $ Note folded
        "type"
          | folded `elem` ["expense", "income", "transfer"] -> Right $ Kind folded
          | otherwise -> Left "expected expense, income or transfer"
        "payer" -> Identity Payer <$> identity quoted value
        "payee" -> Identity Payee <$> identity quoted value
        "member" -> Identity Member <$> identity quoted value
        "date" -> TransactionDate <$> comparison parseDay value
        "created" -> CreatedDate <$> comparison parseDay value
        "currency" -> Currency <$> parseCurrency value
        "amount" -> do
          info <- amountCurrency
          Amount <$> comparison (parseMajorAmount info) value
        _ -> Left "unknown qualifier"
    nonempty field value
      | T.null (T.strip value) = Left $ field <> ": empty value"
      | otherwise = Right ()

identity :: Bool -> Text -> Either Text Identity
identity True value = Right $ Name $ T.toCaseFold value
identity False value = case UUID.fromText value of
  Just uuid -> Right $ Id $ UserId uuid
  Nothing ->
    let handle = maybe value id $ T.stripPrefix "@" value
        initial c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'
        rest c = initial c || c == '.' || c == '-'
     in if T.length handle >= 1 && T.length handle <= 50 && maybe False (initial . fst) (T.uncons handle) && T.all rest handle
          then Right $ Handle $ T.toCaseFold handle
          else Left "expected UUID, handle or quoted display name"

parseDay :: Text -> Either Text Day
parseDay value
  | T.length value /= 10
      || T.index value 4 /= '-'
      || T.index value 7 /= '-'
      || not (T.all (\c -> c >= '0' && c <= '9') (T.take 4 value <> T.take 2 (T.drop 5 value) <> T.drop 8 value)) =
      Left "expected real YYYY-MM-DD date"
  | otherwise =
      maybe (Left "expected real YYYY-MM-DD date") Right $
        parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack value)

comparison :: (Ord a) => (Text -> Either Text a) -> Text -> Either Text (Comparison a)
comparison parse value = case T.splitOn ".." value of
  [lo, hi] -> do
    lower <- parse lo
    upper <- parse hi
    if lower <= upper then Right $ Between lower upper else Left "range endpoints are reversed"
  [_] -> case [(prefix, constructor) | (prefix, constructor) <- [("<=", AtMost), (">=", AtLeast), ("<", Below), (">", Above), ("=", Equal)], prefix `T.isPrefixOf` value] of
    -- Longer operators must win; treating <= as < would leave an invalid '=' in the operand.
    (prefix, constructor) : _ -> constructor <$> parse (T.drop (T.length prefix) value)
    [] -> Equal <$> parse value
  _ -> Left "expected one inclusive lower..upper range"

satisfies :: (Ord a) => Comparison a -> a -> Bool
satisfies (Equal x) y = y == x
satisfies (Below x) y = y < x
satisfies (AtMost x) y = y <= x
satisfies (Above x) y = y > x
satisfies (AtLeast x) y = y >= x
satisfies (Between lo hi) y = y >= lo && y <= hi

recordType :: Record -> Text
recordType ExpenseRecord {} = "expense"
recordType IncomeRecord {} = "income"
recordType TransferRecord {} = "transfer"

payer :: Record -> Maybe User
payer ExpenseRecord {paidBy = u} = Just u
payer TransferRecord {paidBy = u} = Just u
payer _ = Nothing

payee :: Record -> Maybe User
payee IncomeRecord {receivedBy = u} = Just u
payee TransferRecord {transferTo = u} = Just u
payee _ = Nothing

participants :: Record -> [User]
participants r =
  maybeToList (payer r) ++ maybeToList (payee r) ++ case r of
    ExpenseRecord {splits = ss} -> [u | RecordSplit u _ _ _ <- ss]
    IncomeRecord {splits = ss} -> [u | RecordSplit u _ _ _ <- ss]
    _ -> []

-- | Match public identities and literal record text; created dates use the UTC day.
-- Currency and amount terms refer to original values, never converted settlement values.
matchesRecordQuery :: RecordQuery -> Record -> Bool
matchesRecordQuery (RecordQuery terms) r = all matches terms
  where
    matches (Bare text) =
      any (matchesQuery text) $
        r.title : recordType r : maybeToList r.note ++ concat [[handle, name] | User _ handle name <- participants r]
    matches (Title text) = matchesQuery text r.title
    matches (Note text) = maybe False (matchesQuery text) r.note
    matches (Kind text) = recordType r == text
    matches (Identity role ident) = any (matchesIdentity ident) $ case role of
      Payer -> maybeToList $ payer r
      Payee -> maybeToList $ payee r
      Member -> participants r
    matches (TransactionDate cmp) = satisfies cmp r.date
    matches (CreatedDate cmp) = satisfies cmp $ utctDay r.createdAt
    matches (Currency c) = r.currency.code == c
    matches (Amount cmp) = satisfies cmp r.amountMinor
    matchesIdentity (Id wanted) (User actual _ _) = wanted == actual
    matchesIdentity (Handle wanted) (User _ handle _) = wanted == T.toCaseFold handle
    matchesIdentity (Name wanted) (User _ _ name) = wanted == T.toCaseFold name

-- | Accept the record-specific sort fields with an optional descending @-@ prefix.
-- Omission selects descending transaction date; unknown fields are errors.
parseRecordSort :: Maybe Text -> Either Text RecordSort
parseRecordSort input =
  let text = maybe "-date" id input
      (descending, field) = case T.stripPrefix "-" text of
        Just rest -> (True, rest)
        Nothing -> (False, text)
   in case lookup field [("title", ByTitle), ("date", ByDate), ("createdAt", ByCreated), ("type", ByType), ("payer", ByPayer), ("payee", ByPayee), ("currency", ByCurrency), ("amount", ByAmount)] of
        Just key -> Right $ RecordSort (key, descending)
        Nothing -> Left "Invalid record sort"

-- Keys are decorated once, including case folding. Absence and currency grouping
-- are outside the directional comparison, and UUID ties are always ascending.
data Key
  = TextKey Text
  | DayKey Day
  | TimeKey UTCTime
  | IdentityKey (Maybe (Text, Text, UserId))
  | AmountKey CurrencyCode MinorAmount
  deriving (Eq, Ord)

-- | Sort the complete result set before pagination, with UUID-ascending ties in either direction.
-- Missing identities sort last; amount sorting keeps currency groups ascending.
sortRecords :: RecordSort -> [Record] -> [Record]
sortRecords (RecordSort (field, descending)) records =
  [r | (_, _, r) <- sortBy compareRow [(key r, r.recordId, r) | r <- records]]
  where
    key :: Record -> Key
    key r = case field of
      ByTitle -> TextKey $ T.toCaseFold r.title
      ByDate -> DayKey r.date
      ByCreated -> TimeKey r.createdAt
      ByType -> TextKey $ recordType r
      ByPayer -> IdentityKey $ userKey <$> payer r
      ByPayee -> IdentityKey $ userKey <$> payee r
      ByCurrency -> TextKey $ currencyCodeText r.currency.code
      ByAmount -> AmountKey r.currency.code r.amountMinor
    userKey (User uid handle name) = (T.toCaseFold name, T.toCaseFold handle, uid)
    direction a b = if descending then compare b a else compare a b
    primary (IdentityKey Nothing) (IdentityKey Nothing) = EQ
    primary (IdentityKey Nothing) _ = GT
    primary _ (IdentityKey Nothing) = LT
    primary (AmountKey c a) (AmountKey d b) = compare c d <> direction a b
    primary a b = direction a b
    compareRow (a, aid, _) (b, bid, _) = primary a b <> compare aid bid
