{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Process-local request budgets, bounded body buffering and concurrent admission.
-- Forwarded addresses influence IP budgets only through configured trusted proxies.
module Jizhang.RequestProtection
  ( RatePolicy (..),
    Principal (..),
    RateKey (..),
    ProtectionFailure (..),
    RequestProtectionConfig (..),
    RequestProtection,
    defaultRequestProtectionConfig,
    validateRequestProtectionConfig,
    newRequestProtection,
    requestProtectionConfig,
    checkProtectionRate,
    resolveClientIp,
    requestProtectionMiddleware,
    protectionFailureStatus,
    protectionFailureHeaders,
    protectionFailureBody,
  )
where

import Control.Concurrent.MVar
import Control.Exception (finally, mask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.IP (IP (..), IPRange (..), fromSockAddr, ipv4ToIPv6, ipv4RangeToIPv6, isMatchedTo)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Word (Word64)
import Network.HTTP.Types (ResponseHeaders, Status, status400, status413, status429, status503)
import Network.Wai
import Text.Read (readMaybe)

-- | Refill rate per minute and maximum immediately spendable token balance.
data RatePolicy = RatePolicy
  { ratePerMinute :: !Int,
    rateBurst :: !Int
  }
  deriving (Eq, Show)

-- | Keep user and administrator budgets separate even when their UUIDs coincide.
data Principal = UserPrincipal !UUID | AdminPrincipal !UUID
  deriving (Eq, Ord, Show)

-- | Callers choose the enforcement scope; unknown credentials use opaque byte keys.
-- Principal and credential checks are separate from middleware's IP checks.
data RateKey
  = IpTraffic !IP | IpAuthentication !IP
  | PrincipalTraffic !Principal | PrincipalExpensive !Principal
  | UserCredential !UUID | AdminCredential !UUID
  | UnknownUserCredential !ByteString | UnknownAdminCredential !ByteString
  deriving (Eq, Ord)

data ProtectionFailure
  = RateLimited !Int | ProtectionBusy | InvalidForwarding | BodyTooLarge
  deriving (Eq, Show)

-- | Operator-owned resource bounds shared by middleware and authenticated handlers.
data RequestProtectionConfig = RequestProtectionConfig
  { protectionIpRate :: !RatePolicy,
    protectionAuthIpRate :: !RatePolicy,
    protectionPrincipalRate :: !RatePolicy,
    protectionExpensiveRate :: !RatePolicy,
    protectionCredentialRate :: !RatePolicy,
    protectionAuthBodyBytes :: !Word64,
    protectionBodyBytes :: !Word64,
    protectionBulkBodyBytes :: !Word64,
    protectionMaxInFlight :: !Int,
    protectionMaxExpensiveInFlight :: !Int,
    protectionMaxKeys :: !Int,
    protectionTrustedProxies :: ![IPRange]
  }
  deriving (Eq, Show)

defaultRequestProtectionConfig :: RequestProtectionConfig
defaultRequestProtectionConfig = RequestProtectionConfig
  (RatePolicy 600 120) (RatePolicy 20 10) (RatePolicy 300 60)
  (RatePolicy 30 10) (RatePolicy 10 5)
  65536 1048576 8388608 32 4 10000 []

-- | Reject invalid individual bounds and incompatible nested resource limits.
validateRequestProtectionConfig :: RequestProtectionConfig -> Either Text ()
validateRequestProtectionConfig cfg = do
  mapM_ policy
    [ ("RATE_LIMIT_IP_PER_MINUTE", protectionIpRate cfg),
      ("RATE_LIMIT_AUTH_PER_IP_PER_MINUTE", protectionAuthIpRate cfg),
      ("RATE_LIMIT_ACCOUNT_PER_MINUTE", protectionPrincipalRate cfg),
      ("RATE_LIMIT_EXPENSIVE_PER_ACCOUNT_PER_MINUTE", protectionExpensiveRate cfg),
      ("RATE_LIMIT_CREDENTIAL_PER_MINUTE", protectionCredentialRate cfg)
    ]
  bound "REQUEST_AUTH_BODY_MAX_BYTES" 1048576 (protectionAuthBodyBytes cfg)
  bound "REQUEST_BODY_MAX_BYTES" 16777216 (protectionBodyBytes cfg)
  bound "REQUEST_BULK_BODY_MAX_BYTES" 67108864 (protectionBulkBodyBytes cfg)
  bound "REQUEST_MAX_IN_FLIGHT" 1024 (protectionMaxInFlight cfg)
  bound "REQUEST_MAX_EXPENSIVE_IN_FLIGHT" 64 (protectionMaxExpensiveInFlight cfg)
  bound "RATE_LIMIT_MAX_KEYS" 1000000 (protectionMaxKeys cfg)
  require "REQUEST_AUTH_BODY_MAX_BYTES must not exceed REQUEST_BODY_MAX_BYTES"
    (protectionAuthBodyBytes cfg <= protectionBodyBytes cfg)
  require "REQUEST_BODY_MAX_BYTES must not exceed REQUEST_BULK_BODY_MAX_BYTES"
    (protectionBodyBytes cfg <= protectionBulkBodyBytes cfg)
  require "REQUEST_MAX_EXPENSIVE_IN_FLIGHT must not exceed REQUEST_MAX_IN_FLIGHT"
    (protectionMaxExpensiveInFlight cfg <= protectionMaxInFlight cfg)
  require "TRUSTED_PROXY_CIDRS must contain at most 32 ranges" (length (protectionTrustedProxies cfg) <= 32)
  where
    require message valid = if valid then Right () else Left message
    bound key upper value = require (key <> " is outside its permitted range") (value >= 1 && value <= upper)
    policy (key, RatePolicy rate burst) = do
      bound key 1000000 rate
      require (key <> " has an invalid burst") (burst >= 1 && burst <= rate)

-- Credits use nanosecond-scaled minute units; the deadline marks a fully refilled bucket.
data Bucket = Bucket
  { bucketCredits :: !Word64,
    bucketObserved :: !Word64,
    bucketDeadline :: !Integer
  }

-- The expiry index and bucket map change under one lock; observed time never regresses.
data RateTable = RateTable
  { tableBuckets :: !(M.Map RateKey Bucket),
    tableExpiry :: !(S.Set (Integer, RateKey)),
    tableObserved :: !Word64
  }

-- | State is local to this instance, not shared across server processes or restarts.
-- Rate accounting and total/expensive admission counts have separate locks.
data RequestProtection = RequestProtection
  { requestProtectionConfig :: !RequestProtectionConfig,
    protectionClock :: !(IO Word64),
    protectionTable :: !(MVar RateTable),
    protectionAdmission :: !(MVar (Int, Int))
  }

-- | Validate before allocating state. The supplied clock reports monotonic nanoseconds.
newRequestProtection :: RequestProtectionConfig -> IO Word64 -> IO RequestProtection
newRequestProtection cfg clock = case validateRequestProtectionConfig cfg of
  Left message -> ioError $ userError $ T.unpack message
  Right () -> RequestProtection cfg clock
    <$> newMVar (RateTable M.empty S.empty 0)
    <*> newMVar (0, 0)

creditPerToken :: Word64
creditPerToken = 60000000000

-- | Atomically spend one token, pruning fully replenished buckets first.
-- A full key table rejects new keys rather than evicting another active budget.
checkProtectionRate :: RequestProtection -> RateKey -> IO (Either ProtectionFailure ())
checkProtectionRate runtime key = do
  observed <- protectionClock runtime
  modifyMVar (protectionTable runtime) $ \table -> do
    -- Clamp clock rollback so elapsed-time subtraction cannot underflow.
    let !now = max observed (tableObserved table)
        !pruned = prune now table {tableObserved = now}
        previous = M.lookup key (tableBuckets pruned)
        policy = keyPolicy (requestProtectionConfig runtime) key
        !capacity = fromIntegral (rateBurst policy) * creditPerToken
        !rate = fromIntegral (ratePerMinute policy)
    case previous of
      Nothing | M.size (tableBuckets pruned) >= protectionMaxKeys (requestProtectionConfig runtime) ->
        pure (pruned, Left ProtectionBusy)
      _ -> do
        -- One minute already replenishes any valid burst; bounding elapsed time keeps Word64 products safe.
        let !credits = case previous of
              Nothing -> capacity
              Just old -> min capacity $ bucketCredits old
                + min creditPerToken (now - bucketObserved old) * rate
            !allowed = credits >= creditPerToken
            !remaining = if allowed then credits - creditPerToken else credits
            -- Integer deadlines can extend past the clock's Word64 range; round up to avoid early eviction.
            !deadline = toInteger now + toInteger (ceilingDivide (capacity - remaining) rate)
            !bucket = Bucket remaining now deadline
            -- Replace the old index entry even on rejection: each key must have exactly one expiry.
            !expiry = maybe (tableExpiry pruned)
              (\old -> S.delete (bucketDeadline old, key) (tableExpiry pruned)) previous
            !updated = pruned
              { tableBuckets = M.insert key bucket (tableBuckets pruned),
                tableExpiry = S.insert (deadline, key) expiry
              }
            seconds = ceilingDivide (creditPerToken - credits) (rate * 1000000000)
        pure (updated, if allowed then Right () else Left $ RateLimited $ fromIntegral seconds)
  where
    ceilingDivide numerator denominator = numerator `div` denominator
      + if numerator `mod` denominator == 0 then 0 else 1
    prune now table = case S.minView (tableExpiry table) of
      -- Expiry means fully replenished, so forgetting the bucket grants no extra tokens on its next use.
      Just ((deadline, expired), rest) | deadline <= toInteger now ->
        prune now table {tableBuckets = M.delete expired (tableBuckets table), tableExpiry = rest}
      _ -> table

keyPolicy :: RequestProtectionConfig -> RateKey -> RatePolicy
keyPolicy cfg key = case key of
  IpTraffic _ -> protectionIpRate cfg
  IpAuthentication _ -> protectionAuthIpRate cfg
  PrincipalTraffic _ -> protectionPrincipalRate cfg
  PrincipalExpensive _ -> protectionExpensiveRate cfg
  _ -> protectionCredentialRate cfg

protectionFailureStatus :: ProtectionFailure -> Status
protectionFailureStatus failure = case failure of
  RateLimited _ -> status429
  ProtectionBusy -> status503
  InvalidForwarding -> status400
  BodyTooLarge -> status413

-- | Responses are non-cacheable; retry hints distinguish exhausted tokens from capacity.
protectionFailureHeaders :: ProtectionFailure -> ResponseHeaders
protectionFailureHeaders failure =
  [("Content-Type", "text/plain; charset=utf-8"), ("Cache-Control", "no-store")]
    <> case failure of
      RateLimited seconds -> [("Retry-After", BS.pack $ show $ max 1 seconds)]
      ProtectionBusy -> [("Retry-After", "1")]
      _ -> []

protectionFailureBody :: ProtectionFailure -> LBS.ByteString
protectionFailureBody failure = case failure of
  RateLimited _ -> "Too many requests; try again later"
  ProtectionBusy -> "Server is busy; try again later"
  InvalidForwarding -> "Invalid forwarded client address"
  BodyTooLarge -> "Request body is too large"

-- | Ignore forwarding from untrusted peers. For a trusted peer, walk right-to-left
-- through bounded X-Forwarded-For entries until the first untrusted address.
resolveClientIp :: RequestProtectionConfig -> Request -> Either ProtectionFailure IP
resolveClientIp cfg request = do
  peer <- maybe (Left InvalidForwarding) (Right . fst) $ fromSockAddr $ remoteHost request
  if not (trusted peer) then Right peer else
    case [value | (name, value) <- requestHeaders request, name == "X-Forwarded-For"] of
      [] -> Right peer
      [value] | BS.length value <= 4096 -> do
        let elements = BS.split ',' value
        if null elements || length elements > 16 then Left InvalidForwarding else do
          addresses <- traverse parseAddress elements
          pure $ walk peer (reverse addresses)
      _ -> Left InvalidForwarding
  where
    trusted address = any (matches address) (protectionTrustedProxies cfg)
    matches (IPv4 address) (IPv4Range range) = isMatchedTo address range
    matches (IPv6 address) (IPv4Range range) = isMatchedTo address (ipv4RangeToIPv6 range)
    matches (IPv4 address) (IPv6Range range) = isMatchedTo (ipv4ToIPv6 address) range
    matches (IPv6 address) (IPv6Range range) = isMatchedTo address range
    walk current (next : rest) | trusted current = walk next rest
    walk current _ = current
    parseAddress bytes =
      let value = BS.dropWhileEnd asciiSpace $ BS.dropWhile asciiSpace bytes
       in if BS.null value || not (BS.all (\c -> c `elem` ("0123456789abcdefABCDEF:." :: String)) value)
            then Left InvalidForwarding
            else maybe (Left InvalidForwarding) Right $ readMaybe $ BS.unpack value
    asciiSpace c = c == ' ' || (c >= '\t' && c <= '\r')

data RequestClass = RequestClass
  { classAuth :: !Bool,
    classExpensive :: !Bool,
    classBulk :: !Bool
  }

-- Route classification also chooses body caps; every auth or bulk request is expensive.
classifyRequest :: Request -> RequestClass
classifyRequest request = RequestClass auth (auth || expensive || bulk) bulk
  where
    method = requestMethod request
    path = pathInfo request
    auth = case path of
      "auth" : _ -> not (method == "GET" && path == ["auth", "options"])
      ["admin", "auth", "login"] -> method == "POST"
      ["admin", "auth", "session", "login"] -> method == "POST"
      ["users", "me", "reauthenticate"] -> method == "POST"
      ["users", "me", "password"] -> method == "PUT"
      ["users", "me", "oidc", "start"] -> method == "POST"
      _ -> False
    groupPath = case path of
      "groups" : _gid : rest -> Just rest
      "admin" : "groups" : _gid : rest -> Just rest
      _ -> Nothing
    bulk = case groupPath of
      Just ["import"] -> method == "POST"
      Just ["receipts"] -> method == "POST"
      Just ["receipts", _rid] -> method == "PUT"
      _ -> False
    expensive = case groupPath of
      Just ["report"] -> method == "GET"
      Just ["statistics"] -> method == "GET"
      Just ["exchange-quotes"] -> method == "POST"
      _ -> case path of
        ["admin", "users"] -> method == "POST"
        ["admin", "admins"] -> method == "POST"
        ["admin", "authentication"] -> method == "PUT"
        ["admin", "authentication", "test"] -> method == "POST"
        _ -> False

-- | Apply IP budgets before admission, then buffer a bounded body before dispatch.
-- Bodyless currency reads bypass budgets/admission after client-address validation.
requestProtectionMiddleware :: RequestProtection -> Middleware
requestProtectionMiddleware runtime application request respond =
  case resolveClientIp cfg request of
    Left failure -> reject failure
    Right ip
      | requestMethod request == "GET", pathInfo request == ["currencies"],
        KnownLength 0 <- requestBodyLength request -> application request respond
      | otherwise -> do
          general <- checkProtectionRate runtime (IpTraffic ip)
          result <- case general of
            Right () | classAuth classification -> checkProtectionRate runtime (IpAuthentication ip)
            _ -> pure general
          case result of
            Left failure -> reject failure
            Right () -> case requestBodyLength request of
              KnownLength bytes | bytes > cap -> reject BodyTooLarge
              _ -> mask $ \restore -> do
                -- Admission covers upload buffering too, bounding memory held by concurrent slow clients.
                -- Mask admission until its release handler is installed.
                admitted <- modifyMVar (protectionAdmission runtime) $ \counts@(total, expensive) ->
                  if total >= protectionMaxInFlight cfg
                    || (costly && expensive >= protectionMaxExpensiveInFlight cfg)
                    then pure (counts, False)
                    else let !nextTotal = total + 1
                             !nextExpensive = expensive + if costly then 1 else 0
                          in pure ((nextTotal, nextExpensive), True)
                if not admitted then restore (reject ProtectionBusy) else
                  -- Hold admission through body reads and the application's response callback.
                  -- WAI's response callback owns streaming; finally also releases on disconnect or exceptions.
                  restore (bufferAndRun [] 0) `finally` release
  where
    cfg = requestProtectionConfig runtime
    classification = classifyRequest request
    costly = classExpensive classification
    cap | classAuth classification = protectionAuthBodyBytes cfg
        | classBulk classification = protectionBulkBodyBytes cfg
        | otherwise = protectionBodyBytes cfg
    reject failure = respond $ responseLBS (protectionFailureStatus failure)
      (protectionFailureHeaders failure) (protectionFailureBody failure)
    release = modifyMVar_ (protectionAdmission runtime) $ \(total, expensive) ->
      let !nextTotal = total - 1
          !nextExpensive = expensive - if costly then 1 else 0
       in pure (nextTotal, nextExpensive)
    bufferAndRun chunks !bytes = do
      chunk <- getRequestBodyChunk request
      if BS.null chunk then do
        -- Dispatch only after EOF, so an oversized chunked upload cannot trigger handler side effects.
        replay <- newIORef (reverse chunks)
        let readChunk = atomicModifyIORef' replay $ \remaining -> case remaining of
              [] -> ([], BS.empty)
              next : rest -> (rest, next)
        application (setRequestBodyChunks readChunk request) {requestBodyLength = KnownLength bytes} respond
      -- Subtract from the cap before adding chunk length to avoid overflow in the running byte count.
      else if fromIntegral (BS.length chunk) > cap - bytes then reject BodyTooLarge
      else bufferAndRun (chunk : chunks) (bytes + fromIntegral (BS.length chunk))
