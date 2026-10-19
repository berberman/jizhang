-- | Adapt shared rate protection to HTTP handlers, preserving failure status,
-- body and headers. Credential keys avoid retaining unknown plaintext identifiers.
module Jizhang.API.RequestProtection
  ( enforcePrincipalLimit,
    enforceExpensiveLimit,
    enforceUserCredentialLimit,
    enforceAdminCredentialLimit,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Data.UUID (UUID)
import Jizhang.API.App (AppEnv (appRequestProtection), MyHandler)
import Jizhang.Auth.Security (tokenDigest)
import Jizhang.RequestProtection
import Network.HTTP.Types.Status (statusCode, statusMessage)
import qualified Data.ByteString.Char8 as BS
import Servant (ServerError (..), throwError)

-- | Charge ordinary authenticated traffic to its user or operator principal.
enforcePrincipalLimit :: Principal -> MyHandler ()
enforcePrincipalLimit = enforce . PrincipalTraffic

-- | Charge expensive operations separately from ordinary principal traffic.
enforceExpensiveLimit :: Principal -> MyHandler ()
enforceExpensiveLimit = enforce . PrincipalExpensive

-- | Known accounts share a UUID bucket; unknown identifiers use a digest bucket.
enforceUserCredentialLimit :: Maybe UUID -> Text -> MyHandler ()
enforceUserCredentialLimit resolved identifier =
  enforce $ maybe (UnknownUserCredential $ tokenDigest identifier) UserCredential resolved

-- | Keep operator credential buckets separate from end-user credential buckets.
enforceAdminCredentialLimit :: Maybe UUID -> Text -> MyHandler ()
enforceAdminCredentialLimit resolved identifier =
  enforce $ maybe (UnknownAdminCredential $ tokenDigest identifier) AdminCredential resolved

enforce :: RateKey -> MyHandler ()
enforce key = do
  runtime <- asks appRequestProtection
  result <- liftIO $ checkProtectionRate runtime key
  case result of
    Right () -> pure ()
    Left failure -> throwError ServerError
      { errHTTPCode = statusCode $ protectionFailureStatus failure,
        errReasonPhrase = BS.unpack $ statusMessage $ protectionFailureStatus failure,
        errBody = protectionFailureBody failure,
        errHeaders = protectionFailureHeaders failure
      }
