{-# LANGUAGE OverloadedStrings #-}

-- | Secret material and authenticated encryption for authentication persistence.
-- Callers must supply stable, unambiguous AAD binding table, row ID and purpose;
-- ciphertext cannot be copied between rows or purposes without re-encryption.
module Jizhang.Auth.Security
  ( SecretKey,
    parseSecretKey,
    randomToken,
    tokenDigest,
    secureEqual,
    encryptSecret,
    decryptSecret,
    pkceChallenge,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (AEAD_GCM), AuthTag (AuthTag), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)

-- No Show or serialization instance: the only constructor validates key length.
newtype SecretKey = SecretKey AES256

-- | Parse the persistence-encryption key independently of the JWT signing key.
parseSecretKey :: Text -> Either Text SecretKey
parseSecretKey value = do
  bytes <- either (const $ Left invalidKey) Right $ decodeBase64 value
  if BS.length bytes /= 32
    then Left invalidKey
    else case cipherInit bytes of
      CryptoPassed cipher -> Right $ SecretKey cipher
      CryptoFailed _ -> Left invalidKey
  where
    invalidKey = "OIDC_SECRET_KEY must be canonical standard base64 encoding of exactly 32 bytes"

-- | Independent 256-bit bearer values and PKCE verifiers (43 ASCII characters).
randomToken :: IO Text
randomToken = encodeBase64Url <$> (getRandomBytes 32 :: IO ByteString)

-- | Store a SHA-256 lookup/comparison digest for high-entropy proofs, not passwords.
tokenDigest :: Text -> ByteString
tokenDigest value = BA.convert (hash (encodeUtf8 value) :: Digest SHA256)

-- | Compare proof digests without ordinary byte-by-byte early exit.
secureEqual :: ByteString -> ByteString -> Bool
secureEqual = BA.constEq

-- | Hash the encoded verifier, not its decoded random bytes (RFC 7636).
pkceChallenge :: Text -> Text
pkceChallenge = encodeBase64Url . tokenDigest

-- | Encrypt UTF-8 with AES-256-GCM, a fresh 12-byte nonce and a 16-byte tag.
-- The versioned envelope stores the nonce and tag; callers own the AAD namespace.
encryptSecret :: SecretKey -> ByteString -> Text -> IO Text
encryptSecret key aad plaintext = do
  -- Reusing a GCM nonce under the same key would compromise both secrecy and authentication.
  nonce <- getRandomBytes 12 :: IO ByteString
  case initGcm key nonce of
    Left _ -> ioError $ userError "Unable to initialize authenticated encryption"
    Right aead -> do
      let (tag, ciphertext) = aeadSimpleEncrypt aead aad (encodeUtf8 plaintext) 16
      pure $ T.intercalate "." ["v1", encodeBase64 nonce, encodeBase64 ciphertext, encodeBase64 (BA.convert tag)]

-- | Authenticate the envelope and caller's AAD before decoding plaintext.
-- Wrong keys, tampering and malformed text share the same diagnostic.
decryptSecret :: SecretKey -> ByteString -> Text -> Either Text Text
decryptSecret key aad envelope = do
  (nonce, ciphertext, tag) <- case T.splitOn "." envelope of
    ["v1", nonceText, ciphertextText, tagText] -> (,,) <$> decodeBase64 nonceText <*> decodeBase64 ciphertextText <*> decodeBase64 tagText
    _ -> Left invalidEnvelope
  if BS.length nonce /= 12 || BS.length tag /= 16
    then Left invalidEnvelope
    else do
      aead <- initGcm key nonce
      -- Do not interpret plaintext until the tag authenticates both ciphertext and the caller's row binding.
      plaintext <- maybe (Left invalidEnvelope) Right $ aeadSimpleDecrypt aead aad ciphertext (AuthTag $ BA.convert tag)
      either (const $ Left invalidEnvelope) Right $ decodeUtf8' plaintext

initGcm :: SecretKey -> ByteString -> Either Text (AEAD AES256)
initGcm (SecretKey cipher) nonce = case aeadInit AEAD_GCM cipher nonce of
  CryptoPassed aead -> Right aead
  CryptoFailed _ -> Left invalidEnvelope

invalidEnvelope :: Text
invalidEnvelope = "Unable to decrypt authentication secret; verify OIDC_SECRET_KEY and stored configuration"

encodeBase64 :: ByteString -> Text
encodeBase64 = decodeUtf8 . B64.encode

-- Reject whitespace, URL alphabet, missing/excess padding and noncanonical bits.
decodeBase64 :: Text -> Either Text ByteString
decodeBase64 value = case B64.decode (encodeUtf8 value) of
  Right bytes | encodeBase64 bytes == value -> Right bytes
  _ -> Left invalidEnvelope

encodeBase64Url :: ByteString -> Text
encodeBase64Url = T.dropWhileEnd (== '=') . decodeUtf8 . B64URL.encode
