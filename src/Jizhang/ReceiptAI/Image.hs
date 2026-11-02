{-# LANGUAGE OverloadedStrings #-}

module Jizhang.ReceiptAI.Image
  ( ValidatedReceiptImage,
    ReceiptImageError (..),
    validateReceiptImage,
    validatedImageBytes,
    validatedImageInput,
    validatedImageWidth,
    validatedImageHeight,
    validatedImageSha256,
  )
where

import Codec.Picture (DynamicImage (..), Image (..))
import Codec.Picture.Jpg (decodeJpeg)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, tryJust)
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Jizhang.API.Types.ReceiptAI
  ( ReceiptImageInput (..),
    maxReceiptImageBytes,
    maxReceiptImageDimension,
    maxReceiptImagePixels,
  )
import System.Timeout (timeout)

data ReceiptImageError = TooLarge | UnsupportedMedia | InvalidImage
  deriving (Eq, Show)

data ValidatedReceiptImage = ValidatedReceiptImage
  { validatedImageBytes :: !BS.ByteString,
    validatedImageInput :: !ReceiptImageInput,
    validatedImageWidth :: !Int,
    validatedImageHeight :: !Int,
    validatedImageSha256 :: !Text
  }

validateReceiptImage :: Int -> ReceiptImageInput -> IO (Either ReceiptImageError ValidatedReceiptImage)
validateReceiptImage byteLimit input = do
  result <- tryJust synchronousException $ timeout 3000000 $ evaluate $ validate byteLimit input
  pure $ case result of
    Right (Just validated) -> validated
    _ -> Left InvalidImage
  where
    synchronousException :: SomeException -> Maybe ()
    synchronousException exception = case fromException exception :: Maybe SomeAsyncException of
      Just _ -> Nothing
      Nothing -> Just ()

validate :: Int -> ReceiptImageInput -> Either ReceiptImageError ValidatedReceiptImage
validate byteLimit input = do
  unless (receiptImageMediaType input == "image/jpeg") $ Left UnsupportedMedia
  let limit = max 0 $ min maxReceiptImageBytes byteLimit
      encoded = receiptImageDataBase64 input
      encodedLimit = 4 * ((limit + 2) `div` 3)
  when (limit == 0 || T.compareLength encoded encodedLimit == GT) $ Left TooLarge
  unless (T.all isBase64Character encoded) $ Left InvalidImage
  let encodedBytes = encodeUtf8 encoded
  bytes <- either (const $ Left InvalidImage) Right $ B64.decode encodedBytes
  when (BS.length bytes > limit) $ Left TooLarge
  unless (B64.encode bytes == encodedBytes) $ Left InvalidImage
  when (BS.null bytes) $ Left InvalidImage
  unless (BS.isPrefixOf "\xff\xd8" bytes) $ Left UnsupportedMedia
  (width, height) <- scanJpeg bytes
  decoded <- either (const $ Left InvalidImage) Right $ decodeJpeg bytes
  unless (forceDecoded width height decoded) $ Left InvalidImage
  let validated = ValidatedReceiptImage bytes input width height (T.pack $ show (hash bytes :: Digest SHA256))
  validated `seq` pure validated
  where
    isBase64Character character =
      (character >= 'A' && character <= 'Z')
        || (character >= 'a' && character <= 'z')
        || (character >= '0' && character <= '9')
        || character `elem` ['+', '/', '=']

-- Scan every frame before decoding, including markers between progressive scans.
scanJpeg :: BS.ByteString -> Either ReceiptImageError (Int, Int)
scanJpeg bytes = markers 2 Nothing False
  where
    size = BS.length bytes
    at = BS.index bytes
    word16 offset = fromIntegral (at offset) * 256 + fromIntegral (at $ offset + 1)

    markers offset frame scanned = do
      unless (offset < size && at offset == 0xff) $ Left InvalidImage
      let markerOffset = skipFill (offset + 1)
      unless (markerOffset < size) $ Left InvalidImage
      let marker = at markerOffset
          start = markerOffset + 1
      if marker == 0xd9
        then case frame of
          Just dimensions | scanned && start == size -> Right dimensions
          _ -> Left InvalidImage
        else do
          unless (start + 2 <= size) $ Left InvalidImage
          let segmentLength = word16 start
              payload = start + 2
              end = start + segmentLength
          unless (segmentLength >= 2 && end <= size) $ Left InvalidImage
          case marker of
            0xc0 -> frameSegment payload end frame scanned
            0xc2 -> frameSegment payload end frame scanned
            0xda -> do
              unless (maybe False (const True) frame) $ Left InvalidImage
              unless (payload < end) $ Left InvalidImage
              let components = fromIntegral $ at payload
              unless (components >= 1 && components <= 3 && segmentLength == 6 + 2 * components) $ Left InvalidImage
              entropy end frame False
            _
              | marker `elem` [0xc4, 0xdb, 0xdd, 0xfe] || (marker >= 0xe0 && marker <= 0xef) -> markers end frame scanned
              | otherwise -> Left InvalidImage

    frameSegment payload end frame scanned = do
      unless (frame == Nothing && payload + 6 <= end) $ Left InvalidImage
      let precision = at payload
          height = word16 $ payload + 1
          width = word16 $ payload + 3
          components = fromIntegral $ at $ payload + 5
      unless (precision == 8 && components `elem` [1, 3] && end - payload == 6 + 3 * components) $ Left InvalidImage
      unless (width > 0 && height > 0) $ Left InvalidImage
      when (width > maxReceiptImageDimension || height > maxReceiptImageDimension || width * height > maxReceiptImagePixels) $ Left TooLarge
      let sampling = [at (payload + 7 + 3 * index) | index <- [0 .. components - 1]]
          blocks value = fromIntegral (value `div` 16) * fromIntegral (value `mod` 16) :: Int
      unless (all (\value -> value `div` 16 `elem` [1 .. 4] && value `mod` 16 `elem` [1 .. 4]) sampling && sum (map blocks sampling) <= 10) $ Left InvalidImage
      markers end (Just (width, height)) scanned

    skipFill offset
      | offset < size && at offset == 0xff = skipFill $ offset + 1
      | otherwise = offset

    entropy offset frame hasData
      | offset >= size = Left InvalidImage
      | at offset /= 0xff = entropy (offset + 1) frame True
      | otherwise =
          let next = skipFill $ offset + 1
           in if next >= size
                then Left InvalidImage
                else case at next of
                  0x00 -> entropy (next + 1) frame True
                  marker | marker >= 0xd0 && marker <= 0xd7 -> entropy (next + 1) frame hasData
                  _ -> do
                    unless hasData $ Left InvalidImage
                    markers offset frame True

-- Storable vectors are strict, but touching every pixel also forces deferred decode work.
forceDecoded :: Int -> Int -> DynamicImage -> Bool
forceDecoded width height dynamic = case dynamic of
  ImageY8 (Image actualWidth actualHeight pixels) -> forcePixels 1 actualWidth actualHeight pixels
  ImageRGB8 (Image actualWidth actualHeight pixels) -> forcePixels 3 actualWidth actualHeight pixels
  ImageYCbCr8 (Image actualWidth actualHeight pixels) -> forcePixels 3 actualWidth actualHeight pixels
  _ -> False
  where
    forcePixels components actualWidth actualHeight pixels =
      VS.foldl' (.|.) (0 :: Word8) pixels `seq`
        ( actualWidth == width
            && actualHeight == height
            && VS.length pixels == width * height * components
        )
