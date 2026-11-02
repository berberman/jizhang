{-# LANGUAGE OverloadedStrings #-}

module ReceiptImageTest (receiptImageTests) where

import Codec.Picture (Image, Pixel8, PixelYCbCr8 (..), encodePng, generateImage)
import Codec.Picture.Jpg (encodeJpegAtQuality)
import Control.Exception (AsyncException (..), throw, try)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Jizhang.API.Types.ReceiptAI (ReceiptImageInput (..), maxReceiptImageBytes)
import Jizhang.ReceiptAI.Image
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

receiptImageTests :: TestTree
receiptImageTests =
  testGroup
    "Receipt image validation"
    [ testCase "baseline JPEG retains exact bytes, input, dimensions and SHA-256" $ do
        let input = jpegInput colorJpeg
        result <- validateReceiptImage maxReceiptImageBytes input
        case result of
          Left failure -> assertFailure $ show failure
          Right image -> do
            validatedImageBytes image @?= colorJpeg
            (validatedImageInput image == input) @?= True
            validatedImageWidth image @?= 17
            validatedImageHeight image @?= 13
            validatedImageSha256 image @?= T.pack (show (hash colorJpeg :: Digest SHA256)),
      testCase "one-component baseline JPEG is accepted" $ assertDimensions grayscaleJpeg 1 1,
      testCase "one-component progressive JPEG is accepted" $ assertDimensions progressiveJpeg 1 1,
      testCase "synchronous decode failures disclose no exception text" $
        assertError maxReceiptImageBytes InvalidImage $ throw $ userError "private image failure",
      testCase "asynchronous cancellation propagates" $ do
        result <- try $ validateReceiptImage maxReceiptImageBytes $ throw ThreadKilled
        case result of
          Left ThreadKilled -> pure ()
          Left other -> assertFailure $ "Unexpected asynchronous exception: " <> show other
          Right _ -> assertFailure "Asynchronous cancellation was swallowed",
      testCase "configured byte limit is inclusive" $ do
        assertDimensionsWithLimit (BS.length colorJpeg) colorJpeg 17 13
        assertError (BS.length colorJpeg - 1) TooLarge $ jpegInput colorJpeg,
      testCase "zero and negative configured limits disable attachments" $ do
        assertError 0 TooLarge $ jpegInput colorJpeg
        assertError (-1) TooLarge $ jpegInput colorJpeg,
      testCase "decoded size is checked even when rounded base64 length fits" $
        assertError 1 TooLarge $ jpegInput $ BS.pack [0xff, 0xd8, 0xff],
      testCase "deployment cap cannot raise the hard five MiB limit" $
        assertError maxBound TooLarge $ jpegInput $ BS.replicate (maxReceiptImageBytes + 1) 0,
      testGroup
        "transport is JPEG only"
        [ testCase "PNG MIME is unsupported even with JPEG bytes" $
            assertError maxReceiptImageBytes UnsupportedMedia $ (jpegInput colorJpeg) {receiptImageMediaType = "image/png"},
          testCase "PNG signature with declared JPEG is unsupported" $
            assertError maxReceiptImageBytes UnsupportedMedia $ jpegInput smallPng,
          testCase "wrong signatures are unsupported" $
            mapM_ (assertError maxReceiptImageBytes UnsupportedMedia . jpegInput) ["GIF89a", "RIFF\NUL\NUL\NUL\NULWEBP", "<svg/>", "%PDF-1.7"],
          testCase "MIME matching is exact" $
            mapM_ (\mime -> assertError maxReceiptImageBytes UnsupportedMedia $ (jpegInput colorJpeg) {receiptImageMediaType = mime}) ["image/jpg", "IMAGE/JPEG", "image/jpeg; charset=binary", ""]
        ],
      testGroup
        "canonical bounded base64"
        [ testCase label $ assertError maxReceiptImageBytes InvalidImage $ ReceiptImageInput "image/jpeg" encoded
        | (label, encoded) <-
            [ ("empty payload", ""),
              ("invalid alphabet", "!!!!"),
              ("whitespace", "/9j/ 2Q=="),
              ("URL alphabet", "_9j_2Q=="),
              ("missing padding", "/9j/2Q"),
              ("excess padding", "/9j/2Q==="),
              ("nonzero unused pad bits", "/9j/2R=="),
              ("Unicode is not base64", "图像"),
              ("embedded NUL", "/9j\NUL/2Q==")
            ]
        ],
      testGroup
        "frame bounds before pixel allocation"
        [ testCase label $ assertError maxReceiptImageBytes expected $ jpegInput bytes
        | (label, expected, bytes) <-
            [ ("extended sequential frame", InvalidImage, frameOnly 0xc1 8 1 1 [0x11]),
              ("lossless frame", InvalidImage, frameOnly 0xc3 8 1 1 [0x11]),
              ("arithmetic frame", InvalidImage, frameOnly 0xc9 8 1 1 [0x11]),
              ("12-bit precision", InvalidImage, frameOnly 0xc0 12 1 1 [0x11]),
              ("no components", InvalidImage, frameOnly 0xc0 8 1 1 []),
              ("two components", InvalidImage, frameOnly 0xc0 8 1 1 [0x11, 0x11]),
              ("CMYK components", InvalidImage, frameOnly 0xc0 8 1 1 [0x11, 0x11, 0x11, 0x11]),
              ("zero sampling factor", InvalidImage, frameOnly 0xc0 8 1 1 [0x01]),
              ("oversized sampling factor", InvalidImage, frameOnly 0xc0 8 1 1 [0xf1]),
              ("oversized block count", InvalidImage, frameOnly 0xc0 8 1 1 [0x44, 0x44, 0x44]),
              ("zero width", InvalidImage, frameOnly 0xc0 8 0 1 [0x11]),
              ("zero height", InvalidImage, frameOnly 0xc0 8 1 0 [0x11]),
              ("width exceeds bound", TooLarge, frameOnly 0xc0 8 8193 1 [0x11]),
              ("height exceeds bound", TooLarge, frameOnly 0xc2 8 1 8193 [0x11]),
              ("pixel count exceeds bound", TooLarge, frameOnly 0xc0 8 4001 4000 [0x11]),
              ("65535 squared cannot bypass pixel bound", TooLarge, frameOnly 0xc0 8 65535 65535 [0x11]),
              ("dimension boundary still requires complete image", InvalidImage, frameOnly 0xc0 8 8192 1 [0x11]),
              ("pixel boundary still requires complete image", InvalidImage, frameOnly 0xc0 8 4000 4000 [0x11])
            ]
        ],
      testGroup
        "corrupt JPEG structures"
        [ testCase label $ assertError maxReceiptImageBytes InvalidImage $ jpegInput bytes
        | (label, bytes) <-
            [ ("SOI alone", BS.pack [0xff, 0xd8]),
              ("missing frame", BS.pack [0xff, 0xd8, 0xff, 0xd9]),
              ("unterminated marker fill", BS.pack [0xff, 0xd8, 0xff, 0xff]),
              ("truncated segment length", BS.pack [0xff, 0xd8, 0xff, 0xe0, 0]),
              ("segment length below two", BS.pack [0xff, 0xd8, 0xff, 0xe0, 0, 1]),
              ("segment length exceeds input", BS.pack [0xff, 0xd8, 0xff, 0xe0, 0xff, 0xff]),
              ("truncated frame", BS.pack [0xff, 0xd8] <> segment 0xc0 (BS.pack [8, 0, 1])),
              ("no compressed image", frameOnly 0xc0 8 1 1 [0x11] <> BS.pack [0xff, 0xd9]),
              ("empty entropy scan", grayscalePrefix 0xc0 <> segment 0xda (BS.pack [1, 1, 0, 0, 63, 0]) <> BS.pack [0xff, 0xd9]),
              ("missing EOI", BS.take (BS.length colorJpeg - 2) colorJpeg),
              ("truncated entropy with EOI", BS.take 30 grayscaleJpeg <> BS.pack [0xff, 0xd9]),
              ("repeated frame", frameOnly 0xc0 8 1 1 [0x11] <> BS.drop 2 grayscaleJpeg),
              ("unsupported frame after scan", BS.take (BS.length progressiveJpeg - 2) progressiveJpeg <> BS.drop 2 (frameOnly 0xc3 8 1 1 [0x11]) <> BS.pack [0xff, 0xd9]),
              ("trailing bytes", colorJpeg <> "extra")
            ]
        ]
    ]

assertError :: Int -> ReceiptImageError -> ReceiptImageInput -> Assertion
assertError limit expected input = do
  result <- validateReceiptImage limit input
  either Left (const $ Right ()) result @?= Left expected

assertDimensions :: BS.ByteString -> Int -> Int -> Assertion
assertDimensions = assertDimensionsWithLimit maxReceiptImageBytes

assertDimensionsWithLimit :: Int -> BS.ByteString -> Int -> Int -> Assertion
assertDimensionsWithLimit limit bytes width height = do
  result <- validateReceiptImage limit $ jpegInput bytes
  fmap (\image -> (validatedImageWidth image, validatedImageHeight image)) result @?= Right (width, height)

jpegInput :: BS.ByteString -> ReceiptImageInput
jpegInput = ReceiptImageInput "image/jpeg" . decodeUtf8 . B64.encode

colorJpeg :: BS.ByteString
colorJpeg = LBS.toStrict $ encodeJpegAtQuality 92 $ generateImage (\x y -> PixelYCbCr8 (fromIntegral $ x + y + 100) 128 128) 17 13

frameOnly :: Word8 -> Word8 -> Int -> Int -> [Word8] -> BS.ByteString
frameOnly marker precision width height sampling =
  BS.pack [0xff, 0xd8] <> segment marker (BS.pack ([precision] <> word16 height <> word16 width <> [fromIntegral $ length sampling] <> concat [[identifier, factors, 0] | (identifier, factors) <- zip [1 ..] sampling]))

segment :: Word8 -> BS.ByteString -> BS.ByteString
segment marker payload = BS.pack ([0xff, marker] <> word16 (BS.length payload + 2)) <> payload

word16 :: Int -> [Word8]
word16 value = [fromIntegral $ value `div` 256, fromIntegral value]

-- One zero-coefficient block with single-symbol Huffman tables, split into two scans for SOF2.
grayscaleJpeg, progressiveJpeg :: BS.ByteString
grayscaleJpeg = grayscalePrefix 0xc0 <> segment 0xda (BS.pack [1, 1, 0, 0, 63, 0]) <> BS.pack [0x3f, 0xff, 0xd9]
progressiveJpeg = grayscalePrefix 0xc2 <> segment 0xda (BS.pack [1, 1, 0, 0, 0, 0]) <> BS.pack [0x7f] <> segment 0xda (BS.pack [1, 1, 0, 1, 63, 0]) <> BS.pack [0x7f, 0xff, 0xd9]

grayscalePrefix :: Word8 -> BS.ByteString
grayscalePrefix marker =
  frameOnly marker 8 1 1 [0x11]
    <> segment 0xdb (BS.cons 0 $ BS.replicate 64 1)
    <> segment 0xc4 (huffman 0 <> huffman 0x10)
  where
    huffman table = BS.pack ([table, 1] <> replicate 15 0 <> [0])

smallPng :: BS.ByteString
smallPng = LBS.toStrict $ encodePng (generateImage (\_ _ -> 0) 1 1 :: Image Pixel8)
