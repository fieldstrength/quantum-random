-- | This module provides functionality for retriving and parsing the
--   quantum random number data from the Australian National University QRN server.
--
--   Usually to be imported via the __Quantum.Random__ module.
module Quantum.Random.ANU (

-- ** Retrieval within custom error handling context
  fetchQRNErr,
  fetchQRNBitsErr,

-- ** Non-total retrieval within IO
  fetchQRN,
  fetchQRNBits,

-- ** Retrieval within simplified (Either) error-handling context
  fetchQRNEither,
  fetchQRNBitsEither,

-- ** Byte-to-boolean conversion
  w8bools

) where

import Quantum.Random.Codec
import Quantum.Random.ErrorM

import Data.Word            (Word8)
import Data.Bits            (testBit)
import Data.Aeson           (decode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)
import Data.Bifunctor       (first)
import Control.Monad.Except (ExceptT (..), runExceptT)

-- Returns numbers between 0-255 (8 bits)
anuURL :: Int -> String
anuURL n = "http://qrng.anu.edu.au/API/jsonI.php?length=" ++ show n ++ "&type=uint8"

getANU :: Int -> IO ByteString
getANU = simpleHttp . anuURL

fetchQResponse :: Int -> ErrorM QResponse
fetchQResponse n = ExceptT $ parseResponse <$> getANU n

fetchQRNInts :: Int -> ErrorM [Int]
fetchQRNInts n = qdata <$> fetchQResponse n

-- | Fetch QRN data from ANU server as a linked list of bytes within a custom error handling context
--   'ErrorM'. For a pure IO version that crashes upon encountering an error use 'fetchQRN'.
fetchQRNErr :: Int -> ErrorM [Word8]
fetchQRNErr n = map fromIntegral <$> fetchQRNInts n

-- | Fetch QRN data from ANU server as a list of booleans within the custom error handling context
--   'ErrorM'. For a pure IO version that crashes upon encountering an error, use 'fetchQRNBits'.
fetchQRNBitsErr :: Int -> ErrorM [Bool]
fetchQRNBitsErr n = concat . map w8bools <$> fetchQRNErr n


-- | Fetch QRN data from ANU server as a linked list of bytes within a standard IO context.
--   Crashes upon parse error. For a total version that handles possible errors, use 'fetchQRN'.
fetchQRN :: Int -> IO [Word8]
fetchQRN = handleWithCrash . fetchQRNErr

-- | Fetch QRN data from ANU server as a list of booleans within a standard IO context.
--   Crashes upon parse error. For a total version that handles possible errors, use 'fetchQRNBits'.
fetchQRNBits :: Int -> IO [Bool]
fetchQRNBits = handleWithCrash . fetchQRNBitsErr


-- | Fetch QRN data from ANU server as a linked list of bytes within an error handling context.
--   For a pure IO version that crashes upon encountering an error use 'fetchQRN'.
fetchQRNEither :: Int -> IO (Either String [Word8])
fetchQRNEither = fmap (first show) . runExceptT . fetchQRNErr

-- | Fetch QRN data from ANU server as a list of booleans within an error handling context.
--   For a pure IO version that crashes upon encountering an error, use 'fetchQRNBits'.
fetchQRNBitsEither :: Int -> IO (Either String [Bool])
fetchQRNBitsEither = fmap (first show) . runExceptT . fetchQRNBitsErr


-- | Converts a byte (Word8) to the corresponding list of 8 boolean values.
w8bools :: Word8 -> [Bool]
w8bools w = testBit w <$> [0..7]
