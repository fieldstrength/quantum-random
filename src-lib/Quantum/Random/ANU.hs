-- | This module provides functionality for retriving and parsing the
--   quantum random number data from the Australian National University QRN server.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.ANU (

-- ** QRN data retrieval
  fetchQRN,
  fetchQRNBits,

) where

import Quantum.Random.Codec
import Quantum.Random.Exceptions

import Data.Word            (Word8)
import Data.Bits            (testBit)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)

-- Returns numbers between 0-255 (8 bits)
anuURL :: Int -> String
anuURL n = "http://qrng.anu.edu.au/API/jsonI.php?length=" ++ show n ++ "&type=uint8"

getANU :: Int -> IO ByteString
getANU = simpleHttp . anuURL


fetchQResponse :: Int -> IO QResponse
fetchQResponse n = throwLeft $ parseResponse <$> getANU n

fetchQRNInts :: Int -> IO [Int]
fetchQRNInts n = qdata <$> fetchQResponse n

-- | Fetch QRN data from ANU server as a linked list of bytes. Problems are handled via the custom
--   exception data type 'QRNException'.
fetchQRN :: Int -> IO [Word8]
fetchQRN n = map fromIntegral <$> fetchQRNInts n

-- | Fetch QRN data from ANU server as a list of booleans. Problems are handled via the custom
--   exception data type 'QRNException'.
fetchQRNBits :: Int -> IO [Bool]
fetchQRNBits n = concat . map w8bools <$> fetchQRN n


-- Converts a byte (Word8) to the corresponding list of 8 boolean values.
-- 'Bits' type class numbers bits from least to most significant, thus the reverse
w8bools :: Word8 -> [Bool]
w8bools w = reverse $ testBit w <$> [0..7]
