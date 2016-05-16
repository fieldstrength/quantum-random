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


anuURL :: Int -> String
anuURL n = "https://qrng.anu.edu.au/API/jsonI.php?length=" ++ show n ++ "&type=uint8"

getANU :: Int -> IO ByteString
getANU = simpleHttp . anuURL


fetchQResponse :: Int -> IO QResponse
fetchQResponse n = throwLeft $ parseResponse <$> getANU n


-- | Fetch QRN data from ANU server as a linked list of bytes via HTTPS. Problems are handled via
--   the custom exception data type 'QRNException'.
fetchQRN :: Int -> IO [Word8]
fetchQRN n = map fromIntegral . qdata <$> fetchQResponse n

-- | Fetch QRN data from ANU server as a linked list of booleans via HTTPS. Problems are handled
--   via the custom exception data type 'QRNException'.
fetchQRNBits :: Int -> IO [Bool]
fetchQRNBits n = concat . map w8bools <$> fetchQRN n

-- Converts a byte (Word8) to the corresponding list of 8 boolean values.
-- 'Bits' type class indexes bits from least to most significant, thus the reverse
w8bools :: Word8 -> [Bool]
w8bools w = reverse $ testBit w <$> [0..7]
