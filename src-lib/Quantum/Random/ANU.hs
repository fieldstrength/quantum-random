-- | This module provides functionality for retrieving and parsing the
--   quantum random number data from the Australian National University QRN server.
--
--   This module can be used when one only wants to use live data directly from the server,
--   without using any of the data store functionality.
--
--   In most other cases it should be imported via the "Quantum.Random" module.
module Quantum.Random.ANU (

-- ** QRN data retrieval
  fetchQR,
  fetchQRBits,

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


fetchQRResponse :: Int -> IO QRResponse
fetchQRResponse n = throwLeft $ parseResponse <$> getANU n


-- | Fetch quantum random data from ANU server as a linked list of bytes via HTTPS. Network
--   problems may result in an `HttpException`. An invalid response triggers a 'QRException'.
fetchQR :: Int -> IO [Word8]
fetchQR n = map fromIntegral . qdata <$> fetchQRResponse n

-- | Fetch QRN data from ANU server as a linked list of booleans via HTTPS. Network
--   problems may result in an `HttpException`. An invalid response triggers a 'QRException'.
fetchQRBits :: Int -> IO [Bool]
fetchQRBits n = concat . map w8bools <$> fetchQR n

-- Converts a byte (Word8) to the corresponding list of 8 boolean values.
-- 'Bits' type class indexes bits from least to most significant, thus the reverse
w8bools :: Word8 -> [Bool]
w8bools w = reverse $ testBit w <$> [0..7]
