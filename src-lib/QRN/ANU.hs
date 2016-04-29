module QRN.ANU (
  fetchQRN,
  fetchQRN',
  fetchQRNBits,
  fetchQRNBits',
) where

import QRN.Codec
import QRN.Monad

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
fetchQResponse n = ExceptT $ parseResponse . process <$> getANU n

fetchQRNInts :: Int -> ErrorM [Int]
fetchQRNInts n = qdata <$> fetchQResponse n

fetchQRN :: Int -> ErrorM [Word8]
fetchQRN n = map fromIntegral <$> fetchQRNInts n

w8bools :: Word8 -> [Bool]
w8bools w = testBit w <$> [0..7]

fetchQRNBits :: Int -> ErrorM [Bool]
fetchQRNBits n = concat . map w8bools <$> fetchQRN n


anuErrMsg = "Problem interpreting response from ANU. No data obtained."

fetchQRN' :: Int -> IO [Word8]
fetchQRN' = handleCrash . fetchQRN

fetchQRNBits' :: Int -> IO [Bool]
fetchQRNBits' = handleCrash . fetchQRNBits
