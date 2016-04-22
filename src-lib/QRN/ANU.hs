module QRN.ANU (
  fetchQRN,
  fetchQRN',
  fetchQRNBits,
  fetchQRNBits',
) where

import QRN.Codec
import QRN.Helpers

import Data.Word            (Word8)
import Data.Bits            (testBit)
import Data.Aeson           (decode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)


-- Returns numbers between 0-65535 (16 bits)
anuURL :: Int -> String
anuURL n = "http://qrng.anu.edu.au/API/jsonI.php?length=" ++ show n ++ "&type=uint8"

getANU :: Int -> IO ByteString
getANU = simpleHttp . anuURL

fetchQResponse :: Int -> IO (Err QResponse)
fetchQResponse n = eitherDecode . process <$> getANU n

fetchQRNInts :: Int -> IO (Err [Int])
fetchQRNInts n = qdata <$$> fetchQResponse n

fetchQRN' :: Int -> IO (Err [Word8])
fetchQRN' n = fromIntegral <$$$> fetchQRNInts n

w8bools :: Word8 -> [Bool]
w8bools w = testBit w <$> [0..7]

fetchQRNBits' :: Int -> IO (Err [Bool])
fetchQRNBits' n = concat . map w8bools <$$> fetchQRN' n


anuErrMsg = "Problem interpreting response from ANU. No data obtained."

fetchQRN :: Int -> IO [Word8]
fetchQRN = fromErrWith anuErrMsg . fetchQRN'

fetchQRNBits :: Int -> IO [Bool]
fetchQRNBits = fromErrWith anuErrMsg . fetchQRNBits'
