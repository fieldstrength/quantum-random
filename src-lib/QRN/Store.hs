module QRN.Store (

  getStoreFile,
  getStore,
  putStore,
  getStoreBytes,
  putStoreBytes,
  storeSize,

  getMinStoreSize,
  getTargetStoreSize,

  setMinStoreSize,
  setTargetStoreSize,

  restoreDefaults,
  reinitialize,

  addToStore,
  fill,
  refill,
  extract,

  observe,
  observeDefault,
  observeSpins,
  observeBits,
  peek,
  peekAll,
  peekAllDefault,
  peekAllSpins,
  peekAllBits

) where

import Paths_qrn
import QRN.Codec
import QRN.ANU
import QRN.Display
import QRN.Helpers

import Prelude   hiding (readFile, writeFile)
import System.IO (openBinaryFile, IOMode (..), hClose)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Word (Word8)
import Data.ByteString (ByteString, readFile, writeFile, pack, unpack)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Lazy as Lazy (ByteString,readFile,writeFile,pack,unpack,append,
                                               hPut,hGetContents,fromStrict,toStrict)


---- Data/Settings file locations ----

getStoreFile :: IO FilePath
getStoreFile = getDataFileName "qrn_data/qrn_store.bin"

getSettingsFile :: IO FilePath
getSettingsFile = getDataFileName "qrn_data/qrn_settings.json"


---- Store Data retrieval ----

getStore :: IO ByteString
getStore = getStoreFile >>= readFile

putStore :: ByteString -> IO ()
putStore bs = getStoreFile >>= flip writeFile bs

getStoreBytes :: IO [Word8]
getStoreBytes = unpack <$> getStore

putStoreBytes :: [Word8] -> IO ()
putStoreBytes = putStore . pack

storeSize :: IO Int
storeSize = BS.length <$> getStore


---- Settings access and update ----

getSettings' :: IO (Err QSettings)
getSettings' = eitherDecode . Lazy.fromStrict <$> (getSettingsFile >>= readFile)

getSettings :: IO QSettings
getSettings = fromErrWith "Problem reading settings file:" $ getSettings'

putSettings :: QSettings -> IO ()
putSettings qs = getSettingsFile >>= flip writeFile (Lazy.toStrict $ encode qs)

getMinStoreSize :: IO Int
getMinStoreSize = minStoreSize <$> getSettings

getTargetStoreSize :: IO Int
getTargetStoreSize = targetStoreSize <$> getSettings

setMinStoreSize :: Int -> IO ()
setMinStoreSize n = do
  st <- getSettings
  putSettings $ st { minStoreSize = n }

setTargetStoreSize :: Int -> IO ()
setTargetStoreSize n = do
  st <- getSettings
  putSettings $ st { targetStoreSize = n }

restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

reinitialize :: IO ()
reinitialize = restoreDefaults *> fill


---- Data store access and update ----

addToStore :: Int -> IO ()
addToStore n = do
  qrns <- Lazy.pack <$> fetchQRN n
  storefile <- getStoreFile
  h <- openBinaryFile storefile AppendMode
  Lazy.hPut h qrns
  hClose h

refill :: IO ()
refill = getTargetStoreSize >>= fetchQRN >>= putStoreBytes

fill :: IO ()
fill = do
  qs <- getStoreBytes
  size <- storeSize
  targ <- targetStoreSize <$> getSettings
  case (compare size targ) of
       LT -> do anu <- fetchQRN (targ - size)
                putStoreBytes $ qs ++ anu
       _  -> return ()


extract :: Int -> IO [Word8]
extract n = do
  storefile <- getStoreFile
  size <- storeSize
  qs <- getStoreBytes
  st <- getSettings
  case (compare n (size - minStoreSize st)) of
       GT -> do let needed = targetStoreSize st + n - size
                anu <- fetchQRN needed
                let (xs,ys) = splitAt n $ qs ++ anu
                putStoreBytes ys
                return xs
       _  -> do let (xs,ys) = splitAt n qs
                putStoreBytes ys
                return xs


observeDefault :: Int -> IO ()
observeDefault n = extract n >>= showDataColors

observeSpins :: Int -> IO ()
observeSpins n = extract n >>= showSpins

observeBits :: Int -> IO ()
observeBits n = extract n >>= showBits

observe :: DisplayStyle -> Int -> IO ()
observe Default = observeDefault
observe Spins   = observeSpins
observe Bits    = observeBits


peekAllDefault :: IO ()
peekAllDefault = getStoreBytes >>= showDataColors

peekAllSpins :: IO ()
peekAllSpins = getStoreBytes >>= showSpins

peekAllBits :: IO ()
peekAllBits = getStoreBytes >>= showBits

peekAll :: DisplayStyle -> IO ()
peekAll s = getStoreBytes >>= display s


peek :: DisplayStyle -> Int -> IO ()
peek s n = take n <$> getStoreBytes >>= display s
