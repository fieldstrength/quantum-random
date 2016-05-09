-- | This module provides functionality for quantum random number operations involving
--   the local data store and/or the settings file.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Store (

-- * Data store operations
-- ** Store access

  getStoreFile,
  getStore,
  getStoreBytes,
  storeSize,

-- ** Store update

  putStore,
  putStoreBytes,
  addToStore,
  fill,
  refill,

-- ** Store data retrieval

  extract,

-- ** Store data display

  observe,
  peek,
  peekAll,

-- * Settings file operations
-- ** Settings access

  getMinStoreSize,
  getTargetStoreSize,

-- ** Settings update

  setMinStoreSize,
  setTarStoreSize,
  restoreDefaults,

  reinitialize,

) where

import Paths_qrn
import Quantum.Random.Codec
import Quantum.Random.ANU
import Quantum.Random.Display
import Quantum.Random.Exceptions

import Prelude hiding (readFile, writeFile)
import System.IO (openBinaryFile, IOMode (..), hClose)
import Data.Aeson (encode)
import Data.Word (Word8)
import Data.ByteString (ByteString, readFile, writeFile, pack, unpack)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Lazy as Lazy (pack,hPut,fromStrict,toStrict)


---- Data/Settings file locations ----

-- | Get path of local store file set up by cabal on installation.
getStoreFile :: IO FilePath
getStoreFile = getDataFileName "qrn_data/qrn_store.bin"

-- | Get path of local settings file set up by cabal on installation.
getSettingsFile :: IO FilePath
getSettingsFile = getDataFileName "qrn_data/qrn_settings.json"


---- Store Data retrieval ----

-- | Retrieve quantum random data from local store as a raw bytestring.
getStore :: IO ByteString
getStore = getStoreFile >>= readFile

-- | Insert data into local store as a raw bytestring, overwriting any current contents.
putStore :: ByteString -> IO ()
putStore bs = getStoreFile >>= flip writeFile bs

-- | Retrieve quantum random data from local store as a list of bytes.
getStoreBytes :: IO [Word8]
getStoreBytes = unpack <$> getStore

-- | Insert data into local store as a list of bytes, overwriting any current contents.
putStoreBytes :: [Word8] -> IO ()
putStoreBytes = putStore . pack

-- | Compute the size of the current data store.
storeSize :: IO Int
storeSize = BS.length <$> getStore


---- Settings access and update ----

getSettings :: IO QSettings
getSettings = throwLeft $
  parseSettings . Lazy.fromStrict <$>
  (getSettingsFile >>= readFile)

putSettings :: QSettings -> IO ()
putSettings qs = getSettingsFile >>= flip writeFile (Lazy.toStrict $ encode qs)

-- | Query the settings file for the minimum store size setting.
getMinStoreSize :: IO Int
getMinStoreSize = minStoreSize <$> getSettings

-- | Query the settings file for the target store size setting.
getTargetStoreSize :: IO Int
getTargetStoreSize = targetStoreSize <$> getSettings

-- | Update the minimum store size setting in the settings file.
setMinStoreSize :: Int -> IO ()
setMinStoreSize n = updateMinSize n <$> getSettings >>= putSettings

-- | Update the target store size setting in the settings file.
setTarStoreSize :: Int -> IO ()
setTarStoreSize n = updateTarSize n <$> getSettings >>= putSettings

-- | Restore default settings.
restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

-- | Restore default settings and fill up the store.
reinitialize :: IO ()
reinitialize = restoreDefaults *> fill


---- Data store access and update ----

-- | Retrive the specified number of QRN bytes and add them to the store.
addToStore :: Int -> IO ()
addToStore n = do
  qrns <- Lazy.pack <$> fetchQRN n
  storefile <- getStoreFile
  h <- openBinaryFile storefile AppendMode
  Lazy.hPut h qrns
  hClose h

-- | Calculate the amount of data needed to reach target store size and retrieve it from ANU.
fill :: IO ()
fill = do
  targ <- targetStoreSize <$> getSettings
  qs <- getStoreBytes
  size <- storeSize
  case (compare size targ) of
       LT -> do anu <- fetchQRN (targ - size)
                putStoreBytes $ qs ++ anu
       _  -> return ()

-- | Refill data store to target size, discarding data already present.
refill :: IO ()
refill = getTargetStoreSize >>= fetchQRN >>= putStoreBytes

-- | Get the specified number of QRN bytes, either from the store and/or by
--   obtaining more from ANU as needed. As the name implies, the obtained bytes
--   are removed from the store. If the store is left with fewer than the
--   minimum number of QRN bytes it is filled back to the target size.
extract :: Int -> IO [Word8]
extract n = do
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


-- | Destructively view the specified number of bytes, via 'extract'.
--   The name connotes the irreversibility of quantum measurement.
--   Measuring quantum data (analogously, viewing or using) expends them as a randomness resource.
--   Thus they are discarded. Use 'peek' if instead you wish the data to be kept.
observe :: DisplayStyle -> Int -> IO ()
observe s n = extract n >>= display s

-- | Non-destructively view the specified number of bytes.
peek :: DisplayStyle -> Int -> IO ()
peek s n = take n <$> getStoreBytes >>= display s

-- | Non-destructively view all data in the store.
peekAll :: DisplayStyle -> IO ()
peekAll s = getStoreBytes >>= display s
