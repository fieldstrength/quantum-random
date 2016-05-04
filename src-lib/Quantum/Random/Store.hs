-- | This module provides functionality for quantum random number operations involving
--   the local data store and/or the settings file.
--
--   Usually it should be imported via the __Quantum.Random__ module.
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
import Quantum.Random.ErrorM

import Prelude hiding (readFile, writeFile)
import System.IO (openBinaryFile, IOMode (..), hClose)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Word (Word8)
import Data.ByteString (ByteString, readFile, writeFile, pack, unpack)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Lazy as Lazy (ByteString,readFile,writeFile,pack,unpack,append,
                                               hPut,hGetContents,fromStrict,toStrict)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)


---- Data/Settings file locations ----

-- | Get local store file location set up by cabal on installation.
getStoreFile :: IO FilePath
getStoreFile = getDataFileName "qrn_data/qrn_store.bin"

-- | Get local settings file location set up by cabal on installation.
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

getSettings :: ErrorM QSettings
getSettings = ExceptT $ parseSettings . Lazy.fromStrict <$> (getSettingsFile >>= readFile)

putSettings :: QSettings -> IO ()
putSettings qs = getSettingsFile >>= flip writeFile (Lazy.toStrict $ encode qs)

-- | Query the settings file for the minimum store size setting.
getMinStoreSize :: ErrorM Int
getMinStoreSize = minStoreSize <$> getSettings

-- | Query the settings file for the target store size setting.
getTargetStoreSize :: ErrorM Int
getTargetStoreSize = targetStoreSize <$> getSettings

-- | Update the minimum store size setting in the settings file.
setMinStoreSize :: Int -> ErrorM ()
setMinStoreSize n = updateMinSize n <$> getSettings >>= liftIO . putSettings

-- | Update the target store size setting in the settings file.
setTarStoreSize :: Int -> ErrorM ()
setTarStoreSize n = updateTarSize n <$> getSettings >>= liftIO . putSettings

-- | Restore default settings.
restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

-- | Restore default settings and fill up the store.
reinitialize :: ErrorM ()
reinitialize = liftIO restoreDefaults *> fill


---- Data store access and update ----

-- | Retrive the specified number of QRN bytes and add them to the store.
addToStore :: Int -> ErrorM ()
addToStore n = do
  qrns <- Lazy.pack <$> fetchQRNErr n
  liftIO $ do storefile <- getStoreFile
              h <- openBinaryFile storefile AppendMode
              Lazy.hPut h qrns
              hClose h

-- | Calculate the amount of data needed to reach target store size and retrieve it from ANU.
fill :: ErrorM ()
fill = do
  targ <- targetStoreSize <$> getSettings
  qs <- liftIO getStoreBytes
  size <- liftIO storeSize
  case (compare size targ) of
       LT -> do anu <- fetchQRNErr (targ - size)
                liftIO . putStoreBytes $ qs ++ anu
       _  -> return ()

-- | Refill data store to target size, discarding data already present.
refill :: ErrorM ()
refill = getTargetStoreSize >>= fetchQRNErr >>= liftIO . putStoreBytes

-- | Get the specified number of QRN bytes, either from the store and/or by
--   obtaining more from ANU as needed. As the name implies, the obtained bytes
--   are removed from the store. If the store is left with fewer than the
--   minimum number of QRN bytes it is filled back to the target size.
extract :: Int -> ErrorM [Word8]
extract n = do
  storefile <- liftIO getStoreFile
  size <- liftIO storeSize
  qs <- liftIO getStoreBytes
  st <- getSettings
  case (compare n (size - minStoreSize st)) of
       GT -> do let needed = targetStoreSize st + n - size
                anu <- fetchQRNErr needed
                let (xs,ys) = splitAt n $ qs ++ anu
                liftIO $ putStoreBytes ys
                return xs
       _  -> do let (xs,ys) = splitAt n qs
                liftIO $ putStoreBytes ys
                return xs

-- | Destructively view the specified number of bytes, via 'extract'.
--   The name connotes the irreversibility of quantum measurement.
--   Measuring quantum data (analogously, viewing or using) expends them as a randomness resource.
--   Thus they are discarded.
observe :: DisplayStyle -> Int -> ErrorM ()
observe s n = extract n >>= liftIO . display s

-- | Non-destructively view the specified number of bytes.
peek :: DisplayStyle -> Int -> ErrorM ()
peek s n = liftIO $ take n <$> getStoreBytes >>= display s

-- | Non-destructively view all data in the store.
peekAll :: DisplayStyle -> ErrorM ()
peekAll s = liftIO $ getStoreBytes >>= display s
