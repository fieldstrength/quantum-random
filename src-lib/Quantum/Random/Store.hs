-- | This module provides functionality for quantum random number operations involving
--   the local data store and/or the settings file.
--
--   Some functions come in special access-controlled variants, namely those functions in which
--   access permission is only required in particular branches or phases of execution. All other
--   operations involving the local files (which is most of them) are simply wrapped in a
--   'withAccess' to ensure coordinated access.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Store (

-- * Data store operations
-- ** Basic store access

  getStoreFile,
  getStore,
  getStoreBytes,
  storeSize,
  save,

-- ** Store update

  putStore,
  putStoreBytes,
  appendToStore,
  addToStore,
  addSafely,
  fill,
  refill,
  load,
  clearStore,

-- ** Primary store access

  extract,
  extractSafely,

-- ** Store data display

  observe,
  observeSafely,
  peek,
  peekAll,

-- * Settings file operations
-- ** Settings access

  getMinStoreSize,
  getTargetStoreSize,

-- ** Settings update

  setMinStoreSize,
  setTargetStoreSize,
  restoreDefaults,
  reinitialize,

  -- * Store access control

  AccessControl,
  initAccessControl,
  withAccess,
  forkSafely,
  exitSafely

) where

import Paths_quantum_random_numbers
import Quantum.Random.Codec
import Quantum.Random.ANU
import Quantum.Random.Display
import Quantum.Random.Exceptions
import Quantum.Random.Mutex

import System.IO          (openBinaryFile, IOMode (..), hClose)
import System.Directory   (doesFileExist)
import Data.Aeson         (encode)
import Data.Word          (Word8)
import Data.ByteString    (ByteString, readFile, writeFile, pack, unpack, hPut, length)
import qualified Data.ByteString.Lazy as Lazy
                          (fromStrict, toStrict)
import Prelude     hiding (readFile, writeFile, length)


---- Data/Settings file locations ----

-- | Get path of local store file set up by cabal on installation.
getStoreFile :: IO FilePath
getStoreFile = getDataFileName "qrn_data/qrn_store.bin"

-- | Get path of local settings file set up by cabal on installation.
getSettingsFile :: IO FilePath
getSettingsFile = getDataFileName "qrn_data/qrn_settings.json"


---- Settings access and update ----

getSettings :: IO QSettings
getSettings = throwLeft . fmap (parseSettings . Lazy.fromStrict) $ readFile =<< getSettingsFile

putSettings :: QSettings -> IO ()
putSettings qs = do
  file <- getSettingsFile
  writeFile file . Lazy.toStrict . encode $ qs

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
setTargetStoreSize :: Int -> IO ()
setTargetStoreSize n = updateTarSize n <$> getSettings >>= putSettings

-- | Restore default settings.
restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

-- | Restore default settings and fill up the store.
reinitialize :: IO ()
reinitialize = restoreDefaults *> refill


---- Basic store access ----

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
storeSize = length <$> getStore

-- | Save the data store to another file, specified by the provided path.
--   Asks for overwrite confirmation if the file already exists.
save :: String -> IO ()
save path = do
  exists <- doesFileExist path
  qs <- getStore
  case exists of
       False -> writeFile path qs
       True  -> do putStrLn "File already exists. Enter 'yes' to overwrite."
                   i <- getLine
                   case i of
                        "yes" -> writeFile path qs *> putStrLn "Data saved."
                        _     -> putStrLn "Save aborted."


---- Store update ----

-- | Remove all data from the store.
clearStore :: IO ()
clearStore = putStore mempty

-- | Append the supplied bytestring to the store file.
appendToStore :: ByteString -> IO ()
appendToStore bs = do
  storefile <- getStoreFile
  h <- openBinaryFile storefile AppendMode
  hPut h bs
  hClose h

-- | Retrieve the specified number of QRN bytes and add them to the store.
addToStore :: Int -> IO ()
addToStore n = do
  bs <- pack <$> fetchQRN n
  appendToStore bs

-- | Like `addToStore`, but uses 'AccessControl' to ensure that file writing doesn't interfere with
--   other operations.
addSafely :: AccessControl -> Int -> IO ()
addSafely acc n = do
  bs <- pack <$> fetchQRN n
  withAccess acc (appendToStore bs)

-- | Load binary data from specified file path, append it to the data store.
load :: String -> IO ()
load path = do
  exists <- doesFileExist path
  case exists of
       False -> putStrLn "Load failed. File does not exist."
       True  -> readFile path >>= appendToStore

-- | Calculate the amount of data needed to reach target store size and retrieve it from ANU.
fill :: IO ()
fill = do
  targ <- targetStoreSize <$> getSettings
  qs <- getStoreBytes
  size <- storeSize
  case (compare size targ) of
       LT -> do anu <- fetchQRN (targ - size)
                putStoreBytes $ qs ++ anu
       _  -> pure ()

-- | Refill data store to target size, discarding data already present.
refill :: IO ()
refill = getTargetStoreSize >>= fetchQRN >>= putStoreBytes


---- Primary store access, including update/display ----

-- | Get the specified number of QRN bytes, either from the store and/or by
--   obtaining more from ANU as needed. As the name implies, the obtained bytes
--   are removed from the store. If the store is left with fewer than the
--   minimum number of QRN bytes it is filled back to the target size.
extract :: Int -> IO [Word8]
extract n = do
  size <- storeSize
  qs <- getStoreBytes
  st <- getSettings
  let delta = size - minStoreSize st
  let needed = targetStoreSize st + n - size
  case (compare n delta) of
       GT -> do anu <- fetchQRN needed
                let (xs,ys) = splitAt n $ qs ++ anu
                putStoreBytes ys
                pure xs
       _  -> do let (xs,ys) = splitAt n qs
                putStoreBytes ys
                pure xs

-- | Access-controlled version of 'extract'.
extractSafely :: AccessControl -> Int -> IO [Word8]
extractSafely acc n = do
  size <- storeSize
  qs <- getStoreBytes
  st <- getSettings
  let delta = size - minStoreSize st         -- "current margin"
  let needed = targetStoreSize st + n - size -- "what you want minus what you have"
  case (compare n delta, compare n size) of
       (GT,GT) -> do anu <- fetchQRN needed  -- if n > margin then must req ANU
                     let (xs,ys) = splitAt n $ qs ++ anu
                     withAccess acc $ putStoreBytes ys
                     pure xs
       (GT,_) -> do forkSafely acc $ addSafely acc needed
                    let (xs,ys) = splitAt n qs
                    withAccess acc $ putStoreBytes ys
                    pure xs
       _  -> do let (xs,ys) = splitAt n qs
                withAccess acc $ putStoreBytes ys
                pure xs

-- | Destructively view the specified number of bytes, via 'extract'.
--   The name connotes the irreversibility of quantum measurement.
--   Measuring quantum data (analogously, viewing or using) expends them as a randomness resource.
--   Thus they are discarded. Use 'peek' if instead you wish the data to be kept.
observe :: DisplayStyle -> Int -> IO ()
observe s n = extract n >>= display s

-- | Destructively view the specified number of bytes, via 'extractSafely'.
--   Access-controlled version of 'observe'.
observeSafely :: AccessControl -> DisplayStyle -> Int -> IO ()
observeSafely a s n = extractSafely a n >>= display s

-- | Non-destructively view the specified number of bytes.
peek :: DisplayStyle -> Int -> IO ()
peek s n = take n <$> getStoreBytes >>= display s

-- | Non-destructively view all data in the store.
peekAll :: DisplayStyle -> IO ()
peekAll s = getStoreBytes >>= display s
