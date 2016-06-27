-- | This module provides functionality for quantum random data operations involving
--   the local data store and/or the settings file.
--
--   It also provides a way to coordinate access to these local files.
--   See 'AccessControl' for details. Any IO operation that uses these files can used in a
--   coordinated way by wrapping them in a 'withAccess'.
--
--   Some of these functions already come in special access-controlled variants because they only
--   require access in particular branches or phases of execution.
--   In particular we have 'addSafely', 'extractSafely' and 'observeSafely'.
--
--   Finally, there is functionality to ensure that a forked thread is allowed to finish, in case
--   main would otherwise return too soon. This is primarily needed to provide 'addConcurrently',
--   but it can be re-used by forking a thread with 'forkSafely' and exiting the program with
--   'exitSafely'.
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
  status,

-- ** Store update

  putStore,
  putStoreBytes,
  appendToStore,
  addToStore,
  addSafely,
  addConcurrently,
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
  display_,

-- * Settings file operations
-- ** Settings access

  getMinStoreSize,
  getTargetStoreSize,
  getDefaultStyle,

-- ** Settings update

  setMinStoreSize,
  setTargetStoreSize,
  setDefaultStyle,
  restoreDefaults,
  reinitialize,

  -- * Store access control

  AccessControl,
  initAccessControl,
  withAccess,
  forkSafely,
  exitSafely

) where

import Paths_quantum_random
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
import Control.Concurrent (ThreadId)
import Prelude     hiding (readFile, writeFile, length)


---- Data/Settings file locations ----

-- | Get path of local store file set up by cabal on installation.
getStoreFile :: IO FilePath
getStoreFile = getDataFileName "qr_data/qr_store.bin"

-- | Get path of local settings file set up by cabal on installation.
getSettingsFile :: IO FilePath
getSettingsFile = getDataFileName "qr_data/qr_settings.json"


---- Settings access and update ----

getSettings :: IO QRSettings
getSettings = throwLeft . fmap (parseSettings . Lazy.fromStrict) $ readFile =<< getSettingsFile

putSettings :: QRSettings -> IO ()
putSettings qs = do
  file <- getSettingsFile
  writeFile file . Lazy.toStrict . encode $ qs

-- | Query the settings file for the minimum store size setting.
getMinStoreSize :: IO Int
getMinStoreSize = minStoreSize <$> getSettings

-- | Query the settings file for the target store size setting.
getTargetStoreSize :: IO Int
getTargetStoreSize = targetStoreSize <$> getSettings

-- | Query the settings file for the default display style.
getDefaultStyle :: IO DisplayStyle
getDefaultStyle = defaultDisplayStyle <$> getSettings

-- | Update the minimum store size setting in the settings file.
setMinStoreSize :: Int -> IO ()
setMinStoreSize n = updateMinSize n <$> getSettings >>= putSettings

-- | Update the target store size setting in the settings file.
setTargetStoreSize :: Int -> IO ()
setTargetStoreSize n = updateTarSize n <$> getSettings >>= putSettings

-- | Update the default 'DisplayStyle' setting in the settings file.
setDefaultStyle :: DisplayStyle -> IO ()
setDefaultStyle sty = updateDefaultStyle sty <$> getSettings >>= putSettings

-- | Restore default settings.
restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

-- | Restore default settings and 'refill' the store.
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

-- | Display status information: Current store size, minimum size setting, target
--   size setting, default display style and data file path.
status :: IO ()
status = do
  smin <- getMinStoreSize
  star <- getTargetStoreSize
  sty  <- getDefaultStyle
  siz <- storeSize
  sto <- getStoreFile
  mapM_ putStrLn
    [ "Store contains " ++ bitsNBytes siz ++ " of quantum random data."
    , ""
    , "Minimum store size set to " ++ bitsNBytes smin ++ "."
    , "Target  store size set to " ++ bitsNBytes star ++ "."
    , ""
    , "Default display style: " ++ show sty ++ "."
    , ""
    , "Local data store location:"
    , sto
    , ""
    ]

bitsNBytes :: Int -> String
bitsNBytes n = show n ++ bytes ++ show (n*8) ++ " bits)"
   where bytes = if n /= 1 then " bytes (" else " byte ("


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
  bs <- pack <$> fetchQR n
  appendToStore bs

-- | Like `addToStore`, but uses 'AccessControl' to ensure that file writing doesn't interfere with
--   other operations.
addSafely :: AccessControl -> Int -> IO ()
addSafely acc n = do
  bs <- pack <$> fetchQR n
  withAccess acc (appendToStore bs)

-- | Fork a thread to add data to the store concurrently.
addConcurrently :: AccessControl -> Int -> IO ThreadId
addConcurrently acc n = forkSafely acc $ addSafely acc n

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
       LT -> do anu <- fetchQR (targ - size)
                putStoreBytes $ qs ++ anu
       _  -> pure ()

-- | Refill data store to target size, discarding data already present.
refill :: IO ()
refill = getTargetStoreSize >>= fetchQR >>= putStoreBytes


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
       GT -> do anu <- fetchQR needed
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
       (GT,GT) -> do anu <- fetchQR needed  -- if n > margin then must req ANU
                     let (xs,ys) = splitAt n $ qs ++ anu
                     withAccess acc $ putStoreBytes ys
                     pure xs
       (GT,_) -> do addConcurrently acc needed
                    let (xs,ys) = splitAt n qs
                    withAccess acc $ putStoreBytes ys
                    pure xs
       _  -> do let (xs,ys) = splitAt n qs
                withAccess acc $ putStoreBytes ys
                pure xs

-- | Like 'display' only taking a `Maybe` 'DisplayStyle' as the first argument, where `Nothing`
--   signifies using the default display style.
display_ :: Maybe DisplayStyle -> [Word8] -> IO ()
display_ (Just style) ws = display style ws
display_ Nothing      ws = getDefaultStyle >>= flip display ws

-- | Destructively view the specified number of bytes, via 'extract'.
--   The name connotes the irreversibility of quantum measurement.
--   Measuring quantum data (analogously, viewing or using) expends them as a randomness resource.
--   Thus they are discarded. Use 'peek' if instead you wish the data to be kept.
observe :: Maybe DisplayStyle -> Int -> IO ()
observe ms n = extract n >>= display_ ms

-- | Destructively view the specified number of bytes, via 'extractSafely'.
--   Access-controlled version of 'observe'.
observeSafely :: AccessControl -> Maybe DisplayStyle -> Int -> IO ()
observeSafely a ms n = extractSafely a n >>= display_ ms

-- | Non-destructively view the specified number of bytes.
peek :: Maybe DisplayStyle -> Int -> IO ()
peek ms n = take n <$> getStoreBytes >>= display_ ms

-- | Non-destructively view all data in the store.
peekAll :: Maybe DisplayStyle -> IO ()
peekAll ms = getStoreBytes >>= display_ ms
