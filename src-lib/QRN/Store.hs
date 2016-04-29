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
  setTarStoreSize,

  restoreDefaults,
  reinitialize,

  addToStore,
  fill,
  refill,
  extract,

  observe,
  peek,
  peekAll,

) where

import Paths_qrn
import QRN.Codec
import QRN.ANU
import QRN.Display
import QRN.Monad

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

getSettings :: ErrorM QSettings
getSettings = ExceptT $ parseSettings . Lazy.fromStrict <$> (getSettingsFile >>= readFile)

putSettings :: QSettings -> IO ()
putSettings qs = getSettingsFile >>= flip writeFile (Lazy.toStrict $ encode qs)

getMinStoreSize :: ErrorM Int
getMinStoreSize = minStoreSize <$> getSettings

getTargetStoreSize :: ErrorM Int
getTargetStoreSize = targetStoreSize <$> getSettings


setMinStoreSize :: Int -> ErrorM ()
setMinStoreSize n = updateMinSize n <$> getSettings >>= liftIO . putSettings

setTarStoreSize :: Int -> ErrorM ()
setTarStoreSize n = updateTarSize n <$> getSettings >>= liftIO . putSettings

restoreDefaults :: IO ()
restoreDefaults = putSettings defaults

reinitialize :: ErrorM ()
reinitialize = liftIO restoreDefaults *> fill


---- Data store access and update ----

addToStore :: Int -> ErrorM ()
addToStore n = do
  qrns <- Lazy.pack <$> fetchQRN n
  liftIO $ do storefile <- getStoreFile
              h <- openBinaryFile storefile AppendMode
              Lazy.hPut h qrns
              hClose h

refill :: ErrorM ()
refill = getTargetStoreSize >>= fetchQRN >>= liftIO . putStoreBytes

fill :: ErrorM ()
fill = do
  targ <- targetStoreSize <$> getSettings
  qs <- liftIO getStoreBytes
  size <- liftIO storeSize
  case (compare size targ) of
       LT -> do anu <- fetchQRN (targ - size)
                liftIO . putStoreBytes $ qs ++ anu
       _  -> return ()


extract :: Int -> ErrorM [Word8]
extract n = do
  storefile <- liftIO getStoreFile
  size <- liftIO storeSize
  qs <- liftIO getStoreBytes
  st <- getSettings
  case (compare n (size - minStoreSize st)) of
       GT -> do let needed = targetStoreSize st + n - size
                anu <- fetchQRN needed
                let (xs,ys) = splitAt n $ qs ++ anu
                liftIO $ putStoreBytes ys
                return xs
       _  -> do let (xs,ys) = splitAt n qs
                liftIO $ putStoreBytes ys
                return xs


observe :: DisplayStyle -> Int -> ErrorM ()
observe s n = extract n >>= liftIO . display s

peekAll :: DisplayStyle -> ErrorM ()
peekAll s = liftIO $ getStoreBytes >>= display s

peek :: DisplayStyle -> Int -> ErrorM ()
peek s n = liftIO $ take n <$> getStoreBytes >>= display s
