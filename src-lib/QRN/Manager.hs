{-# LANGUAGE ViewPatterns #-}

module QRN.Manager (main) where

import QRN
import QRN.Monad

import Prelude hiding (writeFile)
import Data.Char (isDigit, toLower)
import Data.Maybe (isJust)
import Control.Monad.Except (ExceptT,liftIO,lift)
import System.Console.Haskeline (InputT, getInputLine, runInputT, defaultSettings, outputStrLn)
import Data.ByteString (writeFile)
import System.Directory (doesFileExist)
import System.Environment (getArgs)


---- Structure of commands ----

data Setting = MinSize | TargetSize

data Command = Add Int
             | Observe Int DisplayStyle
             | Peek Int DisplayStyle
             | PeekAll DisplayStyle
             | Live Int DisplayStyle
             | Fill
             | RestoreDefaults
             | Reinitialize
             | Status
             | Save String
             | Set Setting Int
             | Help
             | Quit

helpMsg = unlines
  [ ""
  , "======= Available commands ======="
  , ""
  , "add [# bytes]     –  Request specified number of QRN bytes from ANU and add them to the store"
  , "live [# bytes]    –  Request specified number of QRN bytes from ANU and display them directly"
  , "observe [# bytes] –  Take and display QRN data from store, retrieving more if needed. Those taken from the store are removed"
  , "peek [# bytes]    –  Display up to the specified number of bytes from the store without removing them"
  , "peekAll           –  Display all data from the store without removing them"
  , "fill              –  Fill the store to the target size with live ANU quantum random numbers"
  , "restoreDefaults   –  Restore default settings"
  , "reinitialize      –  Restore default settings, and refill store to target size"
  , "status            –  Display status of store and settings"
  , "save [filepath]   –  save binary qrn file to specified file path"
  , "set minSize       –  Set the number of bytes below which the store is refilled"
  , "set targetSize    –  Set the number of bytes to have after refilling"
  , "help/?            –  Display this text"
  , "quit              –  Quit"
  , ""
  , "======= Display options ======="
  , ""
  , "Commands that display QRN data can take an optional display style modifier: "
  , "'colors' (the default), 'spins', 'bits', 'colorSpins' or 'colorBits'."
  , ""
  , "Examples:"
  , "\"observe 25 colorspins\""
  , "\"live 50 bits\""
  ]

---- Parsing commands ----

readDigit :: Char -> Maybe Int
readDigit c = if isDigit c then Just (read [c]) else Nothing

readInt :: String -> Maybe Int
readInt = readIntRev . reverse
   where readIntRev :: String -> Maybe Int
         readIntRev []       = Nothing
         readIntRev [c]      = readDigit c
         readIntRev (c:x:xs) = fmap (+) (readDigit c) <*> fmap (*10) (readIntRev (x:xs))


cwords :: String -> [String]
cwords = words . map toLower

readCommand :: String -> Maybe Command
readCommand (cwords -> ["add",n])              = Add <$> readInt n
readCommand (cwords -> ["peekall"])            = Just (PeekAll Default)
readCommand (cwords -> ["peekall",s])          = PeekAll <$> parseStyle s
readCommand (cwords -> ["peek","all"])         = Just (PeekAll Default)
readCommand (cwords -> ["peek","all",s])       = PeekAll <$> parseStyle s
readCommand (cwords -> ["observe",n])          = Observe <$> readInt n <*> Just Default
readCommand (cwords -> ["observe",n,s])        = Observe <$> readInt n <*> parseStyle s
readCommand (cwords -> ["peek",n])             = Peek <$> readInt n <*> Just Default
readCommand (cwords -> ["peek",n,s])           = Peek <$> readInt n <*> parseStyle s
readCommand (cwords -> ["live",n])             = Live <$> readInt n <*> Just Default
readCommand (cwords -> ["live",n,s])           = Live <$> readInt n <*> parseStyle s
readCommand (cwords -> ["fill"])               = Just Fill
readCommand (cwords -> ["restore"])            = Just RestoreDefaults
readCommand (cwords -> ["reinitialize"])       = Just Reinitialize
readCommand (cwords -> ["status"])             = Just Status
readCommand (cwords -> ["save",path])          = Just (Save path)
readCommand (cwords -> ["help"])               = Just Help
readCommand (cwords -> ["?"])                  = Just Help
readCommand (cwords -> ["quit"])               = Just Quit
readCommand (cwords -> ["q"])                  = Just Quit
readCommand (cwords -> ["set","minsize",n])    = Set MinSize <$> readInt n
readCommand (cwords -> ["set","targetsize",n]) = Set TargetSize <$> readInt n
readCommand _                                  = Nothing


---- Describing/announcing commands ----

bitsNBytes :: Int -> String
bitsNBytes n = show n ++ b ++ show (n*8) ++ " bits)"
   where b = if n /= 1 then " bytes (" else " byte ("

description :: Command -> String
description (Add n)         = "Adding " ++ bitsNBytes n ++ " of quantum random data to store"
description (Live n _)      = "Viewing up to " ++ bitsNBytes n ++ " of live quantum random data from ANU"
description (Observe n _)   = "Observing " ++ bitsNBytes n ++ " of quantum random data from store"
description (Peek n _)      = "Viewing up to " ++ bitsNBytes n ++ " of quantum random data from store"
description (PeekAll _)     = "Viewing all quantum random data from store"
description Fill            = "Filling quantum random data store to specified level"
description RestoreDefaults = "Reverting to default settings"
description Reinitialize    = "Reverting to default settings, and refilling store"
description Quit            = "Exiting"
description _               = ""

announce :: Command -> IO ()
announce c = let str = description c in if str == "" then pure () else putStrLn str


---- Interpreting commands to actions ----

interp :: Command -> ErrorM ()
interp (Add n)            = addToStore n
interp (Observe n style)  = observe style n
interp (Peek n style)     = peek style n
interp (PeekAll style)    = peekAll style
interp (Live n style)     = fetchQRN n >>= liftIO . display style
interp Fill               = fill
interp RestoreDefaults    = liftIO restoreDefaults
interp Reinitialize       = reinitialize
interp Status             = status
interp (Save path)        = liftIO $ save path
interp (Set MinSize n)    = setMinStoreSize n
interp (Set TargetSize n) = setTarStoreSize n
interp Help               = liftIO $ putStrLn helpMsg
interp Quit               = return ()

command :: Command -> IO ()
command c = handleErrors $ liftIO (announce c) *> interp c

execCommand :: String -> IO ()
execCommand (readCommand -> Just c) = command c
execCommand _                       = errorMsg

---- Auxilliary IO actions ----

status :: ErrorM ()
status = do
  min <- getMinStoreSize
  tar <- getTargetStoreSize
  liftIO $ do siz <- storeSize
              sto <- getStoreFile
              mapM_ putStrLn
                [ "Store contains " ++ bitsNBytes siz ++ " of quantum random data"
                , ""
                , "Minimum store size set to " ++ bitsNBytes min ++ "."
                , "Target store size set to " ++ bitsNBytes tar ++ "."
                , ""
                , "Local data store location:"
                , sto
                , ""
                ]

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

errorMsg :: IO ()
errorMsg = do
  putStrLn "***** QRN Error: Couldn't parse command."
  putStrLn "***** Enter 'help' or '?' to see list of available commands."


---- Core program code ----

qrn :: InputT IO ()
qrn = do str <- getInputLine "QRN> "
         let jc = readCommand =<< str
         case jc of
              Just Quit -> return ()
              Just c    -> lift (command c) *> qrn
              Nothing   -> lift errorMsg    *> qrn

main :: IO ()
main = do args <- getArgs
          case args of
               [] -> runInputT defaultSettings qrn
               _  -> execCommand (unwords args)
