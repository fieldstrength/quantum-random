{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Quantum.Random

import Data.Char                 (isDigit, toLower)
import Control.Monad.Reader      (ReaderT (..), liftIO, lift)
import System.Console.Haskeline  (InputT, getInputLine, runInputT, defaultSettings)
import System.Environment        (getArgs)


---- Structure of commands ----

-- | Represents the settings saved in the settings file. This data type could be
--   extended in the future.
data Setting = MinSize Int
             | TargetSize Int
             | DefaultStyle DisplayStyle

-- | Represents the supported commands.
data Command = Add Int
             | Observe Int (Maybe DisplayStyle)
             | Peek    Int (Maybe DisplayStyle)
             | PeekAll     (Maybe DisplayStyle)
             | Live    Int (Maybe DisplayStyle)
             | Fill
             | RestoreDefaults
             | Reinitialize
             | Status
             | Save String
             | Load String
             | Set Setting
             | Help
             | Quit

helpMsg :: String
helpMsg = unlines
  [ ""
  , "======= Available commands ======="
  , ""
  , "add [# bytes]     –  Request specified number of quantum random bytes from ANU and add them to the store"
  , "live [# bytes]    –  Request specified number of quantum random bytes from ANU and display them directly"
  , "observe [# bytes] –  Take and display data from store, retrieving more if needed. Those taken from the store are removed"
  , "peek [# bytes]    –  Display up to the specified number of bytes from the store without removing them"
  , "peekAll           –  Display all data from the store without removing them"
  , "fill              –  Fill the store to the target size with live ANU quantum random numbers"
  , "restore           –  Restore default settings"
  , "reinitialize      –  Restore default settings, and refill store to target size"
  , "status            –  Display status of store and settings"
  , "save [filepath]   –  save binary quantum random store file to specified file path"
  , "load [filepath]   –  load binary file and append data to store"
  , "set minSize       –  Set the number of bytes below which the store is refilled"
  , "set targetSize    –  Set the number of bytes to have after refilling"
  , "set style [style] –  Set the default display style"
  , "help/?            –  Display this text"
  , "quit/q            –  Quit"
  , ""
  , "======= Display options ======="
  , ""
  , "Commands that display QR data can take an optional display style modifier: "
  , "'spins', 'bits', 'hex', 'colors', 'colorSpins', 'colorBits', 'colorHex' (the default)."
  , ""
  , "Examples:"
  , "\"observe 25 colorspins\""
  , "\"live 50 bits\""
  ]

---- Parsing commands ----

cwords :: String -> [String]
cwords = words . map toLower

readCommand :: String -> Maybe Command
readCommand (cwords -> ["add",n])        = Add <$> readInt n
readCommand (cwords -> ["peekall"])      = Just (PeekAll Nothing)
readCommand (cwords -> ["peekall",s])    = PeekAll <$> readStyle s
readCommand (cwords -> ["peek","all"])   = Just (PeekAll Nothing)
readCommand (cwords -> ["peek","all",s]) = PeekAll <$> readStyle s
readCommand (cwords -> ["observe",n])    = Observe <$> readInt n <*> Just Nothing
readCommand (cwords -> ["observe",n,s])  = Observe <$> readInt n <*> readStyle s
readCommand (cwords -> ["peek",n])       = Peek <$> readInt n <*> Just Nothing
readCommand (cwords -> ["peek",n,s])     = Peek <$> readInt n <*> readStyle s
readCommand (cwords -> ["live",n])       = Live <$> readInt n <*> Just Nothing
readCommand (cwords -> ["live",n,s])     = Live <$> readInt n <*> readStyle s
readCommand (cwords -> ["fill"])         = Just Fill
readCommand (cwords -> ["restore"])      = Just RestoreDefaults
readCommand (cwords -> ["reinitialize"]) = Just Reinitialize
readCommand (cwords -> ["status"])       = Just Status
readCommand (savePattern -> Just path)   = Just (Save path)
readCommand (loadPattern -> Just path)   = Just (Load path)
readCommand (cwords -> ["help"])         = Just Help
readCommand (cwords -> ["?"])            = Just Help
readCommand (cwords -> ["quit"])         = Just Quit
readCommand (cwords -> ["q"])            = Just Quit
readCommand (cwords -> "set" : ws)       = Set <$> readSetting ws
readCommand _                            = Nothing

--- Read Integers

readInt :: String -> Maybe Int
readInt = readIntRev . reverse
   where readIntRev :: String -> Maybe Int
         readIntRev []       = Nothing
         readIntRev [c]      = readDigit c
         readIntRev (c:x:xs) = fmap (+) (readDigit c) <*> fmap (*10) (readIntRev (x:xs))

readDigit :: Char -> Maybe Int
readDigit c = if isDigit c then Just (read [c]) else Nothing

--- Read Settings

readSetting :: [String] -> Maybe Setting
readSetting ["minsize",n]      = MinSize <$> readInt n
readSetting ["tarsize",n]      = TargetSize <$> readInt n
readSetting ["targetsize",n]   = TargetSize <$> readInt n
readSetting ["defaultstyle",s] = DefaultStyle <$> parseStyle s
readSetting ["style",s]        = DefaultStyle <$> parseStyle s
readSetting _                  = Nothing

--- Read Display Style

readStyle :: String -> Maybe (Maybe DisplayStyle)
readStyle = fmap Just . parseStyle

--- Read save/load commands
--- Used because 'cwords' pattern used elsewhere loses upper/lowercase information

savePattern :: String -> Maybe String
savePattern str =
  case cwords str of
    [_]        -> Nothing
    "save" : _ -> return . unwords . tail . words $ str
    _          -> Nothing

loadPattern :: String -> Maybe String
loadPattern str =
  case cwords str of
    [_]        -> Nothing
    "load" : _ -> return . unwords . tail . words $ str
    _          -> Nothing


---- Describing/announcing commands ----

bitsNBytes :: Int -> String
bitsNBytes n = show n ++ bytes ++ show (n*8) ++ " bits)"
   where bytes = if n /= 1 then " bytes (" else " byte ("

description :: Command -> String
description (Add n)         = "Adding " ++ bitsNBytes n ++ " of quantum random data to store"
description (Live n _)      = "Retrieving " ++ bitsNBytes n ++ " of live quantum random data from ANU"
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

-- | For a given command, as described by the 'Command' data type, interpret to the corresponding
--   IO action. This is the simple version, whereas 'interpSafe' ensures these actions cannot
--   interfere as they access local files.
interp :: Command -> IO ()
interp (Add n)                = addToStore n
interp (Observe n msty)       = observe msty n
interp (Peek n msty)          = peek msty n
interp (PeekAll msty)         = peekAll msty
interp (Live n msty)          = fetchQR n >>= display_ msty
interp Fill                   = fill
interp RestoreDefaults        = restoreDefaults
interp Reinitialize           = reinitialize
interp Status                 = status
interp (Save path)            = save path
interp (Load path)            = load path
interp (Set (MinSize n))      = setMinStoreSize n
interp (Set (TargetSize n))   = setTargetStoreSize n
interp (Set (DefaultStyle s)) = setDefaultStyle s
interp Help                   = putStrLn helpMsg
interp Quit                   = pure ()

-- | Perform command, via 'interp', after printing a description to STDOUT.
command :: Command -> IO ()
command c = announce c *> interp c

-- | Given a string, attempt to interpret it as an IO action, and either perform it or report
--   the parse failure.
execCommand :: String -> IO ()
execCommand (readCommand -> Just c) = command c
execCommand _                       = errorMsg

errorMsg :: IO ()
errorMsg = do
  putStrLn "***** QR Error: Could not parse command."
  putStrLn "***** Enter 'help' or '?' to see list of available commands."


---- Interpreting commands in the lifted monadic context ----

type QReader = ReaderT AccessControl IO

-- | Controlled-access variant of 'interp'.
interpSafe :: Command -> QReader ()
interpSafe (Add n)       = ReaderT $ \a -> addSafely a n
interpSafe (Observe n s) = ReaderT $ \a -> observeSafely a s n
interpSafe Quit          = ReaderT exitSafely
interpSafe c@(Live _ _)  = liftIO $ interp c
interpSafe c@Help        = liftIO $ interp c
interpSafe c             = ReaderT $ \a -> withAccess a (interp c)

-- | Controlled-access, ReaderT-based variant of 'command'. Performs command via 'interpSafe' after
--   printing a description to STDOUT.
commandSafe :: Command -> QReader ()
commandSafe c = do
  liftIO $ announce c
  interpSafe c


---- Core program code ----

type QR = InputT (ReaderT AccessControl IO)

qrand :: QR ()
qrand = do
  str <- getInputLine "quantum-random> "
  let jc = readCommand =<< str
  case jc of
       Just Quit -> lift (commandSafe Quit)
       Just c    -> lift (commandSafe c)    *> qrand
       Nothing   -> liftIO errorMsg         *> qrand

-- | The main function associated with the executable @qrand@, the interactive manager program.
main :: IO ()
main = do
  args <- getArgs
  case args of
       [] -> initAccessControl >>= runReaderT (runInputT defaultSettings qrand)
       _  -> execCommand (unwords args)
