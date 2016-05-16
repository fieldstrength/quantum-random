{-# LANGUAGE ViewPatterns #-}

-- | Exports the command line interface program ('main') for managing the quantum random data, and
--   some related functions that would be useful for any variant implementation.
module Quantum.Random.Interface (

-- * Abstract command representation
  Command (..),
  Setting (..),
  DisplayStyle (..),

-- * Interpreting commands
  interp,
  command,

-- * Controlled-access commands
-- | These are variants of the above command execution operations that use 'AccessControl' to
--   ensure operations involving the local files do not interfere. To use them, first use
--   'initAccessControl' and then make sure that /that same/ 'AccessControl' is passed to the
--   IO operations that take it as an argument.
  interpSafe,
  commandSafe,

-- * String-based invocation
  execCommand,

-- * Main executable
  main,

) where

import Quantum.Random

import Data.Char                 (isDigit, toLower)
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline  (InputT, getInputLine, runInputT, defaultSettings)
import System.Environment        (getArgs)


---- Structure of commands ----

-- | Represents the settings saved in the settings file. This data type may be
--   extended in the future.
data Setting = MinSize | TargetSize

-- | Represents the supported commands.
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
             | Load String
             | Set Setting Int
             | Help
             | Quit

helpMsg :: String
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
  , "load [filepath]   –  load binary file and append data to store"
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
readCommand (cwords -> ["add",n])        = Add <$> readInt n
readCommand (cwords -> ["peekall"])      = Just (PeekAll ColorHex)
readCommand (cwords -> ["peekall",s])    = PeekAll <$> parseStyle s
readCommand (cwords -> ["peek","all"])   = Just (PeekAll ColorHex)
readCommand (cwords -> ["peek","all",s]) = PeekAll <$> parseStyle s
readCommand (cwords -> ["observe",n])    = Observe <$> readInt n <*> Just ColorHex
readCommand (cwords -> ["observe",n,s])  = Observe <$> readInt n <*> parseStyle s
readCommand (cwords -> ["peek",n])       = Peek <$> readInt n <*> Just ColorHex
readCommand (cwords -> ["peek",n,s])     = Peek <$> readInt n <*> parseStyle s
readCommand (cwords -> ["live",n])       = Live <$> readInt n <*> Just ColorHex
readCommand (cwords -> ["live",n,s])     = Live <$> readInt n <*> parseStyle s
readCommand (cwords -> ["fill"])         = Just Fill
readCommand (cwords -> ["restore"])      = Just RestoreDefaults
readCommand (cwords -> ["reinitialize"]) = Just Reinitialize
readCommand (cwords -> ["status"])       = Just Status
readCommand (cwords -> ["save",path])    = Just (Save path)
readCommand (cwords -> ["load",path])    = Just (Load path)
readCommand (cwords -> ["help"])         = Just Help
readCommand (cwords -> ["?"])            = Just Help
readCommand (cwords -> ["quit"])         = Just Quit
readCommand (cwords -> ["q"])            = Just Quit
readCommand (cwords -> ["set",st,n])     = Set <$> readSetting st <*> readInt n
readCommand _                            = Nothing

readSetting :: String -> Maybe Setting
readSetting (cwords -> ["minsize"])    = Just MinSize
readSetting (cwords -> ["tarsize"])    = Just TargetSize
readSetting (cwords -> ["targetsize"]) = Just TargetSize
readSetting _                          = Nothing


---- Describing/announcing commands ----

bitsNBytes :: Int -> String
bitsNBytes n = show n ++ bytes ++ show (n*8) ++ " bits)"
   where bytes = if n /= 1 then " bytes (" else " byte ("

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

-- | For a given command, as described by the 'Command' data type, interpret to the corresponding
--   IO action. This is the simple version, whereas 'interpSafe' ensures these actions cannot
--   interfere as they access local files.
interp :: Command -> IO ()
interp (Add n)            = addToStore n
interp (Observe n style)  = observe style n
interp (Peek n style)     = peek style n
interp (PeekAll style)    = peekAll style
interp (Live n style)     = fetchQRN n >>= display style
interp Fill               = fill
interp RestoreDefaults    = restoreDefaults
interp Reinitialize       = reinitialize
interp Status             = status
interp (Save path)        = save path
interp (Load path)        = load path
interp (Set MinSize n)    = setMinStoreSize n
interp (Set TargetSize n) = setTargetStoreSize n
interp Help               = putStrLn helpMsg
interp Quit               = pure ()

-- | Controlled-access variant of 'interp'.
interpSafe :: AccessControl -> Command -> IO ()
interpSafe a   (Add n)       = addSafely a n
interpSafe a   (Observe n s) = observeSafely a s n
interpSafe _ c@(Live _ _)    = interp c
interpSafe _ c@Help          = interp c
interpSafe _ c@Quit          = interp c
interpSafe a c               = withAccess a (interp c)

-- | Perform command, via 'interp', after printing a description to STDOUT, with any exceptions
--   reported there as well.
command :: Command -> IO ()
command c = handleQRNExceptions (announce c *> interp c)

-- | Controlled-access variant of 'command'. Performs command via 'interpSafe' after printing a
--   description to STDOUT, with any exceptions reported there as well.
commandSafe :: AccessControl -> Command -> IO ()
commandSafe sys c = handleQRNExceptions (announce c *> interpSafe sys c)

-- | Given a string, attempt to interpret it as an IO action, and either perform it or report
--   the parse failure.
execCommand :: String -> IO ()
execCommand (readCommand -> Just c) = command c
execCommand _                       = errorMsg

---- Auxilliary IO actions ----

status :: IO ()
status = do
  smin <- getMinStoreSize
  star <- getTargetStoreSize
  siz <- storeSize
  sto <- getStoreFile
  mapM_ putStrLn
    [ "Store contains " ++ bitsNBytes siz ++ " of quantum random data."
    , ""
    , "Minimum store size set to " ++ bitsNBytes smin ++ "."
    , "Target store size set to " ++ bitsNBytes star ++ "."
    , ""
    , "Local data store location:"
    , sto
    , ""
    ]

errorMsg :: IO ()
errorMsg = do
  putStrLn "***** QRN Error: Couldn't parse command."
  putStrLn "***** Enter 'help' or '?' to see list of available commands."


---- Core program code ----

qrn :: AccessControl -> InputT IO ()
qrn ac = do str <- getInputLine "QRN> "
            let jc = readCommand =<< str
            case jc of
                 Just Quit -> pure ()
                 Just c    -> lift (commandSafe ac c) *> qrn ac
                 Nothing   -> lift errorMsg           *> qrn ac

-- | The main function associated with the executable @qrn@. The interactive QRN manager program.
main :: IO ()
main = do args <- getArgs
          case args of
               [] -> initAccessControl >>= runInputT defaultSettings . qrn
               _  -> execCommand (unwords args)
