-- | Provides the error-handling context (monad) within which parse errors are handled,
--   as well as the corresponding data type describing the errors. Implementation is a
--   straightforward use of the ExceptT monad transformer from mtl.
module Quantum.Random.ErrorM (

-- * Error-handling context

  ErrorM,

-- * Converting the context

  handleErrors,
  handleWithCrash,

-- * Describing error conditions

  responseErr,
  settingsErr

) where

import Control.Monad.Except (ExceptT (..), runExceptT)


-- | Prepend a string to make ANU response parsing errors more descriptive.
responseErr :: String -> String
responseErr str = unlines ["Problem parsing response from ANU server:", str]

-- | Prepend a string to make settings file parsing errors more descriptive.
settingsErr :: String -> String
settingsErr str = unlines ["Problem parsing settings file:", str]


-- | An alias for the monadic context in which prase errors are handled
type ErrorM = ExceptT String IO


-- | Any computation in the error-handling context that returns only unit can be
--   turned into a (total) standard IO computation. Error information is reported via 'putStrLn'.
handleErrors :: ErrorM () -> IO ()
handleErrors mx = do x <- runExceptT mx
                     case x of
                          Left er -> putStrLn er
                          Right _ -> return ()

-- | A computation in the error-handling context that returns an arbitrary value
--   can only be run as an IO action by crashing when an error is encountered.
handleWithCrash :: ErrorM a -> IO a
handleWithCrash mx = do x <- runExceptT mx
                        case x of
                             Left er -> error er
                             Right y -> return y
