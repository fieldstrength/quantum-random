-- | Provides the error-handling context (monad) within which parse errors are handled,
--   as well as the corresponding data type describing the errors. Implementation is a
--   straightforward use of the ExceptT monad transformer from mtl.
module Quantum.Random.ErrorM (

-- * Error-handling context
  ErrorM,

  QError (..),

-- * Converting the context

  stringifyErrorM,
  handleErrors,
  handleWithCrash

) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)

-- | Represents two possible error conditions that may be encountered: either in interpreting
--   data retrived from ANU, or while reading the local settings file.
data QError = ParseResponseError String
            | ParseSettingsError String

instance Show QError where
  show (ParseResponseError str) =
    unlines ["Problem parsing response from ANU server:", str]
  show (ParseSettingsError str) =
    unlines ["Problem loading or interpreting settings file:", str]


-- | An alias for the monadic context in which store-related operations are run, supporting
--   the possible errors described by the 'QError' type.
type ErrorM = ExceptT QError IO


-- | Convert the error-describing type from our custom datatype 'QError' to String,
--   in our small monad transformer stack.
stringifyErrorM :: ErrorM a -> ExceptT String IO a
stringifyErrorM = ExceptT . fmap (first show) . runExceptT

-- | Any computation in the error-handling context that returns only unit can be
--   turned into a (total) standard IO computation. Error information is reported via 'putStrLn'.
handleErrors :: ErrorM () -> IO ()
handleErrors mx = do x <- runExceptT mx
                     case x of
                          Left er -> print er
                          Right _ -> return ()

-- | A computation in the error-handling context that returns an arbitrary value
--   can only be run as an IO action by crashing when an error is encountered.
handleWithCrash :: ErrorM a -> IO a
handleWithCrash mx = do x <- runExceptT mx
                        case x of
                             Left er -> error $ show er
                             Right y -> return y
