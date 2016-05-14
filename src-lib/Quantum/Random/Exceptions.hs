{-# LANGUAGE DeriveDataTypeable #-}

-- | Provides a data type describing possible error conditions for the package,
--   instantiating the 'Exception' type class, and basic functionality for throwing and
--   catching these exceptions.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Exceptions (

-- * QRN exception data type
  QRNException (..),

-- ** Throwing
  throwLeft,

-- ** Catching
  qrnExceptHandler,
  handleQRNExceptions

) where

import Data.Typeable        (Typeable)
import Control.Exception    (Exception, handle, throw)
import Network.HTTP.Conduit (HttpException)

-- | Represents two possible error conditions that may be encountered: in interpreting
--   data retrived from ANU, or while reading the local settings file.
data QRNException = ParseResponseError String
                  | ParseSettingsError String deriving Typeable

instance Show QRNException where
  show (ParseResponseError str) = unlines ["Problem parsing response from ANU server:", str]
  show (ParseSettingsError str) = unlines ["Problem loading or interpreting settings file:", str]

instance Exception QRNException

-- | Perform an IO computation that might encounter a problem corresponding to a 'Exception'.
--   If so, throw the exception, if not just return the value.
throwLeft :: Exception e => IO (Either e a) -> IO a
throwLeft ioer = do
  mx <- ioer
  case mx of
       Left er -> throw er
       Right x -> pure x

-- | Action to perform when encountering a 'QRNException'. Report the error via the `Show` instance.
--   Equivalent to `print` but with a necessarily more specific type.
qrnExceptHandler :: QRNException -> IO ()
qrnExceptHandler = print

httpExceptHandler :: HttpException -> IO ()
httpExceptHandler e = putStr "HTTP exception: " *> print e

-- | Apply to an IO action. When either a 'QRNException' or 'HttpException' is encountered,
--   it is reported instead of crashing.
handleQRNExceptions :: IO () -> IO ()
handleQRNExceptions = handle qrnExceptHandler . handle httpExceptHandler