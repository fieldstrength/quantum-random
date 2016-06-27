{-# LANGUAGE DeriveDataTypeable #-}

-- | Provides a data type describing possible error conditions for the package,
--   instantiating the 'Exception' type class, and basic functionality for throwing and
--   catching these exceptions.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Exceptions (

-- * QRN exception data type
  QRException (..),

-- * Throwing
  throwLeft

) where

import Data.Typeable        (Typeable)
import Control.Exception    (Exception, throw)

-- | Represents two possible error conditions that may be encountered: in interpreting
--   data retrived from ANU, or while reading the local settings file.
data QRException = ParseResponseError String
                 | ParseSettingsError String deriving Typeable

instance Show QRException where
  show (ParseResponseError str) = unlines ["Problem parsing response from ANU server:", str]
  show (ParseSettingsError str) = unlines ["Problem interpreting settings file:", str]

instance Exception QRException

-- | Perform an IO computation that might encounter a problem corresponding to a 'Exception'.
--   If so, throw the exception, if not just return the value.
throwLeft :: Exception e => IO (Either e a) -> IO a
throwLeft ioer = do
  mx <- ioer
  case mx of
       Left er -> throw er
       Right x -> pure x
