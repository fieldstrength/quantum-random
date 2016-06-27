{-# LANGUAGE DeriveDataTypeable #-}

-- | Provides a data type describing the parsing errors that are possible, though unlikely,
--   to be encountered.
--
--   Note that since network requests are handled by the @http-conduit@ package,
--   the corresponding 'HttpException' is also one a user may wish to handle.
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
