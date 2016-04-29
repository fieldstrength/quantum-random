module QRN.Monad where

import Control.Monad.Except (ExceptT (..), runExceptT)


data QError = ParseResponseError String
            | ParseSettingsError String deriving Show

type ErrorM = ExceptT QError IO

handleErrors :: ErrorM () -> IO ()
handleErrors mx = do x <- runExceptT mx
                     case x of
                          Left er -> print er
                          Right y -> return y

handleCrash :: ErrorM a -> IO a
handleCrash mx = do x <- runExceptT mx
                    case x of
                         Left er -> error $ show er
                         Right y -> return y
