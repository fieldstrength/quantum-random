{-# LANGUAGE DeriveGeneric #-}

module QRN.Codec where

import QRN.Monad

import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON,eitherDecode)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Text.Regex.Posix ((=~))
import Data.Bifunctor (first)
import Control.Monad.Except (ExceptT (..), runExceptT)


-- | Corresponds to the JSON object returned by ANU, minus 'q' prefixes.
--   'process' function performs appropriate renamings.
data QResponse = QResponse { qtype   :: !Text
                           , qlength :: !Int
                           , qdata   :: ![Int]
                           , success :: !Bool } deriving (Show, Generic)

data QSettings = QSettings { minStoreSize :: Int
                           , targetStoreSize :: Int } deriving (Show, Generic)


instance FromJSON QResponse
instance ToJSON   QResponse
instance FromJSON QSettings
instance ToJSON   QSettings


defaults = QSettings 200 800


updateMinSize :: Int -> QSettings -> QSettings
updateMinSize n qs = qs { minStoreSize = n }

updateTarSize :: Int -> QSettings -> QSettings
updateTarSize n qs = qs { targetStoreSize = n }


-- | Replace instances of the words "data", "type", "length", with "qdata", "qtype", "qlength"
--   respectively, to avoid clashes between record field names and keywords/Prelude.
process :: ByteString -> ByteString
process = pack . repData . repType . repLength . unpack


repData = replaceWord "data" "qdata"

repType = replaceWord "type" "qtype"

repLength = replaceWord "length" "qlength"


replaceWord :: String -> String -> String -> String
replaceWord x y s = let (a,b,c) = s =~ x :: (String, String, String)
                    in  a ++ y ++ c


parseResponse :: ByteString -> Either QError QResponse
parseResponse = first ParseResponseError . eitherDecode

parseSettings :: ByteString -> Either QError QSettings
parseSettings = first ParseSettingsError . eitherDecode
