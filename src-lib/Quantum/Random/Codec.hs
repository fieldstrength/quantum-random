{-# LANGUAGE DeriveGeneric #-}

-- | Internal module for handling conversion between raw text and structured data.
module Quantum.Random.Codec (

  QResponse (..),
  QSettings (..),
  defaults,
  parseResponse,
  parseSettings,
  updateTarSize,
  updateMinSize

) where

import Quantum.Random.Exceptions

import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON,eitherDecode)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Text.Regex.Posix ((=~))
import Data.Bifunctor (first)


-- | Corresponds to the JSON object returned by ANU, minus 'q' prefixes.
--   'process' function performs appropriate renamings.
data QResponse = QResponse { qtype   :: !Text
                           , qlength :: !Int
                           , qdata   :: ![Int]
                           , success :: !Bool } deriving (Show, Generic)

-- | Corresponds to the JSON object in which settings are stored locally.
data QSettings = QSettings { minStoreSize :: Int
                           , targetStoreSize :: Int } deriving (Show, Generic)


instance FromJSON QResponse
instance ToJSON   QResponse
instance FromJSON QSettings
instance ToJSON   QSettings

-- | Default settings.
defaults :: QSettings
defaults = QSettings 200 800

-- | Update the minimum store size field of a settings record.
updateMinSize :: Int -> QSettings -> QSettings
updateMinSize n qs = qs { minStoreSize = n }

-- | Update the target store size field of a settings record.
updateTarSize :: Int -> QSettings -> QSettings
updateTarSize n qs = qs { targetStoreSize = n }


-- | Replace instances of the words "data", "type", "length", with "qdata", "qtype", "qlength"
--   respectively, to avoid clashes between record field names and keywords/Prelude.
process :: ByteString -> ByteString
process = pack . repData . repType . repLength . unpack

repData :: String -> String
repData = replaceWord "data" "qdata"

repType :: String -> String
repType = replaceWord "type" "qtype"

repLength :: String -> String
repLength = replaceWord "length" "qlength"


replaceWord :: String -> String -> String -> String
replaceWord x y s = let (a,_,c) = s =~ x :: (String, String, String)
                    in  a ++ y ++ c

-- | From a Bytestring, attempt to decode a response from ANU ('QResponse').
parseResponse :: ByteString -> Either QRNException QResponse
parseResponse = first ParseResponseError . eitherDecode . process

-- | From a Bytestring, attempt to decode a settings record.
parseSettings :: ByteString -> Either QRNException QSettings
parseSettings = first ParseSettingsError . eitherDecode
