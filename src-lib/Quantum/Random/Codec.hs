{-# LANGUAGE DeriveGeneric #-}

-- | Internal module for handling conversion between raw text and structured data.
module Quantum.Random.Codec (

  QRResponse (..),
  QRSettings (..),
  defaults,
  parseResponse,
  parseSettings,
  updateTarSize,
  updateMinSize,
  updateDefaultStyle

) where

import Quantum.Random.Exceptions
import Quantum.Random.Display

import GHC.Generics               (Generic)
import Data.Aeson                 (FromJSON,ToJSON,eitherDecode)
import Data.Text                  (Text)
import Data.ByteString.Lazy       (ByteString)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Text.Regex.Posix           ((=~))
import Data.Bifunctor             (first)


-- | Corresponds to the JSON object returned by ANU, minus 'q' prefixes.
--   The 'process' function performs appropriate renamings.
data QRResponse = QRResponse { qtype   :: !Text
                             , qlength :: !Int
                             , qdata   :: ![Int]
                             , success :: !Bool } deriving (Show, Generic)

-- | Corresponds to the JSON object in which settings are stored locally.
data QRSettings = QRSettings { minStoreSize :: Int
                             , targetStoreSize :: Int
                             , defaultDisplayStyle :: DisplayStyle } deriving (Show, Generic)


instance FromJSON QRResponse
instance ToJSON   QRResponse
instance FromJSON QRSettings
instance ToJSON   QRSettings

-- | Default settings.
defaults :: QRSettings
defaults = QRSettings 400 800 ColorHex

-- | Update the minimum store size field of a settings record.
updateMinSize :: Int -> QRSettings -> QRSettings
updateMinSize n qs = qs { minStoreSize = n }

-- | Update the target store size field of a settings record.
updateTarSize :: Int -> QRSettings -> QRSettings
updateTarSize n qs = qs { targetStoreSize = n }

-- | Update the default 'DisplayStyle' of a settings record.
updateDefaultStyle :: DisplayStyle -> QRSettings -> QRSettings
updateDefaultStyle sty qs = qs { defaultDisplayStyle = sty }

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

-- | From a Bytestring, attempt to decode a response from ANU ('QRResponse').
parseResponse :: ByteString -> Either QRException QRResponse
parseResponse = first ParseResponseError . eitherDecode . process

-- | From a Bytestring, attempt to decode a settings record.
parseSettings :: ByteString -> Either QRException QRSettings
parseSettings = first ParseSettingsError . eitherDecode
