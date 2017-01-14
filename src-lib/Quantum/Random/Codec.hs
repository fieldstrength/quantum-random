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
import Data.Aeson
import Data.Aeson.Types           (Options (..))
import Data.Text                  (Text)
import Data.ByteString.Lazy       (ByteString)
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

instance FromJSON QRResponse where
    parseJSON = genericParseJSON $
        defaultOptions { fieldLabelModifier = qrFieldRenamer }
            where qrFieldRenamer :: String -> String
                  qrFieldRenamer ('q':str) = str
                  qrFieldRenamer str       = str

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

-- | From a Bytestring, attempt to decode a response from ANU ('QRResponse').
parseResponse :: ByteString -> Either QRException QRResponse
parseResponse = first ParseResponseError . eitherDecode

-- | From a Bytestring, attempt to decode a settings record.
parseSettings :: ByteString -> Either QRException QRSettings
parseSettings = first ParseSettingsError . eitherDecode
