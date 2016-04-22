{-# LANGUAGE DeriveGeneric #-}

module QRN.Codec where

import GHC.Generics               (Generic)
import Data.Aeson                 (encode,decode,FromJSON,ToJSON)
import Data.Text                  (Text)
import Data.ByteString.Lazy       (ByteString)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Text.Regex.Posix           ((=~))

-- | Corresponds to the JSON object returned by ANU, minus 'q' prefixes.
--   'process' function performs appropriate renamings.
data QResponse = QResponse { qtype   :: !Text
                           , qlength :: !Int
                           , qdata   :: ![Int]
                           , success :: !Bool } deriving (Show, Generic)

data QSettings = QSettings { minStoreSize :: Int
                           , targetStoreSize :: Int } deriving (Show, Generic)

defaults = QSettings 100 1000

instance FromJSON QResponse
instance ToJSON   QResponse
instance FromJSON QSettings
instance ToJSON   QSettings


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
