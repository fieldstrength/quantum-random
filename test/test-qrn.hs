module Main where

import Test.Hspec
import Test.QuickCheck
import Quantum.Random


main :: IO ()
main = hspec $ do

  describe "parseStyle" $ do

    it "returns Nothing for unrecognized strings" $
      parseStyle "notastyle" `shouldBe` Nothing
    it "parses 'colors' to Just Colors" $
      parseStyle "colors" `shouldBe` Just Colors
    it "parses 'COLORS' to Just Colors" $
      parseStyle "COLORS" `shouldBe` Just Colors
    it "parses 'bits' to Just Bits" $
      parseStyle "bits" `shouldBe` Just Bits
    it "parses 'BITS' to Just Bits" $
      parseStyle "BITS" `shouldBe` Just Bits

    it "parses 'hex' to Just Hex" $
      parseStyle "hex" `shouldBe` Just Hex
    it "parses 'HEX' to Just Hex" $
      parseStyle "HEX" `shouldBe` Just Hex
    it "parses 'hexidecimal' to Just Hex" $
      parseStyle "hexidecimal" `shouldBe` Just Hex
    it "parses 'HEXIDECIMAL' to Just Hex" $
      parseStyle "HEXIDECIMAL" `shouldBe` Just Hex

    it "parses 'colorbits' to Just ColorBits" $
      parseStyle "colorbits" `shouldBe` Just ColorBits
    it "parses 'COLORBITS' to Just ColorBits" $
      parseStyle "COLORBITS" `shouldBe` Just ColorBits
    it "parses 'colorhex' to Just ColorHex" $
      parseStyle "colorhex" `shouldBe` Just ColorHex
    it "parses 'COLORHEX' to Just ColorHex" $
      parseStyle "COLORHEX" `shouldBe` Just ColorHex

    it "parses 'colorspins' to Just ColorSpins" $
      parseStyle "colorspins" `shouldBe` Just ColorSpins
    it "parses 'COLORSPINS' to Just ColorSpins" $
      parseStyle "COLORSPINS" `shouldBe` Just ColorSpins


{-
-- | Parse a string to one of the supported display styles.
parseStyle :: String -> Maybe DisplayStyle
parseStyle (map toLower -> "colors")      = Just Colors
parseStyle (map toLower -> "spins")       = Just Spins
parseStyle (map toLower -> "bits")        = Just Bits
parseStyle (map toLower -> "binary")      = Just Bits
parseStyle (map toLower -> "hex")         = Just Hex
parseStyle (map toLower -> "hexidecimal") = Just Hex
parseStyle (map toLower -> "colorspins")  = Just ColorSpins
parseStyle (map toLower -> "colorbits")   = Just ColorBits
parseStyle (map toLower -> "colorbinary") = Just ColorBits
parseStyle (map toLower -> "colorhex")    = Just ColorHex
parseStyle _                              = Nothing
-}
