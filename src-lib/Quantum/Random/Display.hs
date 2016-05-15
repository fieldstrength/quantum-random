{-# LANGUAGE ViewPatterns #-}

-- | Functionality for display of binary data. Seeing a visual representation of quantum random
--   data lets a user visually verify that it is indeed random.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Display (

  DisplayStyle (..),
  parseStyle,
  display

) where

import System.Console.ANSI      (Color (..), ColorIntensity (..), setSGR)
import System.Console.Ansigraph (AnsiColor (..), setFG, clear)
import Data.Word                (Word8)
import Data.Bits                (testBit)
import Data.Char                (toLower)
import Numeric                  (showHex)


-- | Represents the supported methods for displaying binary data.
--   This data type may be extended in the future.
data DisplayStyle = Colors
                  | Spins
                  | Bits
                  | Hex
                  | ColorSpins
                  | ColorBits
                  | ColorHex

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


---- Interpreting as colors ----

-- 'Bits' type class numbers bits from least to most significant, thus the reverse
w8bools :: Word8 -> [Bool]
w8bools w = reverse $ testBit w <$> [0..7]

type EightBits = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)
type FourBits  = (Bool,Bool,Bool,Bool)

byteBits :: Word8 -> EightBits
byteBits (w8bools -> [a,b,c,d,e,f,g,h]) = (a,b,c,d,e,f,g,h)
byteBits _ = error "byteBits: Impossible case: w8bools produces length-8 list"

sepByte :: Word8 -> (FourBits, FourBits)
sepByte (byteBits -> (a,b,c,d,e,f,g,h)) = ((a,b,c,d), (e,f,g,h))

color :: FourBits -> AnsiColor
color (False,False,False,False) = AnsiColor Dull Black
color (False,False,False,True)  = AnsiColor Vivid Black
color (False,False,True,False)  = AnsiColor Dull Red
color (False,False,True,True)   = AnsiColor Vivid Red
color (False,True,False,False)  = AnsiColor Dull Green
color (False,True,False,True)   = AnsiColor Vivid Green
color (False,True,True,False)   = AnsiColor Dull Yellow
color (False,True,True,True)    = AnsiColor Vivid Yellow
color (True,False,False,False)  = AnsiColor Dull Blue
color (True,False,False,True)   = AnsiColor Vivid Blue
color (True,False,True,False)   = AnsiColor Dull Magenta
color (True,False,True,True)    = AnsiColor Vivid Magenta
color (True,True,False,False)   = AnsiColor Dull Cyan
color (True,True,False,True)    = AnsiColor Vivid Cyan
color (True,True,True,False)    = AnsiColor Dull White
color (True,True,True,True)     = AnsiColor Vivid White

colorBlock :: AnsiColor -> IO ()
colorBlock c = setSGR [setFG c] *> putStr "█" *> clear


---- Interpreting as characters ----

spinChar :: Bool -> Char
spinChar False = '↑'
spinChar True  = '↓'

spinStr :: FourBits -> String
spinStr (a,b,c,d) = [spinChar a, spinChar b, spinChar c, spinChar d]

bitChar :: Bool -> Char
bitChar False = '0'
bitChar True  = '1'

bitStr :: FourBits -> String
bitStr (a,b,c,d) = [bitChar a, bitChar b, bitChar c, bitChar d]

hexShow :: Word8 -> String
hexShow w = let hx = showHex w ""
            in  if length hx < 2 then '0' : hx else hx

hexChar1 :: Word8 -> Char
hexChar1 = head . hexShow

hexChar2 :: Word8 -> Char
hexChar2 = head . tail . hexShow


---- Byte display functions ----

binaryDisplay :: Word8 -> IO ()
binaryDisplay (sepByte -> (x,y)) = do
  putStr (bitStr x)
  putStr (bitStr y)

spinDisplay :: Word8 -> IO ()
spinDisplay (sepByte -> (x,y)) = do
  putStr (spinStr x)
  putStr (spinStr y)

bitColorDisplay :: Word8 -> IO ()
bitColorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  putStr (bitStr x)
  colorBlock (color y)
  putStr (bitStr y)

spinColorDisplay :: Word8 -> IO ()
spinColorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  putStr (spinStr x)
  colorBlock (color y)
  putStr (spinStr y)

hexColorDisplay :: Word8 -> IO ()
hexColorDisplay w = do
  let (x,y) = sepByte w
  colorBlock (color x)
  colorBlock (color y)
  putChar (hexChar1 w)
  putChar (hexChar2 w)

colorDisplay :: Word8 -> IO ()
colorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  colorBlock (color y)


---- Interpreting as display IO actions ----

displayByte :: DisplayStyle -> Word8 -> IO ()
displayByte Colors     = colorDisplay
displayByte Spins      = spinDisplay
displayByte Bits       = binaryDisplay
displayByte ColorSpins = spinColorDisplay
displayByte ColorBits  = bitColorDisplay
displayByte Hex        = putStr . hexShow
displayByte ColorHex   = hexColorDisplay

displayBytes :: DisplayStyle -> [Word8] -> IO ()
displayBytes = mapM_ . displayByte

-- | Display a given list of bytes with the specified display style.
display :: DisplayStyle -> [Word8] -> IO ()
display s l = displayBytes s l *> putStrLn ""
