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

import System.Console.ANSI          (Color (..), ColorIntensity (..), setSGR)
import System.Console.Ansigraph     (AnsiColor (..), setFG, clear)
import System.Console.Terminal.Size (size,width)
import Data.Word                    (Word8)
import Data.Bits                    (testBit)
import Data.Char                    (toLower)
import Numeric                      (showHex)


-- | Represents supported methods for displaying binary data.
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

-- 'Bits' type class indexes bits from least to most significant, thus the reverse
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


---- Interpreting as strings ----

binChar :: Bool -> Char
binChar False = '0'
binChar True  = '1'

spinChar :: Bool -> Char
spinChar False = '↑'
spinChar True  = '↓'

binStr :: FourBits -> String
binStr (a,b,c,d) = [binChar a, binChar b, binChar c, binChar d]

spinStr :: FourBits -> String
spinStr (a,b,c,d) = [spinChar a, spinChar b, spinChar c, spinChar d]

hexStr :: Word8 -> String
hexStr w = let hx = showHex w " "
           in  if length hx < 3 then '0' : hx else hx


---- Byte display functions ----

binDisplay :: Word8 -> IO ()
binDisplay (sepByte -> (x,y)) = do
  putStr $ (binStr x) ++ " " ++ (binStr y) ++ "  "

spinDisplay :: Word8 -> IO ()
spinDisplay (sepByte -> (x,y)) = do
  putStr $ (spinStr x) ++ " " ++ (spinStr y) ++ "  "

hexDisplay :: Word8 -> IO ()
hexDisplay = putStr . hexStr

binColorDisplay :: Word8 -> IO ()
binColorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  colorBlock (color y)
  putStr $ " " ++ (binStr x) ++ " " ++ (binStr y) ++ " "

spinColorDisplay :: Word8 -> IO ()
spinColorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  colorBlock (color y)
  putStr $ " " ++ (spinStr x) ++ " " ++ (spinStr y) ++ " "

hexColorDisplay :: Word8 -> IO ()
hexColorDisplay w = do
  let (x,y) = sepByte w
  colorBlock (color x)
  colorBlock (color y)
  putStr $ ' ' : hexStr w

colorDisplay :: Word8 -> IO ()
colorDisplay (sepByte -> (x,y)) = do
  colorBlock (color x)
  colorBlock (color y)


---- Interpreting as display IO actions ----

displayByte :: DisplayStyle -> Word8 -> IO ()
displayByte Colors     = colorDisplay
displayByte Spins      = spinDisplay
displayByte Bits       = binDisplay
displayByte ColorSpins = spinColorDisplay
displayByte ColorBits  = binColorDisplay
displayByte Hex        = hexDisplay
displayByte ColorHex   = hexColorDisplay

-- How many characters each display style uses per byte
byteSize :: DisplayStyle -> Int
byteSize ColorHex   = 6
byteSize ColorBits  = 13
byteSize ColorSpins = 13
byteSize Bits       = 11
byteSize Spins      = 11
byteSize Hex        = 3
byteSize _          = 1

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n x l = take n l ++ nl
   where nl = if length (drop n l) > 0
              then (x : insertEvery n x (drop n l))
              else []

-- Obtain the character-width of the terminal. On failure assume a conservative default.
termWidth :: IO Int
termWidth = do mw <- fmap width <$> size
               case mw of
                    Just w  -> pure w
                    Nothing -> pure 80

-- Display data, such that no byte is broken by a new line.
displayBytes :: DisplayStyle -> [Word8] -> IO ()
displayBytes sty ws = do
  w <- termWidth
  let bw = w `div` byteSize sty
  sequence_ $ insertEvery bw (putStrLn "") $ displayByte sty <$> ws

-- | Display a given list of bytes with the specified display style.
display :: DisplayStyle -> [Word8] -> IO ()
display s l = displayBytes s l *> putStrLn ""
