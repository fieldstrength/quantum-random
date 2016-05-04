{-# LANGUAGE ViewPatterns #-}

-- | Functionality for display of binary data. Seeing a visual representation of quantum random
--   data lets a user visually verify that it is indeed random.
--
--   Usually to be imported via the __Quantum.Random__ module.
module Quantum.Random.Display (
  DisplayStyle (..),
  parseStyle,
  display
) where

import System.Console.ANSI
import System.Console.Ansigraph (AnsiColor (..), setFG, clear)
import Data.Word (Word8)
import Data.Bits (testBit)
import Data.Char (toLower)


-- | Represents the five supported methods for displaying binary data.
data DisplayStyle = Default
                  | Spins
                  | Bits
                  | ColorSpins
                  | ColorBits

-- | Parse a string to one of the supported display styles.
parseStyle :: String -> Maybe DisplayStyle
parseStyle (map toLower -> "colors")      = Just Default
parseStyle (map toLower -> "spins")       = Just Spins
parseStyle (map toLower -> "bits")        = Just Bits
parseStyle (map toLower -> "binary")      = Just Bits
parseStyle (map toLower -> "colorspins")  = Just ColorSpins
parseStyle (map toLower -> "colorbits")   = Just ColorBits
parseStyle (map toLower -> "colorbinary") = Just ColorBits
parseStyle _                              = Nothing


---- Interpreting as characters ----

spinChar :: Bool -> Char
spinChar False = '↑'
spinChar True  = '↓'

spinStr :: [Bool] -> String
spinStr = map spinChar

bitChar :: Bool -> Char
bitChar False = '0'
bitChar True  = '1'

bitStr :: [Bool] -> String
bitStr = map bitChar


---- Interpreting as colors ----

boolColor :: [Bool] -> AnsiColor
boolColor [False,False,False,False] = AnsiColor Dull Black
boolColor [False,False,False,True]  = AnsiColor Vivid Black
boolColor [False,False,True,False]  = AnsiColor Dull Red
boolColor [False,False,True,True]   = AnsiColor Vivid Red
boolColor [False,True,False,False]  = AnsiColor Dull Green
boolColor [False,True,False,True]   = AnsiColor Vivid Green
boolColor [False,True,True,False]   = AnsiColor Dull Yellow
boolColor [False,True,True,True]    = AnsiColor Vivid Yellow
boolColor [True,False,False,False]  = AnsiColor Dull Blue
boolColor [True,False,False,True]   = AnsiColor Vivid Blue
boolColor [True,False,True,False]   = AnsiColor Dull Magenta
boolColor [True,False,True,True]    = AnsiColor Vivid Magenta
boolColor [True,True,False,False]   = AnsiColor Dull Cyan
boolColor [True,True,False,True]    = AnsiColor Vivid Cyan
boolColor [True,True,True,False]    = AnsiColor Dull White
boolColor [True,True,True,True]     = AnsiColor Vivid White
boolColor _ = error "boolColor: Bool list not of length 4"

colorBlock :: AnsiColor -> IO ()
colorBlock c = setSGR [setFG c] *> putStr "█" *> clear


---- Byte display functions ----

w8bools :: Word8 -> [Bool]
w8bools w = testBit w <$> [0..7]

bitColorShow :: [Bool] -> IO ()
bitColorShow [a,b,c,d,e,f,g,h] = do
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h]
  colorBlock (boolColor l1)
  putStr (bitStr l1)
  colorBlock (boolColor l2)
  putStr (bitStr l2)
bitColorShow _ = error "bitColorShow: Bool list not of length 8"

bitShow :: [Bool] -> IO ()
bitShow [a,b,c,d,e,f,g,h] = do
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h]
  putStr (bitStr l1)
  putStr (bitStr l2)
bitShow _ = error "bitShow: Bool list not of length 8"

spinColorShow :: [Bool] -> IO ()
spinColorShow [a,b,c,d,e,f,g,h] = do
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h]
  colorBlock (boolColor l1)
  putStr (spinStr l1)
  colorBlock (boolColor l2)
  putStr (spinStr l2)
spinColorShow _ = error "spinColorShow: Bool list not of length 8"

spinShow :: [Bool] -> IO ()
spinShow [a,b,c,d,e,f,g,h] = do
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h]
  putStr (spinStr l1)
  putStr (spinStr l2)
spinShow _ = error "spinShow: Bool list not of length 8"

colorShow :: [Bool] -> IO ()
colorShow [a,b,c,d,e,f,g,h] = do
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h]
  colorBlock (boolColor l1)
  colorBlock (boolColor l2)
colorShow _ = error "colorShow: Bool list not of length 8"


---- Interpreting as display IO actions ----

displayByte :: DisplayStyle -> Word8 -> IO ()
displayByte Default    = colorShow . w8bools
displayByte Spins      = spinShow . w8bools
displayByte Bits       = bitShow . w8bools
displayByte ColorSpins = spinColorShow . w8bools
displayByte ColorBits  = bitColorShow . w8bools

displayBytes :: DisplayStyle -> [Word8] -> IO ()
displayBytes = mapM_ . displayByte

-- | Display a given list of bytes with the specified display style.
display :: DisplayStyle -> [Word8] -> IO ()
display s l = displayBytes s l *> putStrLn ""
