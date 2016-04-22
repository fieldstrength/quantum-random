module QRN.Display (
  showDataColors,
  showSpins,
  showBits,
  DisplayStyle (..),
  display
) where

import System.Console.ANSI
import System.Console.Ansigraph
import Data.Word (Word8)
import Data.Bits (testBit)


boolColor :: [Bool] -> AnsiColor
boolColor [False,False,False,False] = AnsiColor Dull Black
boolColor [False,False,False,True] = AnsiColor Vivid Black
boolColor [False,False,True,False] = AnsiColor Dull Red
boolColor [False,False,True,True] = AnsiColor Vivid Red
boolColor [False,True,False,False] = AnsiColor Dull Green
boolColor [False,True,False,True] = AnsiColor Vivid Green
boolColor [False,True,True,False] = AnsiColor Dull Yellow
boolColor [False,True,True,True] = AnsiColor Vivid Yellow
boolColor [True,False,False,False] = AnsiColor Dull Blue
boolColor [True,False,False,True] = AnsiColor Vivid Blue
boolColor [True,False,True,False] = AnsiColor Dull Magenta
boolColor [True,False,True,True] = AnsiColor Vivid Magenta
boolColor [True,True,False,False] = AnsiColor Dull Cyan
boolColor [True,True,False,True] = AnsiColor Vivid Cyan
boolColor [True,True,True,False] = AnsiColor Dull White
boolColor [True,True,True,True] = AnsiColor Vivid White
boolColor _ = error "boolColor: Bool list not multiple of 4 length"

colorBlock :: AnsiColor -> IO ()
colorBlock c = setSGR [setFG c] *> putStr "█" *> clear

colorBlocks :: [AnsiColor] -> IO ()
colorBlocks l = mapM_ colorBlock l *> putStrLn ""

toColors :: [Bool] -> [AnsiColor]
toColors (a:b:c:d:zs) = boolColor [a,b,c,d] : toColors zs
toColors []           = []
toColors _            = error "toColors: Bool list not multiple of 4 length"

w8bools :: Word8 -> [Bool]
w8bools w = testBit w <$> [0..7]

showDataColors :: [Word8] -> IO ()
showDataColors = colorBlocks . toColors . concat . map w8bools




spinChar :: Bool -> Char
spinChar False = '↑'
spinChar True  = '↓'

spinStr :: [Bool] -> String
spinStr = map spinChar

spinShow :: [Bool] -> IO ()
spinShow [a,b,c,d,e,f,g,h] =
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h] in do
        colorBlock (boolColor l1)
        putStr (spinStr l1)
        colorBlock (boolColor l2)
        putStr (spinStr l2)
spinShow _ = error "spinShow: Bool list not of length 8"

spins :: [Word8] -> IO ()
spins = mapM_ spinShow . map w8bools

showSpins :: [Word8] -> IO ()
showSpins l = spins l *> putStrLn ""

-- ⬇, ⬆, ⬍, ↑, ↓, ↕, ⇡, ⇣, ⇳,


bitChar :: Bool -> Char
bitChar False = '0'
bitChar True  = '1'

bitStr :: [Bool] -> String
bitStr = map bitChar

bitShow :: [Bool] -> IO ()
bitShow [a,b,c,d,e,f,g,h] =
  let l1 = [a,b,c,d]
      l2 = [e,f,g,h] in do
        colorBlock (boolColor l1)
        putStr (bitStr l1)
        colorBlock (boolColor l2)
        putStr (bitStr l2)
bitShow _ = error "boolShow: Bool list not of length 8"

bits :: [Word8] -> IO ()
bits = mapM_ bitShow . map w8bools

showBits :: [Word8] -> IO ()
showBits l = bits l *> putStrLn ""


data DisplayStyle = Default | Spins | Bits

display :: DisplayStyle -> [Word8] -> IO ()
display Default = showDataColors
display Spins   = showSpins
display Bits    = showBits
