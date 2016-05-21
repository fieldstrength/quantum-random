module Main where

import Test.Hspec
import Test.QuickCheck
import Quantum.Random
import Quantum.Random.Interface hiding (main)


main :: IO ()
main = hspec $ do

  describe "parseStyle" $ do

    it "returns Nothing for unrecognized strings" $
      parseStyle "notastyle" `shouldBe` Nothing
