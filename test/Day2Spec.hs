module Day2Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Bathroom code" $ do
    it "ULL returns 1" $ do
     bathCode "ULL\nRRDDD" `shouldBe` [1,9]

    it "calculates the code from the file" $ do
      contents <- readFile "test/day2.input.txt"
      bathCode contents `shouldBe` [6, 5, 5, 5, 6]


