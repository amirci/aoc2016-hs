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

  describe "Fancy Bath Code " $ do
    it "returns 1 for ULL" $ do
      fancyBathCode "ULL\nRRDDD" `shouldBe` "5D"

    it "returns B for RRDD" $ do
      fancyBathCode "RRD" `shouldBe` "B"

    it "calculates the code from the file" $ do
      contents <- readFile "test/day2.input.txt"
      fancyBathCode contents `shouldBe` "CB779"
