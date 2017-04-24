module Day1Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "blocksToHq" $ do
    it "Does L2 R3" $ do
      blocksToHq "L2, R3" `shouldBe` 5

    it "Does R2 x 3" $ do
      blocksToHq "R2, R2, R2" `shouldBe` 2

    it "calculates the blocks to HQ from the file" $ do
      contents <- readFile "test/day1.input.txt"
      (blocksToHq $ head $ splitOn "\n" contents) `shouldBe` 273

  describe "firstLocationTwice" $ do
    it "R8, R4, R4, R8" $ do
      firstLocTwice "R8, R4, R4, R8" `shouldBe` 4

    it "calculates first block twice to HQ from the file" $ do
      contents <- readFile "test/day1.input.txt"
      (firstLocTwice $ head $ splitOn "\n" contents) `shouldBe` 115

