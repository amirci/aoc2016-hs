module Day4Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - count real rooms" $ do

    it "Adds up the sector ids of real rooms" $ do
      rooms <- readFile "test/day4.input.txt"
      sumRealRooms rooms `shouldBe` 158835

  describe "Part B - decipher North Pole Objects" $ do

    it "Finds the sector ID for NPO room" $ do
      rooms <- readFile "test/day4.input.txt"
      findNPO rooms `shouldBe` 993


