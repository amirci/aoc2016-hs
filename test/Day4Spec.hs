module Day4Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - Possible triangles" $ do

    it "calculates all the possible triangles" $ do
      contents <- readFile "test/day3.input.txt"
      possTri contents `shouldBe` 982

  describe "Part B - Possible triangles" $ do

    it "calculates all the possible triangles" $ do
      contents <- readFile "test/day3.input.txt"
      possTriB contents `shouldBe` 1826


