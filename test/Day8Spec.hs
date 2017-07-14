module Day8Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split
import Data.Matrix

import Day8

main :: IO ()
main = hspec spec

countPx = countPixels . mkScreen

display = map putStrLn . toLists


spec :: Spec
spec = do

  describe "Part A - count pixels" $ do

    it "Count for example" $ do
      countPx ["rect 3x2"] `shouldBe` 6

    it "Count for example 2" $ do
      countPx ["rect 3x2", "rotate column x=1 by 1"] `shouldBe` 6

    it "Count for example 3" $ do
      let instr = ["rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4"]
      countPx instr `shouldBe` 6

    it "Counts pixes from applying the file" $ do
      contents <- lines <$> readFile "test/day8.input.txt"
      countPx contents `shouldBe` 115

    -- part B :  EFEYKFRFIJ 
