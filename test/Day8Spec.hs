module Day8Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day8

main :: IO ()
main = hspec spec

countPx = countPixels . mkScreen

spec :: Spec
spec = do

  describe "Part A - count pixels" $ do

    it "Count for example" $ do
      countPx ["rect 3x2"] `shouldBe` 6

    it "Count for example 2" $ do
      countPx ["rect 3x2", "rotate column x=1 by 1"] `shouldBe` 6

