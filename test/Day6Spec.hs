module Day6Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - find original message" $ do

    it "finds original message in the file" $ do
      contents <- readFile "test/day6.input.txt"
      origMsg contents `shouldBe` "qtbjqiuq"

