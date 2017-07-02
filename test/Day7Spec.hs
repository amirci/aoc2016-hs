module Day7Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - count how may ips support ssl" $ do

    it "checks the string supports ssl" $ do
      sslSupport "abba[bab]xyz" `shouldBe` True

    it "How many pwds support TLS" $ do
      contents <- readFile "test/day7.input.txt"
      sslCount contents `shouldBe` 115

