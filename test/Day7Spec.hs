module Day7Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - count how may ips support TLS" $ do

    it "checks the string supports TLS" $ do
      tlsSupport "abba[bab]xyz" `shouldBe` True

    it "How many pwds support TLS" $ do
      contents <- readFile "test/day7.input.txt"
      ipCount tlsSupport contents `shouldBe` 115


  describe "Part B - count how may ips support SSL" $ do

    it "checks the string supports SSL" $ do
      sslSupport "aba[bab]xyz" `shouldBe` True

    it "How many pwds support SSL" $ do
      contents <- readFile "test/day7.input.txt"
      ipCount sslSupport contents `shouldBe` 231

