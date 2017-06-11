module Day5Spec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Split

import Day5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Part A - find password" $ do

    it "finds password using MD5" $ do
      findPwd "reyedfim" `shouldBe` "f97c354d"

