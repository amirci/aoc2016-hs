module Day7 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor


sslCount :: String -> Int
sslCount = length . filter sslSupport . lines

sslSupport :: String -> Bool
sslSupport = supernetButNotHypernet . findAbba . parseLine
  where
    supernetButNotHypernet = (== (True, False))
    hasAbba = or . map anyAbba
    findAbba = bimap hasAbba hasAbba
    parseLine = unzip
              . fromRight 
              . parse lineParser "(source)"

anyAbba = notEmpty . filter isAbba . split4
  where
    isAbba (a, b, c, d) = a == d && b == c && a /= b
    split4 s = zip4 s (drop 1 s) (drop 2 s) (drop 3 s)

fromRight (Right s) = s
fromRight _ = []

notEmpty = (> 0) . length

lineParser = many1 abbaParser

abbaParser = (,) <$> abbaPart <*> option [] babPart

abbaPart = many1 letter

babPart = do
  char '['
  bab <- many1 letter
  char ']'
  return bab

