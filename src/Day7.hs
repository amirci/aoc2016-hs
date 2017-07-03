module Day7 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor


ipCount :: (String -> Bool) -> String -> Int
ipCount f = length . filter f . lines

-- Part A
tlsSupport :: String -> Bool
tlsSupport = supernetButNotHypernet . findAbba . parseLine
  where
    supernetButNotHypernet = (== (True, False))
    hasAbba = or . map anyAbba
    findAbba = bimap hasAbba hasAbba
    anyAbba = notEmpty . filter isAbba . cuartets
      where
        isAbba (a, b, c, d) = a == d && b == c && a /= b
        cuartets s = zip4 s (drop 1 s) (drop 2 s) (drop 3 s)

-- Part B
sslSupport :: String -> Bool
sslSupport = abaCorrespondsBab
           . second (map flipBab)
           . bimap abaTriplets abaTriplets
           . parseLine
  where
    abaCorrespondsBab (abas, babs) = notEmpty $ intersect abas babs
    flipBab (a, b, _) = (b, a, b)
    abaTriplets = filter isAba . concatMap triplets
    triplets s = zip3 s (drop 1 s) (drop 2 s)
    isAba (a, b, c) = a == c && a /= b

parseLine = unzip
          . fromRight 
          . parse lineParser "(source)"

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

