module Day3 where

import Data.List.Split
import Data.List
import Debug.Trace

possTri :: String -> Int
possTri = length 
        . filter possible 
        . map parse
        . splitLines
  where 
    parse = sort . readNumbers

possTriB = length 
         . filter possible 
         . map sort
         . concatMap transpose
         . chunksOf 3
         . map readNumbers
         . splitLines


possible [s1, s2, s3] = s1 + s2 > s3

splitLines = filter notEmpty . splitOn "\n"

readNumbers = map readInt . filter notEmpty . splitOn " "
  where readInt = read :: String -> Int

notEmpty = (> 0) . length 


