module Day3 where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as Map
import Debug.Trace

possTri :: String -> Int
possTri = length . filter possible . parseTriangles

parseTriangles = map parse . splitLines
possible [s1, s2, s3] = s1 + s2 > s3
parse = sort . map readInt . filter notEmpty . splitOn " "

splitLines = filter notEmpty . splitOn "\n"
notEmpty = (> 0) . length 
readInt = read :: String -> Int

