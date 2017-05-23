module Day2 where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as Map
import Debug.Trace

bathCode :: String -> [Int]
bathCode = map findCode . getCodeLines
  where
    findCode = fromJust . lookup' square . foldl walkSquare (1, 1)
    walkSquare = walkInside square

fancyBathCode :: String -> String
fancyBathCode = map findCode . getCodeLines
  where 
    findCode = toHex . fromJust . lookup' diamond . foldl walkDiamond (0, 2)
    walkDiamond = walkInside diamond
    toHex n 
      | n <= 9 = head $ show n
      | otherwise = chr $ n + 55

lookup' = flip Map.lookup

getCodeLines = filter notEmpty . splitOn "\n"
  where notEmpty = (> 0) . length 

walk (x, y) 'U' = (x, y - 1)
walk (x, y) 'D' = (x, y + 1)
walk (x, y) 'L' = (x - 1, y)
walk (x, y) 'R' = (x + 1, y)

walkInside m old d = if inside then new else old
  where 
    new = walk old d
    inside = Map.member new m

square = Map.fromList [((x, y), toNum x y) | x <- [0..2], y <- [0..2]]
  where
    toNum x y = x + y * 3 + 1

diamond = Map.fromList $ row0 ++ row1 ++ row2 ++ row3 ++ row4
  where
    row0 = [((2, 0), 1)]
    row1 = [((i, 1), i + 1) | i <- [1..3]]
    row2 = [((i, 2), i + 5) | i <- [0..4]]
    row3 = [((i, 3), i + 9) | i <- [1..3]]
    row4 = [((2, 4), 13)]

