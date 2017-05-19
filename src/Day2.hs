module Day2 where

import Data.List.Split
import Data.List
import Data.Maybe


bathCode :: String -> [Int]
bathCode = map findCode . filter notEmpty . splitOn "\n"
  where
    notEmpty = (> 0) . length 

findCode = adjust . foldl walk (1, 1)

adjust (x, y) = x + y * 3 + 1

walk (x, y) 'U' = (x, max (y - 1) 0)
walk (x, y) 'D' = (x, min (y + 1) 2)
walk (x, y) 'L' = (max (x - 1) 0, y)
walk (x, y) 'R' = (min (x + 1) 2, y)
