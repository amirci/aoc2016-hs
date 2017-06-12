module Day6 where

import Data.List.Split
import Data.List
import Debug.Trace
import Data.Ord

origMsg :: String -> String
origMsg = findCommon maximumBy

origMsgB = findCommon minimumBy

findCommon f = map common . transpose . splitLines
  where
    common = fst . f (comparing snd) . freq

splitLines = filter notEmpty . splitOn "\n"
notEmpty = (> 0) . length 

freq :: (Eq a, Ord a) => [a] -> [(a, Int)]
freq = map (\g -> (head g, length g)) . group . sort

