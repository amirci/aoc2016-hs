module Day4 where

import Data.List.Split
import Data.List
import Debug.Trace

type Room = String

sumRealRooms :: String -> Int
sumRealRooms = sum
             . map secId 
             . filter isReal 
             . map parseRoom
             . splitLines
  where
    splitLines = filter notEmpty . splitOn "\n"
    notEmpty = (> 0) . length 
    secId (sid, _, _) = sid

isReal (_, chars, check) = check == check'
  where
    check' = map fst 
           $ take 5 
           $ sortBy mostCommon 
           $ freq chars
    mostCommon (a, l) (a', l') 
      | l  > l' = LT
      | l  < l' = GT
      | l == l' = compare a a'


parseRoom r = (readInt sectorId, chars, checksum)
  where
    tokens = splitOn "-" r
    chars = concat $ init tokens
    [sectorId, checksum, _] = splitOneOf "[]" $ last tokens
    readInt = read :: String -> Int


freq :: (Eq a, Ord a) => [a] -> [(a, Int)]
freq = map (\g -> (head g, length g)) . group . sort

