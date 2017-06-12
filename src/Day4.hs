module Day4 where

import Data.List.Split
import Data.List
import Debug.Trace
import Data.Char

type Room = String

sumRealRooms :: String -> Int
sumRealRooms = sum
             . map secId 
             . filter isReal 
             . map parseRoom
             . splitLines

findNPO :: String -> Int
findNPO = secId
        . head
        . filter isNpo
        . map parseRoom
        . splitLines

isNpo (sid, words, _) = (== "northpoleobjectstorage")
                      $ map decipher 
                      $ concat words
  where
    decipher = cipher sid

cipher sid c = chr
         $ (+ base)
         $ (`mod` 26)
         $ ord c + shift - base
  where
    base = ord 'a'
    shift = sid `mod` 26

splitLines = filter notEmpty . splitOn "\n"
notEmpty = (> 0) . length 
secId (sid, _, _) = sid

isReal (_, words, check) = check == check'
  where
    check' = map fst 
           $ take 5 
           $ sortBy mostCommon 
           $ freq 
           $ concat words
    mostCommon (a, l) (a', l') 
      | l  > l' = LT
      | l  < l' = GT
      | l == l' = compare a a'


parseRoom r = (readInt sectorId, init tokens, checksum)
  where
    tokens = splitOn "-" r
    [sectorId, checksum, _] = splitOneOf "[]" $ last tokens
    readInt = read :: String -> Int


freq :: (Eq a, Ord a) => [a] -> [(a, Int)]
freq = map (\g -> (head g, length g)) . group . sort

