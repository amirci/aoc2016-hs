module Day1 where

import Data.List.Split
import Data.List
import Data.Maybe

data Direction = North | East | South | West
  deriving (Eq, Show, Enum, Bounded)

data Cmd = Righty | Lefty
  deriving (Eq, Show)

type Coord = (Int, Int)
type Pos = (Coord, Direction)
type Command = (Cmd, Int)

blocksToHq :: String -> Int
blocksToHq = sumCoords
           . fst
           . foldl move ((0, 0), North)
           . map parseCmd
           . splitOn ", "

firstLocTwice :: String -> Int
firstLocTwice = sumCoords
              . fromJust
              . fst4
              . foldl walk (Nothing, [], (0, 0), North)
              . map parseCmd
              . splitOn ", "

  where 
    fst4 (a, _, _, _) = a

move :: Pos -> Command -> Pos
move (p, dir) (cmd, n) = (p', dir')
  where
    dir' = changeDir dir cmd
    p'   = last $ walking dir' p n

walk found@(Just x, _, _, _) _ = found
walk (_, vis, pos, dir) (cmd, n) = (found, vis', pos', dir')
  where
    found  = listToMaybe $ intersect vis blocks
    dir'   = changeDir dir cmd
    pos'   = last blocks
    vis'   = vis ++ blocks
    blocks = walking dir' pos n


walking North (x, y) n = [(x, y) | y <- [y+1..y+n]]
walking East  (x, y) n = [(x, y) | x <- [x+1..x+n]]
walking West  (x, y) n = reverse [(x, y) | x <- [x-n..x-1]]
walking South (x, y) n = reverse [(x, y) | y <- [y-n..y-1]]

changeDir d Righty = if d == maxBound then minBound else succ d
changeDir d Lefty  = if d == minBound then maxBound else pred d

readInt = read :: String -> Int

sumCoords (x, y) = abs x + abs y

parseCmd ('L':xs) = (Lefty , readInt xs)
parseCmd ('R':xs) = (Righty, readInt xs)


