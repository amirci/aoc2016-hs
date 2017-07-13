module Day8 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor
import Data.Matrix

bOff = '.'
bOn  = '#'

type Screen = Matrix Char

empty = matrix 5 60 (const bOff)

mkScreen :: [String] -> Screen
mkScreen = foldl apply empty

countPixels :: Screen -> Int
countPixels = length . filter (== bOn) . toList

apply :: Screen -> String -> Screen
apply s instr = s

turnOnRect :: Int -> Int -> Screen -> Screen
turnOnRect w h s = foldl setM s pts
  where 
    setM = flip $ setElem bOn
    pts = [(x, y) | x <- [1..h], y <- [1..w]]

rectParser :: Parsec String st (Int, Int)
rectParser = do
  string "rect"
  spaces
  width <- read <$> many1 digit
  char 'x'
  height <- read <$> many1 digit
  return (width, height)

