module Day8 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor
import Data.Matrix
import qualified Data.Vector as Vec

bOff = '.'
bOn  = '#'
totalRows = 5
totalCols = 60

type Screen = Matrix Char
type ScreenFn = Screen -> Screen

empty = matrix totalRows totalCols (const bOff)

mkScreen :: [String] -> Screen
mkScreen = foldl (flip apply) empty

countPixels :: Screen -> Int
countPixels = length . filter (== bOn) . toList

apply :: String -> ScreenFn
apply instr = unwrap $ parse rectParser "(unknown)" instr
  where
    unwrap = either (const id) id

shift :: Int -> [a] -> [a]
shift n = reverse . shift' . reverse
  where shift' l = drop n l ++ take n l

-- rect nxm
turnOnRect :: Int -> Int -> ScreenFn
turnOnRect w h s = foldl setM s pts
  where 
    setM = flip $ setElem bOn
    pts = [(x, y) | x <- [1..h], y <- [1..w]]

rectParser :: Parsec String st ScreenFn
rectParser = do
  string "rect"
  spaces
  width <- read <$> many1 digit
  char 'x'
  height <- read <$> many1 digit
  return $ turnOnRect width height

-- rotate column x=1 by 1
rotateCol :: Int -> Int -> ScreenFn
rotateCol col n s = replace shifted
  where
    shifted = shift n $ Vec.toList $ getCol (col + 1) s
    replace col = foldl setM s $ zip col [1..totalRows]
    setM s (e, row) = setElem e (row, col + 1) s

rotateColParser :: Parsec String st ScreenFn
rotateColParser = do
  string "rotate"
  spaces
  string "column"
  spaces
  char 'x'
  char '='
  col <- read <$> many1 digit
  spaces
  string "by"
  spaces
  n <- read <$> many1 digit
  return $ rotateCol col n

