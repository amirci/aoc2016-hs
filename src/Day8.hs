module Day8 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec

bOff = '.'
bOn  = '#'
totalRows = 5
totalCols = 60

type Screen = Mat.Matrix Char
type ScreenFn = Screen -> Screen

empty = Mat.matrix totalRows totalCols (const bOff)

mkScreen :: [String] -> Screen
mkScreen = foldl (flip apply) empty

countPixels :: Screen -> Int
countPixels = length . filter (== bOn) . Mat.toList

apply :: String -> ScreenFn
apply = unwrap . parse instrParser "(unknown)"
  where
    unwrap = either (const $ const empty) id

instrParser = try rectParser 
          <|> rotateParser

shift :: Int -> [a] -> [a]
shift n = reverse . shift' . reverse
  where shift' l = drop n l ++ take n l

-- rect nxm
turnOnRect :: Int -> Int -> ScreenFn
turnOnRect w h s = foldl setM s pts
  where 
    setM = flip $ Mat.setElem bOn
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
    shifted = shift n $ Vec.toList $ Mat.getCol (col + 1) s
    replace col = foldl setM s $ zip col [1..totalRows]
    setM s (e, row) = Mat.setElem e (row, col + 1) s

rotateRow row n s = replace shifted
  where
    shifted = shift n $ Vec.toList $ Mat.getRow (row + 1) s
    replace row = foldl setM s $ zip row [1..totalCols]
    setM s (e, col) = Mat.setElem e (row + 1, col) s

setRow :: [a] -> Int -> Mat.Matrix a -> Mat.Matrix a
setRow row = Mat.mapRow (\c _ -> row !! (c - 1))

rotateParser :: Parsec String st ScreenFn
rotateParser = do
  string "rotate"
  spaces
  fn <- rotateFnParser
  spaces
  oneOf "xy"
  char '='
  tgt <- read <$> many1 digit
  spaces
  string "by"
  spaces
  n <- read <$> many1 digit
  return $ fn tgt n

rotateFnParser :: Parsec String st (Int -> Int -> ScreenFn)
rotateFnParser = chooseFn <$> rowOrCol
  where
    rowOrCol = try (string "column") <|> string "row"
    chooseFn "column" = rotateCol
    chooseFn "row" = rotateRow
  
