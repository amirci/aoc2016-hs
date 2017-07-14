module Day8 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec

bOff = '.'
bOn  = '#'
totalRows = 6
totalCols = 50

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
    instrParser = try rectParser <|> rotateParser

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
  width <- numberParser
  char 'x'
  height <- numberParser
  return $ turnOnRect width height

-- rotate column x=1 by 1
rotateCol :: Int -> Int -> ScreenFn
rotateCol col n s = setCol shifted (col + 1) s
  where shifted = shift n $ Vec.toList $ Mat.getCol (col + 1) s

rotateRow row n s = setRow shifted (row + 1) s
  where shifted = shift n $ Vec.toList $ Mat.getRow (row + 1) s

setRow :: [a] -> Int -> Mat.Matrix a -> Mat.Matrix a
setRow row = Mat.mapRow (\c _ -> row !! (c - 1))

setCol col = Mat.mapCol (\c _ -> col !! (c - 1))

rotateParser :: Parsec String st ScreenFn
rotateParser = do
  string "rotate"
  spaces
  fn <- rotateFnParser
  spaces
  oneOf "xy"
  char '='
  tgt <- numberParser
  spaces
  string "by"
  spaces
  n <- numberParser
  return $ fn tgt n

rotateFnParser :: Parsec String st (Int -> Int -> ScreenFn)
rotateFnParser = chooseFn <$> rowOrCol
  where
    rowOrCol = try (string "column") <|> string "row"
    chooseFn "column" = rotateCol
    chooseFn "row" = rotateRow
  
numberParser :: Parsec String st Int
numberParser = read <$> many1 digit
