module Day8 where

import Data.List
import Debug.Trace
import Text.Parsec 
import Data.Bifunctor
import Data.Matrix

bOff = '.'
bOn  = '#'

type Screen = Matrix Char

mkScreen :: [String] -> Screen
mkScreen instructions = matrix 5 60 (const bOff)


countPixels :: Screen -> Int
countPixels = length . filter (== bOn) . toList
