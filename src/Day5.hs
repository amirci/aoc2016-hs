module Day5 where

import Data.List.Split
import Data.List
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5

findPwd :: String -> String
findPwd = map theSixth
        . take 8 
        . filter sw5z 
        . map toMd5
        . pwdSeq 
  where
    theSixth = (!! 5)
    toMd5 = show . md5 . B.pack
    sw5z = (== "00000") . take 5
    pwdSeq s = map ((s ++) . show) $ [0..]
