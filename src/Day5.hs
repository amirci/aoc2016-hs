module Day5 where

import Data.List.Split
import Data.List
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5
import Control.Applicative
import qualified Data.Map.Strict as Map

findPwd :: String -> String
findPwd = map (!! 5)
        . take 8 
        . filter sw5z 
        . map toMd5
        . pwdSeq 

findPwdB = Map.elems
         . head
         . dropWhile (not . fullPwd)
         . scanl assignPwd emptyPwd
         . filter (sw5z .&&. validPos)
         . map toMd5
         . pwdSeq 
  where
    zeroSeven c = c >= '0' && c <= '7'
    validPos = zeroSeven . (!! 5)
    (.&&.) = liftA2 (&&)
    emptyPwd = Map.empty
    fullPwd p = Map.keys p == "01234567"
    assignPwd p hsh = Map.insert (hsh !! 5) (hsh !! 6) p

toMd5 = show . md5 . B.pack
sw5z = (== "00000") . take 5
pwdSeq s = map ((s ++) . show) $ [0..]
