module Main where

import Data.Ix
import Data.SuffixArray
import Data.CountingSort

main = interact $ doSuffixArr

dumb s = s

doSuffixArr s = let (a, _) = suffixArray s
             in (show (head a)) ++ "\n"
