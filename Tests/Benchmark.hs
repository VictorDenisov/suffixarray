module Main where

import Data.Ix
import Data.SuffixArray
import Data.CountingSort

import qualified Data.Vector as V

main = interact doSuffixArr

--dumb s = s

doSuffixArr s = let SuffixArray a _ = suffixArray $ V.fromList s
             in (show (V.head a)) ++ "\n"
