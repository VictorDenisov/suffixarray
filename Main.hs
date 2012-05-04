module Main where

import Data.SuffixArray

main = do
    s <- getLine
    print $ suffixArray s

