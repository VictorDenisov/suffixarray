module Data.SuffixArray
( suffixArray
, simpleEquator
, fancyEquator
, shiftList
, composeLists
, populateClassesBy
) where

import Data.Ix
import Data.List (foldl', (!!))
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits (shiftL)
import Debug.Trace
import qualified Data.Vector as V

import Data.CountingSort

{- |Generate a suffix array as list. -}
suffixArray :: (Ix a, Ord a, Bounded a)
            => [a] -> (V.Vector Int, [Int])
suffixArray s = let p = countingSort s (V.generate n id)
                    equator = simpleEquator (V.fromList s) p
                    c = populateClassesBy equator p
                in go 0 p c
    where
        n = length s
        go h p c | (1 << h) >= n = (p, c)
        go h p c = let
            pn = shiftList n h p
            ck = V.toList $ composeLists (V.fromList c) pn
            p' = countingSort ck pn
            equator = fancyEquator (V.fromList c) p' h n
            c' =  populateClassesBy equator p'
            in go (h + 1) p' c'

{- axiliary functions -}

(<<) = shiftL

{- Equator is a function that takes two indexes and returns true if values
 - pointed by them are equal.
 -}
type Equator = Int -> Int -> Bool

simpleEquator :: (Ix a, Ord a, Bounded a)
               => V.Vector a -> V.Vector Int -> Int -> Int -> Bool
simpleEquator s indexes i j = (s V.! (indexes V.! i)) == (s V.! (indexes V.! j))

fancyEquator :: (Ix a, Ord a, Bounded a)
             => V.Vector a -> V.Vector Int -> Int -> Int -> Int -> Int -> Bool
fancyEquator s indexes h n i j
    = (s V.! i') == (s V.! j') && (s V.! mid1) == (s V.! mid2)
    where mid1 = ((i' + (1 << h)) `mod` n)
          mid2 = ((j' + (1 << h)) `mod` n)
          i' = indexes V.! i
          j' = indexes V.! j


shiftList :: Int -> Int -> V.Vector Int -> V.Vector Int
shiftList n h p = V.map step p
    where step v = let x = (v - (1 << h))
                       x' = if x < 0 then x + n else x
                     in x'

{- Build composition of two lists. First argument is source list.
 - Second argument is vector of indexes. Elements of first list should
 - be reordered accordingly to indexes in the second argument.
 -}
composeLists :: V.Vector Int -> V.Vector Int -> V.Vector Int
composeLists c p = V.map (c V.!) p

{- populateClassesBy implementation
 -}

populateClassesBy :: Equator -> V.Vector Int -> [Int]
populateClassesBy equator p = unsafePerformIO $ do
        ans <- populateClassesIO equator p
        peekArray n ans
    where n = V.length p

populateClassesIO :: Equator -> V.Vector Int -> IO (Ptr Int)
populateClassesIO equals p = do
        let n = V.length p
        arr <- mallocArray n
        let 
            go i classNum | i == n = return ()
            go i classNum = do
                let pcur = p V.! i
                let newClassNum = if i `equals` (i - 1)
                                    then classNum
                                    else classNum + 1
                pokeElemOff arr pcur (newClassNum - 1)
                go (i + 1) newClassNum
        go 1 1
        return arr

