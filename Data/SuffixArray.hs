{- |
 - Module      : Data.SuffixArray
 - Copyright   : (c) 2010 Daniël de Kok (c) 2012 Victor Denisov
 - License     : GPL2
 -
 - Maintainer  : Daniël de Kok <me@danieldk.eu> Victor Denisov <denisovenator@gmail.com>
 - Stability   : experimental
 -
 - Construction of suffix arrays (arrays ordered by suffix). Given an
 - array /d/ elements, the suffix array is a sorted array of the sub-arrays
 - in /d/. For instance, the suffix array of /banana apple pear apple/ is:
 -
 - * apple
 -
 - * apple pear apple
 -
 - * banana apple pear apple
 -
 - * pear apple
 -}

module Data.SuffixArray
( SuffixArray(..)
, suffixArray
, simpleEquator
, fancyEquator
, shiftList
, composeLists
, populateClassesBy
, fromList
, toList
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
import qualified Data.Vector.Generic.Mutable as MVector

import Data.CountingSort

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
                 deriving Show

{- |
 - 'elems' provides a vector of each element in the suffix array. One element
 - of the suffix array contains the full data array.
 -}
elems :: SuffixArray a -> V.Vector (V.Vector a)
elems (SuffixArray d i) = V.map vecAt i
    where vecAt idx = V.drop idx d

{- |
 - 'fromList' constructs a suffix array from a list of elements.
 -}
fromList :: (Ix a, Ord a, Bounded a) => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

{- |
 - 'toList' constructs a list from a suffix array.
 -}
toList :: SuffixArray a -> [[a]]
toList (SuffixArray d i) = V.foldr vecAt [] i
    where vecAt idx l = V.toList (V.drop idx d) : l 

{- |Generate a suffix array as list. -}
suffixArray :: (Ix a, Ord a, Bounded a)
            => V.Vector a -> SuffixArray a
suffixArray s = let p = countingSort s (V.generate n id)
                    equator = simpleEquator s p
                    c = populateClassesBy equator p
                in go 0 p c
    where
        n = V.length s
        go h p c | (1 << h) >= n = SuffixArray s p
        go h p c = let
            pn = shiftList n h p
            ck = V.toList $ composeLists c pn
            p' = countingSort (V.fromList ck) pn
            equator = fancyEquator c p' h n
            c' =  populateClassesBy equator p'
            in go (h + 1) p' c'

{- axiliary functions -}

(<<) = shiftL

{- Equator is a function that takes two indexes and returns true if values
 - pointed by them are equal.
 -}
type Equator = Int -> Int -> Bool

simpleEquator :: (Ix a, Ord a, Bounded a)
               => V.Vector a -> V.Vector Int -> Equator
simpleEquator s indexes i j = (s V.! (indexes V.! i)) == (s V.! (indexes V.! j))

fancyEquator :: (Ix a, Ord a, Bounded a)
             => V.Vector a -> V.Vector Int -> Int -> Int -> Equator
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
composeLists c indexes = V.map (c V.!) indexes

{- populateClassesBy implementation
 -}

populateClassesBy :: Equator -> V.Vector Int -> V.Vector Int
populateClassesBy equals p = unsafePerformIO $ do
    let n = V.length p
    arr <- MVector.replicate n 0
    let
        go i classNum | i == n = return ()
        go i classNum = do
            let pcur = p V.! i
            let newClassNum = if i `equals` (i - 1)
                                then classNum
                                else classNum + 1
            MVector.write arr pcur (newClassNum - 1)
            go (i + 1) newClassNum
    go 1 1
    V.unsafeFreeze arr

