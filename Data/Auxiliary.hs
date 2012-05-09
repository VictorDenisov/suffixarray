module Data.Auxiliary where

import Data.Ix
import Data.List (foldl', (!!))
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits (shiftL)
import Data.Array.IArray
import Debug.Trace

{- countingSort implementation -}

countingSort :: (Ix a, Ord a, Bounded a, Storable a, Show a) => [a] -> Array Int Int -> [Int]
countingSort s indexes | trace ("countingSort: s - " ++ (show s) ++ ", indexes - " ++ (show indexes)) False = undefined
countingSort s indexes = unsafePerformIO $ do 
        arr <- countingSortIO s indexes
        peekArray n arr
    where n = length s

countingSortIO :: (Ix a, Ord a, Bounded a, Storable a)  => [a] -> Array Int Int -> IO (Ptr Int)
countingSortIO s indexes = withArray s $ \ss -> let
        go (-1) p a = return a
        go i p a = do
            x <- peekElemOff ss i -- x = s[i]
            pos <- peekElemOff p (index rng x) -- pos = p[x]
            pokeElemOff a (pos - 1) (indexes ! i) -- a [pos - 1] = indexes[i]
            pokeElemOff p (index rng x) (pos - 1) -- p[x] -= 1
            go (i - 1) p a
        in do
            p <- partialSumsIO n (countOccurencesIO s)
            ans <- mallocArray n
            go (n - 1) p ans
    where
        n = length s
        rng = (minimum s, maximum s)

{- partialSums implementation -}

partialSums :: [Int] -> [Int]
partialSums a | trace ("partialSums: " ++ (show a)) False = undefined
partialSums a = unsafePerformIO $ do
        arr <- partialSumsIO n (newArray a)
        peekArray n arr
    where n = length a

partialSumsIO :: Int -> IO (Ptr Int) -> IO (Ptr Int)
partialSumsIO n s = foldl' step s [1..(n - 1)]
    where
        step arr i = do
            a <- arr
            k <- peekElemOff a i
            prev <- peekElemOff a (i - 1)
            pokeElemOff a i (prev + k)
            return a
            
{- countOccurences implementation -}

countOccurences :: (Ix a, Ord a, Bounded a) => [a] -> [Int]
--countOccurences s | trace ("countOccurences: " ++ (show s)) False = undefined
countOccurences s = unsafePerformIO $ do
        arr <- countOccurencesIO s
        peekArray rs arr
    where
        rng = (minimum s, maximum s)
        rs = rangeSize rng

countOccurencesIO :: (Ix a, Ord a, Bounded a) => [a] -> IO (Ptr Int)
countOccurencesIO s = foldr step (mallocArray rs) s
    where
        rng = (minimum s, maximum s)
        rs = rangeSize rng
        step c pp = do
            p <- pp
            let a = index rng c
            x <- peekElemOff p a
            pokeElemOff p a (x + 1)
            return p

