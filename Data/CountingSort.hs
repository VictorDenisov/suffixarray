module Data.CountingSort where

import Data.Ix
import Data.List (foldl', (!!))
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits (shiftL)
import Data.Array.IArray
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MVector
import Control.Monad (forM_)

{- counting sort implementation -}

countingSort :: (Ix a, Ord a, Bounded a, Storable a) => [a] -> Array Int Int -> [Int]
countingSort s indexes = unsafePerformIO $ do 
        arr <- countingSortIO s indexes
        peekArray n arr
    where n = length s

countingSortIO :: (Ix a, Ord a, Bounded a, Storable a)  => [a] -> Array Int Int -> IO (Ptr Int)
countingSortIO s indexes = withArray s $ \ss -> let
        go (-1) p a = return a
        go i p a = do
            x <- peekElemOff ss i -- x = s[i]
            pos <- p `MVector.read` (index rng x) -- pos = p[x]
            pokeElemOff a (pos - 1) (indexes ! i) -- a [pos - 1] = indexes[i]
            MVector.write p (index rng x) (pos - 1) -- p[x] -= 1
            go (i - 1) p a
        in do
            let occurences = countOccurences s
            let p = partialSums occurences
            ans <- mallocArray n
            pp <- V.unsafeThaw p
            go (n - 1) pp ans
    where
        n = length s
        rng = (minimum s, maximum s)

{- partial sums implementation -}


partialSums :: V.Vector Int -> V.Vector Int
partialSums = V.postscanl (+) 0

            
{- count occurences implementation -}

countOccurences :: (Ix a, Ord a, Bounded a) => [a] -> V.Vector Int
countOccurences s = unsafePerformIO $ do
    let rng = (minimum s, maximum s)
    let rs = rangeSize rng
    arr <- MVector.replicate rs 0
    forM_ s $ \c -> do
        let a = index rng c
        value <- MVector.read arr a
        MVector.write arr a (value + 1)
    V.unsafeFreeze arr

