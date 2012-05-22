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
import Control.Monad (forM_, zipWithM_)

{- counting sort implementation -}

countingSort :: (Ix a, Ord a, Bounded a, Storable a, Show a) => [a] -> [Int] -> [Int]
countingSort s indexes = V.toList
                       $ unsafePerformIO
                       $ countingSortIO s indexes

countingSortIO :: (Ix a, Ord a, Bounded a, Storable a, Show a)  => [a] -> [Int] -> IO (V.Vector Int)
countingSortIO s indexes = do
    let n = length s
    let rng = (minimum s, maximum s)
    let ind = V.fromList indexes
    let ss = V.fromList s
    let occurences = countOccurences s
    let p = partialSums occurences
    pp <- V.unsafeThaw p
    ans <- MVector.replicate n 0
    iforeach s $ \i x -> do
        pos <- pp `MVector.read` (index rng x)
        MVector.write ans (pos - 1) (ind V.! i)
        MVector.write pp (index rng x) (pos - 1)
    V.unsafeFreeze ans

iforeach :: (Ix a, Ord a, Bounded a, Storable a, Show a)  => [a] -> (Int -> a -> IO ()) -> IO ()
iforeach s f = zipWithM_ f (reverse [0..(n - 1)]) (reverse s)
    where
        n = length s

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

