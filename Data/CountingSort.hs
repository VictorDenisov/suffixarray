{- |
 - Module      : Data.CountingSort
 - License     : GPL2
 -
 - Stability   : experimental
 -}
module Data.CountingSort where

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
import Control.Monad (forM_, zipWithM_)

{- counting sort implementation -}

countingSort :: (Ix a, Ord a, Bounded a)
             => V.Vector a -> V.Vector Int -> V.Vector Int
countingSort s indexes = unsafePerformIO $ do
    let n = V.length s
    let rng = (V.minimum s, V.maximum s)
    let occurences = countOccurences s
    let p = partialSums occurences
    pp <- V.unsafeThaw p
    ans <- MVector.replicate n 0
    iforeachr s $ \i x -> do
        pos <- pp `MVector.read` (index rng x)
        MVector.write ans (pos - 1) (indexes V.! i)
        MVector.write pp (index rng x) (pos - 1)
    V.unsafeFreeze ans

iforeachr :: (Ix a, Ord a, Bounded a)
           => V.Vector a -> (Int -> a -> IO ()) -> IO ()
iforeachr s f = forr ((V.length s) - 1) f
    where
        forr 0 f = f 0 (s V.! 0)
        forr i f = do
                    f i (s V.! i)
                    forr (i - 1) f

{- partial sums implementation -}

partialSums :: V.Vector Int -> V.Vector Int
partialSums = V.postscanl (+) 0

            
{- count occurences implementation -}

countOccurences :: (Ix a, Ord a, Bounded a)
                => V.Vector a -> V.Vector Int
countOccurences s = unsafePerformIO $ do
    let rng = (V.minimum s, V.maximum s)
    let rs = rangeSize rng
    arr <- MVector.replicate rs 0
    V.forM_ s $ \c -> do
        let a = index rng c
        value <- MVector.read arr a
        MVector.write arr a (value + 1)
    V.unsafeFreeze arr

