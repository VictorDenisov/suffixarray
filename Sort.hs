import Foreign.Marshal.Array (allocaArray, mallocArray)
import Data.Ix
import Data.List (foldl')
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr

main = do
    s <- getLine
    putStrLn $ show $ (countingSort s)

--populateClasses :: (Ix a, Ord a, Bounded a, Storable a) => [a] -> [Int] -> [Int]
--populateClasses s p = withArray s $ \s

countingSort :: (Ix a, Ord a, Bounded a, Storable a) => [a] -> [Int]
countingSort s = unsafePerformIO $ do 
        arr <- countingSortIO s
        peekArray n arr
    where n = (length s)
          rng = ((minimum s), (maximum s))

countingSortIO :: (Ix a, Ord a, Bounded a, Storable a) => [a] -> IO (Ptr Int)
countingSortIO s = withArray s $ \ss -> let
        go (-1) p a = return a
        go i p a = do
            x <- peekElemOff ss i
            pos <- peekElemOff p (index rng x)
            pokeElemOff a (pos - 1) i
            go (i - 1) p a
    in do
        p <- partialSumsIO n (countOccurencesIO s)
        ans <- mallocArray n
        go (n - 1) ans p
    where
        n = (length s)
        rng = ((minimum s), (maximum s))

partialSums :: [Int] -> [Int]
partialSums a = unsafePerformIO $ do
        arr <- partialSumsIO n (newArray a)
        peekArray n arr
    where n = (length a)

partialSumsIO :: Int -> IO (Ptr Int) -> IO (Ptr Int)
partialSumsIO n s = do foldl' step s [1..(n - 1)]
    where
        step arr i = do
            a <- arr
            k <- peekElemOff a i
            prev <- peekElemOff a (i - 1)
            pokeElemOff a i (prev + k)
            return a

--countOccurencesIO :: 

countOccurences :: (Ix a, Ord a, Bounded a) => [a] -> [Int]
countOccurences s = unsafePerformIO $ do 
        arr <- countOccurencesIO s
        peekArray rs arr
    where
        rng = ((minimum s), (maximum s))
        rs = rangeSize rng

countOccurencesIO :: (Ix a, Ord a, Bounded a) => [a] -> IO (Ptr Int)
countOccurencesIO s = do foldr step (mallocArray rs) s
    where
        rng = ((minimum s), (maximum s))
        rs = rangeSize rng
        step c pp = do
            p <- pp
            let a = index rng c
            x <- peekElemOff p a
            pokeElemOff p a (x + 1)
            return p

