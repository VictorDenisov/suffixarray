import Foreign.Marshal.Array (allocaArray, mallocArray)
import Data.Ix
import Data.List (foldl', (!!))
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits (shiftL)
import Data.Array.IArray

(<<) = shiftL

main = do
    s <- getLine
    putStrLn $ show $ suffixArray s

simpleEquator s i j = (s !! i) == (s !! j)
fancyEquator s h n i j = (s !! i) == (s !! j) && (s !! ((i + (1 << h)) `mod` n)) == (s !! ((j + (1 << h)) `mod` n))

suffixArray s = let p = countingSort s $ listArray (0, (n - 1)) [0..(n - 1)]
                    c = populateClassesBy (simpleEquator s) s p
                in go 0 p c
    where
        n = length s
        go h p c | (1 << h) >= n = (p, c)
        go h p c = let
            pn = shiftList n h p
            ck = composeLists c pn
            p' = countingSort ck $ listArray (0, (n - 1)) pn
            c' = populateClassesBy (fancyEquator c h n) c p'
            in go (h + 1) c' p'

shiftList :: Int -> Int -> [Int] -> [Int]
shiftList n h p = foldr step [] p
    where step v b = let x = (v - (1 << h)) 
                         x' = if (x < 0) then x + n else x
                     in x' : b
composeLists :: [Int] -> [Int] -> [Int]
composeLists c p = unsafePerformIO $ withArray c $ \cc -> withArray p $ \pp ->
    let go x arr = do
        a <- arr
        pe <- peekElemOff pp x
        ce <- peekElemOff cc pe
        pokeElemOff a x ce
        return a
    in do
    ans <- foldr go (mallocArray n) [0..(n - 1)]
    peekArray n ans
    where n = length p

populateClassesBy :: (Ix a, Ord a, Bounded a, Storable a, Show a) => (Int -> Int -> Bool) -> [a] -> [Int] -> [Int]
populateClassesBy equator s p = unsafePerformIO $ withArray s $ \ss -> withArray p $ \pp -> do
        ans <- populateClassesIO equator n ss pp
        peekArray n ans
    where n = (length s)

populateClassesIO :: (Ix a, Ord a, Bounded a, Storable a) => (Int -> Int -> Bool) -> Int -> Ptr a -> Ptr Int -> IO (Ptr Int)
populateClassesIO equator n s p = do
        arr <- mallocArray n
        let 
            go i classNum | i == n = return ()
            go i classNum = do
                pcur <- peekElemOff p i
                pprev <- peekElemOff p (i - 1)
                let newClassNum = if pcur `equator` pprev then classNum else classNum + 1
                pokeElemOff arr pcur (newClassNum - 1)
                go (i + 1) newClassNum
        go 1 1
        return arr


countingSort :: (Ix a, Ord a, Bounded a, Storable a, Show a) => [a] -> Array Int Int -> [Int]
countingSort s indexes = unsafePerformIO $ do 
        arr <- countingSortIO s indexes
        peekArray n arr
    where n = (length s)
          rng = ((minimum s), (maximum s))

countingSortIO :: (Ix a, Ord a, Bounded a, Storable a)  => [a] -> Array Int Int -> IO (Ptr Int)
countingSortIO s indexes = withArray s $ \ss -> let
        go (-1) p a = return a
        go i p a = do
            x <- peekElemOff ss i
            pos <- peekElemOff p (index rng x)
            pokeElemOff a (pos - 1) (indexes ! i)
            pokeElemOff p (index rng x) (pos - 1)
            go (i - 1) p a
        in do
            p <- partialSumsIO n (countOccurencesIO s)
            ans <- mallocArray n
            go (n - 1) p ans
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

