--module Data.SuffixArray
--(suffixArray) where

import Data.Ix
import Data.List (foldl', (!!))
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits (shiftL)
import Data.Array.IArray
import Debug.Trace

(<<) = shiftL

main = do
    s <- getLine
    print $ suffixArray s

type Equator = Int -> Int -> Bool

simpleEquator s indexes i j = (s !! (indexes !! i)) == (s !! (indexes !! j))
fancyEquator s indexes h n i j = (s !! (indexes !! i)) == (s !! (indexes !! j)) && (s !! mid1) == (s !! mid2)
    where mid1 = (((indexes !! i) + (1 << h)) `mod` n)
          mid2 = (((indexes !! j) + (1 << h)) `mod` n)

-- |Generate a suffix array as list.
suffixArray :: (Ix a, Ord a, Bounded a, Storable a, Show a) => [a] -> ([Int], [Int])
suffixArray s = let p = countingSort s $ listArray (0, n - 1) [0..(n - 1)]
                    c = trace ("p = " ++ (show p)) $ populateClassesBy (simpleEquator s p) s p
                in trace ("c = " ++ (show c)) $ go 0 p c
    where
        n = length s
        go h p c | (1 << h) >= n = (p, c)
        go h p c = let
            pn = shiftList n h p
            ck = trace ("pn = " ++ (show pn)) $ composeLists c pn
            p' = trace ("cpn = " ++ (show ck)) $ countingSort ck $ listArray (0, n - 1) pn
            c' = trace ("p' = " ++ (show p')) $ populateClassesBy (fancyEquator c p' h n) c p'
            in trace ("c' = " ++ (show c')) $ go (h + 1) p' c'

{- axilliary?spelling functions -}

shiftList :: Int -> Int -> [Int] -> [Int]
shiftList n h p | trace ("shiftList: n - " ++ (show n) ++ ", h - " ++ (show h) ++ ", p - " ++ (show p)) False = undefined
shiftList n h p = foldr step [] p
    where step v b = let x = (v - (1 << h)) 
                         x' = if x < 0 then x + n else x
                     in x' : b

composeLists :: [Int] -> [Int] -> [Int]
composeLists c p | trace ("composeLists: c - " ++ (show c) ++ ", p - " ++ (show p)) False = undefined
composeLists c p = unsafePerformIO $ withArray c $ \cc -> withArray p $ \pp ->
    let go x arr = do
        a <- arr
        pe <- peekElemOff pp x
        ce <- peekElemOff cc pe
        pokeElemOff a x ce
        return a
    in do
    ans <- foldr go (mallocArray n) [0..(n - 1)]
    peekArray n ans where n = length p

{- populateClassesBy implementation -}

populateClassesBy :: (Ix a, Ord a, Bounded a, Storable a, Show a) => Equator -> [a] -> [Int] -> [Int]
populateClassesBy equator s p | trace ("populateClassesBy: s - " ++ (show s) ++ ", p - " ++ (show p)) False = undefined
populateClassesBy equator s p = unsafePerformIO $ withArray s $ \ss -> withArray p $ \pp -> do
        ans <- populateClassesIO equator n ss pp
        peekArray n ans
    where n = length s

populateClassesIO :: (Ix a, Ord a, Bounded a, Storable a) => Equator -> Int -> Ptr a -> Ptr Int -> IO (Ptr Int)
populateClassesIO equator n s p = do
        arr <- mallocArray n
        let 
            go i classNum | i == n = return ()
            go i classNum = do
                pcur <- peekElemOff p i
                let newClassNum = if i `equator` (i - 1) then classNum else classNum + 1
                pokeElemOff arr pcur (newClassNum - 1)
                go (i + 1) newClassNum
        go 1 1
        return arr

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

countOccurences :: (Ix a, Ord a, Bounded a, Show a) => [a] -> [Int]
countOccurences s | trace ("countOccurences: " ++ (show s)) False = undefined
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

