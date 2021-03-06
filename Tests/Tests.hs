module Main where

import Test.HUnit

import Data.Ix
import Data.SuffixArray
import Data.CountingSort
import qualified Data.Vector as V

testCountOccurences :: (Ix a, Ord a, Bounded a, Show a)  => [a] -> [Int] -> Test
testCountOccurences testData answer = TestCase (
    assertEqual ("countOccurences " ++ (show testData))
        answer (V.toList $ countOccurences (V.fromList testData))
    )

testPartialSums :: [Int] -> [Int] -> Test
testPartialSums testData answer = TestCase (
    assertEqual ("partialSums " ++ (show testData))
        answer (V.toList $ partialSums $ V.fromList testData)
    )

testCountingSort :: String -> [Int] -> [Int] -> Test
testCountingSort testList testIndexes answer = TestCase (
    assertEqual
        ("countingSort " ++ (show testList) ++ " " ++ (show testIndexes))
        answer (V.toList (countingSort (V.fromList testList) (V.fromList testIndexes)))
    )

testSuffixArray :: String -> [Int] -> Test
testSuffixArray testData answer = TestCase (
    assertEqual ("suffixArray " ++ (show testData))
        answer (V.toList myAnswer)
    )
    where SuffixArray _ myAnswer = suffixArray $ V.fromList testData

testComposeLists :: [Int] -> [Int] -> [Int] -> Test
testComposeLists testData testIndexes answer = TestCase (
    assertEqual
        ("composeLists" ++ (show testData) ++ " " ++ (show testIndexes))
        answer (V.toList (composeLists testData' (V.fromList testIndexes)))
    )
    where testData' = V.fromList testData

tests = TestList
        [ TestLabel "countOccurences tests" $ TestList
            [ testCountOccurences "abc" [1, 1, 1]
            , testCountOccurences "abb" [1, 2]
            , testCountOccurences "hello" [1, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1]
            , testCountOccurences (([1, 1, 1])::[Int]) [3]
            , testCountOccurences (([9, 10, 12])::[Int]) [1, 1, 0, 1]
            , testCountOccurences "aabc" [2, 1, 1]
            ]
        , TestLabel "partialSums tests" $ TestList
            [ testPartialSums [1, 1, 1] [1, 2, 3]
            , testPartialSums [1, 0, 5, 2] [1, 1, 6, 8]
            ]
        , TestLabel "countingSort tests" $ TestList
            [ testCountingSort "bcb" [0, 1, 2] [0, 2, 1]
            , testCountingSort "bcb" [2, 1, 0] [2, 0, 1]
            ]
        , TestLabel "suffixArray tests" $ TestList
            [ testSuffixArray "abcb" [0, 3, 1, 2]
            , testSuffixArray "aaba" [3, 0, 1, 2]
            , testSuffixArray "abc" [0, 1, 2]
            , testSuffixArray "eefdcba" [6, 5, 4, 3, 0, 1, 2]
            -- the difference in the following 2 tests is due to the fact
            -- that the algorithm builds ordered cyclic shifts of the string.
            -- to get an actual suffix array, a "sentinel" element that is
            -- lexicographically smaller than all other elements in the input
            -- must be appended to the input sequence. see 
            -- https://github.com/VictorDenisov/suffixarray/issues/1 for details.
            , testSuffixArray "ababcabab" [5, 7, 0, 2, 6, 8, 1, 3, 4]
            , testSuffixArray "ababcabab\0" [9, 7, 5, 0, 2, 8, 6, 1, 3, 4]
            ]
        , TestLabel "composeLists tests" $ TestList
            [ testComposeLists [0, 1, 2] [0, 1, 2] [0, 1, 2]
            , testComposeLists [0, 1, 2] [1, 0, 2] [1, 0, 2]
            , testComposeLists [3, 0, 2, 1] [0, 2, 1, 3] [3, 2, 0, 1]
            ]
        ]

main = runTestTT tests
