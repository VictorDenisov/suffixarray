module Main where

import Test.HUnit

import Data.Ix
import Data.SuffixArray
import Data.CountingSort
import qualified Data.Vector as V

testCountOccurences :: (Ix a, Ord a, Bounded a, Show a)  => [a] -> [Int] -> Test
testCountOccurences testData answer = TestCase (
    assertEqual ("countOccurences " ++ (show testData))
        answer (V.toList $ countOccurences testData)
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
        answer (countingSort testList testIndexes)
    )

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
            ]

main = runTestTT tests
