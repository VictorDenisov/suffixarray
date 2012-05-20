module Main where

import Test.HUnit

import Data.Ix
import Data.SuffixArray
import Data.CountingSort

testCountOccurences :: (Ix a, Ord a, Bounded a, Show a)  => [a] -> [Int] -> Test
testCountOccurences testData answer = TestCase (
    assertEqual ("countOccurences " ++ (show testData))
        answer (countOccurences testData)
    )

testPartialSums :: [Int] -> [Int] -> Test
testPartialSums testData answer = TestCase (
    assertEqual ("partialSums " ++ (show testData))
        answer (partialSums testData)
    )

tests = TestList
            [ TestLabel "countOccurences tests" $ TestList
                 [ testCountOccurences "abc" [1, 1, 1]
                 , testCountOccurences "abb" [1, 2]
                 , testCountOccurences "hello" [1, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1]
                 , testCountOccurences (([1, 1, 1])::[Int]) [3]
                 , testCountOccurences (([9, 10, 12])::[Int]) [1, 1, 0, 1]
                 , testCountOccurences "aabc" [2, 1, 1]]
            , TestLabel "partialSums tests" $ TestList
                 [ testPartialSums [1, 1, 1] [1, 2, 3]
                 , testPartialSums [1, 0, 5, 2] [1, 1, 6, 8]
                 ]
            ]

main = runTestTT tests
