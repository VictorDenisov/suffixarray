module Main where

import Test.HUnit

import Data.Ix
import Data.SuffixArray
import Data.Auxiliary

testCountOccurences :: (Ix a, Ord a, Bounded a, Show a)  => [a] -> [Int] -> Test
testCountOccurences testData answer = TestCase (
    assertEqual ("countOccurences " ++ (show testData))
        answer (countOccurences testData)
    )

tests = TestLabel "countOccurences tests" $ TestList
                 [ testCountOccurences "abc" [1, 1, 1]
                 , testCountOccurences "abb" [1, 2]
                 , testCountOccurences "hello" [1, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1]
                 , testCountOccurences (([1, 1, 1])::[Int]) [3]
                 , testCountOccurences "aabc" [2, 1, 1]]

main = runTestTT tests
