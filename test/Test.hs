module Main where
import TestLex
import Test.Tasty
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [allLexTests]