module LambdaTest where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Lambda

testSimpleSquare :: Test
testSimpleSquare = TestCase $ assertEqual "should square 2" 4 (square 2)
  where
    square = \x -> x^2

testDoubleApplication :: Test
testDoubleApplication = TestCase  $ assertEqual "should give 16 " 16 (applyTwice square 2)
  where
    applyTwice = \f -> (\x -> f (f x))
    square x = x^2

yFactTests :: Test 
yFactTests = TestLabel "YFact" $ TestList
    [ TestCase $ assertEqual "fact 0" 1 (fact 0)
    , TestCase $ assertEqual "fact 1" 1 (fact 1)
    , TestCase $ assertEqual "fact 2" 2 (fact 2)
    , TestCase $ assertEqual "fact 3" 6 (fact 3)
    ]

safeHeadTests :: Test
safeHeadTests = TestLabel "SafeHead" $ TestList
    [ TestCase $ assertEqual "Empty"     (Nothing :: Maybe Int) (safeHead [])
    , TestCase $ assertEqual "Populated" (Just "1")             (safeHead ["1", "2", "3"])
    ]

tests :: Test
tests = TestList
    [ TestLabel "Simple Square"    testSimpleSquare
    , TestLabel "Double Apply"     testDoubleApplication
    , yFactTests
    , safeHeadTests
    ]

-- tests = TestList


-- testHead :: Test
-- testHead = TestCase $ assertEqual "Should be Nothing" (Nothing :: Maybe Int) (safeHead [])

-- testHeadbis :: Test
-- testHeadbis = TestCase $ assertEqual "Should be Nothing" (Just "1") (safeHead ["1", "2", "ploc"])


        -- if null l then Nothing else Just (head l)



main :: IO ()
main = do
    putStrLn (head ["1", "2", "3"])
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
