module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Lambda

import Test.QuickCheck

-- The rule: Mapping 'id' over a Maybe should equal the original Maybe.
prop_functorIdentity :: Maybe Int -> Bool
prop_functorIdentity m = fmap id m == m

qcToHUnit :: String -> (Maybe Int -> Bool) -> Test
qcToHUnit name prop = TestLabel name $ TestCase $ do
    -- quickCheckResult runs the test and returns a Result object
    result <- quickCheckResult prop 
    -- We tell HUnit to fail if the QuickCheck result isn't a success
    assertBool "QuickCheck found a failing property!" (isSuccess result)

(-->) :: (Show a, Eq a) => a -> a -> Test
got --> expected = TestCase $ assertEqual "" expected got

-- applyTwice = \f -> (\x -> f (f x))
square :: Int -> Int
square x = x^2

testSimpleSquare :: Test
testSimpleSquare = TestLabel "Simple Square" $ TestList
    [ square 2 --> 4 ]

testDoubleApplication :: Test
testDoubleApplication = TestLabel "Double Apply" $ TestList
    [ applyTwice square 2 --> 16
    , applyTwice (+1) 10  --> 12
    ]


yFactTests :: Test 
yFactTests = TestLabel "YFact" $ TestList
    [ fact 0 --> 1
    , fact 1 --> 1
    , fact 2 --> 2
    , fact 3 --> 6
    ]

safeHeadTests :: Test
safeHeadTests = TestLabel "SafeHead" $ TestList 
    [ safeHead ([] :: [Int]) --> Nothing
    , safeHead [1,2,3] --> Just 1
    , safeHead [10] --> Just 10 
    ]

addMaybesTests :: Test 
addMaybesTests = TestLabel "AddMaybe" $ TestList
    [ addMaybes (Just 1) (Just 1) --> Just 2
    , addMaybes Nothing  (Just 1) --> Nothing
    , addMaybes (Just 1) Nothing  --> Nothing
    , addMaybes Nothing  Nothing  --> Nothing
    ] 

safeDivTests :: Test
safeDivTests = TestLabel "SafeDiv" $ TestList
    [ safeDiv 10 2 --> Just 5
    , safeDiv 10 0 --> Nothing
    , safeDiv 10 3 --> Just 3
    , safeDiv 0 10 --> Just 0
    ]

functorIdTest :: Test
functorIdTest = TestLabel "FunctorId" $ TestList    
    [ fmap id (Just 10) --> Just 10
    , fmap id (Nothing :: Maybe Int) --> Nothing
    ]

tests :: Test
tests = TestList
    [ testSimpleSquare
    , testDoubleApplication
    , yFactTests
    , safeHeadTests
    , addMaybesTests
    , safeDivTests
    , functorIdTest
    , TestLabel "Old HUnit Test" $ TestCase (assertEqual "" (Just 10) (fmap id (Just 10)))
    , qcToHUnit "QuickCheck: Functor Identity" prop_functorIdentity
    ]

main :: IO ()
main = do
    putStrLn "\n--- Running: Lambda Suite ---"
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
