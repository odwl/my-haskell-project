{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.RequiredTypeArgumentsTest (requiredTypeArgumentsTests) where

import Lambda.RequiredTypeArguments
  ( describeIntBoolPair
  , lengthTwo
  , lengthZero
  , sizeOfDouble
  , sizeOfInt
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

requiredTypeArgumentsTests :: TestTree
requiredTypeArgumentsTests =
  testGroup
    "GHC 9.10 RequiredTypeArguments & ExplicitNamespaces Crash Course"
    [ testGroup
        "Level 1: Required Positional Type Arguments (forall a ->)"
        [ testCase "sizeOfDirect gets the size of Int and Double directly via `(type T)`" $ do
            sizeOfInt @?= 8      -- 64-bit Int
            sizeOfDouble @?= 8   -- 64-bit Double
        ],
      testGroup
        "Level 2: Type-Level Computation Passed Directly"
        [ testCase "describeType extracts and formats compound type names directly" $ do
            describeIntBoolPair @?= "The type is: (Int, Bool)"
        ],
      testGroup
        "Level 3: Querying Promoted Peano Vectors with Required Type Arguments"
        [ testCase "vecLengthDirect inspects promoted length `(type n)` directly" $ do
            lengthZero @?= 0
            lengthTwo  @?= 2
        ]
    ]
