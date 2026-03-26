{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lambda.UserAddressCityTest (userAddressCityTests) where

import Control.Lens
import Lambda.UserAddressCity
import Test.Tasty
import Test.Tasty.HUnit

userAddressCityTests :: TestTree
userAddressCityTests =
  testGroup
    "UserAddressCity Tests"
    [ testGroup "Record Tests"
        [ testCase "Create and access nested fields via dot notation" $ do
            let user = User "Alice" (Address (City "Tokyo") "Shibuya")
            view (address . city . name) user @?= "Tokyo",
          testCase "Use exampleAccess function" $ do
            let user = User "Alice" (Address (City "Tokyo") "Shibuya")
            exampleAccess user @?= City "Tokyo",
          testCase "Use exampleAccessName function" $ do
            let user = User "Alice" (Address (City "Tokyo") "Shibuya")
            exampleAccessName user @?= "Tokyo",
          testCase "Use updateCity function" $ do
            let user = User "Alice" (Address (City "Tokyo") "Shibuya")
            let newCity = City "London"
            let updatedUser = updateCity user newCity
            view (address . city . name) updatedUser @?= "London",
          testCase "Use updateCityName function" $ do
            let user = User "Alice" (Address (City "Tokyo") "Shibuya")
            let updatedUser = updateCityName user "London"
            view (address . city . name) updatedUser @?= "London"
        ],
      testGroup "Tuple Tests"
        [ testCase "Tuple lens test" $ do
            (1, ("Hi", "Ho"), 2) ^. _2 . _1 @?= "Hi",
          testCase "Tuple lens test set" $ do
            let updatedTuple = set (_2 . _1) 5 (1, ("Hi", "Ho"), 2)
            updatedTuple ^. _2 . _1 @?= 5
            updatedTuple @?= (1, (5, "Ho"), 2)
        ]
    ]


-- Change "Hi" to a integer value of your choice and bind the result to a name. Again, try the
-- operator and function.