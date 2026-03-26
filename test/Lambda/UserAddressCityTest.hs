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
            (1, ("Hi", "Ho"), 2) ^. _2 . _1 @?= "Hi"
        ]
    ]
