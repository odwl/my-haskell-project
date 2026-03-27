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
            let updatedTuple2 = (1, ("Hi", "Ho"), 2) & _2 . _1 .~ 5
            updatedTuple ^. _2 . _1 @?= 5
            updatedTuple @?= (1, (5, "Ho"), 2)
            updatedTuple2 ^. _2 . _1 @?= 5
            updatedTuple2 @?= (1, (5, "Ho"), 2), 
          testCase "Tuple lens test mult" $ do
            let updatedTuple = over (_2 . _1) (*11) (1, (5, "Ho"), 2)
            updatedTuple ^. _2 . _1 @?= 55
            updatedTuple @?= (1, (55, "Ho"), 2),
          testCase "Tuple lens test list" $ do
            let res = view (_1 . folded) (["Hi", "Ho"], ["He", "Hu"])
            res @?= "Hi"
            let res2 = preview (_1 . folded) (["Hi", "Ho"], ["He", "Hu"])
            res2 @?= Just "Hi"
        ]
    ]

-- Change "Hi" to a integer value of your choice and bind the result to a name. Again, try the
-- operator and function.d) Finally, use the lens on the updated tuple, to multiply the integer value you set in the last
-- task with 11.
-- e) Now we change things up a little bit. The tuple now contains lists of stings, with "Hi" being
-- in the first one: (["Hi", "Ho"], ["He", "Hu"]). Change your lens so it still focuses on
-- "Hi".