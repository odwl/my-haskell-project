{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lambda.UserAddressCityTest (userAddressCityTests) where

import Control.Lens
import Lambda.UserAddressCity
import Test.Tasty
import Test.Tasty.HUnit

userAddressCityTests :: TestTree
userAddressCityTests =
  testGroup
    "UserAddressCity Tests"
    [ testGroup
        "Record Tests"
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
      testGroup
        "Tuple Tests"
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
            let updatedTuple = over (_2 . _1) (* 11) (1, (5, "Ho"), 2)
            updatedTuple ^. _2 . _1 @?= 55
            updatedTuple @?= (1, (55, "Ho"), 2),
          testCase "Tuple lens test list" $ do
            let res0 = view (_1 . _head) (["Hi", "Ho"], ["He", "Hu"])
            res0 @?= "Hi"
            let res = view (_1 . ix 0) (["Hi", "Ho"], ["He", "Hu"])
            res @?= "Hi"
            let res1 = preview (_1 . ix 0) (["Hi", "Ho"], ["He", "Hu"]) -- winner
            res1 @?= Just "Hi"
            let res2 = preview (_1 . folded) (["Hi", "Ho"], ["He", "Hu"])
            res2 @?= Just "Hi"
            let res3 = view (_1 . to head) (["Hi", "Ho"], ["He", "Hu"]) -- bad
            res3 @?= "Hi"
            let res4 = view (_1 . traversed . filtered (== "Hi")) (["Hi", "He"], ["He", "Hu"])
            res4 @?= "Hi"
            let res5 = preview (_1 . traversed . filtered (== "Hi")) (["Hi", "Hi"], ["He", "Hu"])
            res5 @?= Just "Hi"
        ]
    ]

-- g) Finally, we no longer want to focus on just one element, but on "Hi" and "He" at the same
-- time. Write a lens that does exactly this.