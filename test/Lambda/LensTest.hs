{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lambda.LensTest (lensTests) where

import Control.Lens
import Lambda.Lens
import Test.Tasty
import Test.Tasty.HUnit

lensTests :: TestTree
lensTests =
  testGroup
    "Lens Tests"
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
            res5 @?= Just "Hi",
          testCase "Tuple lens test mult" $ do
            let res = toListOf (both . ix 0) (["Hi", "Ho"], ["He", "Hu"])
            res @?= ["Hi", "He"]
            let res1 = (["Hi", "Ho"], ["He", "Hu"]) ^.. both . ix 0
            res1 @?= ["Hi", "He"]
            let res2 = over (both . ix 0) (++ "!") (["Hi", "Ho"], ["He", "Hu"])
            res2 @?= (["Hi!", "Ho"], ["He!", "Hu"])
            let res25 = (["Hi", "Ho"], ["He", "Hu"]) & both . ix 0 %~ (++ "!")
            res25 @?= (["Hi!", "Ho"], ["He!", "Hu"])
            let res26 = (["Hi", "Ho"], ["He", "Hu"]) & both . ix 0 .~ "XX"
            res26 @?= (["XX", "Ho"], ["XX", "Hu"])
            -- Since `preview` returns a `Maybe`, we use `liftA2 (,)` to safely combine the two Maybes into a Tuple!
            let res3 = toListOf (each . ix 0) (["Hi", "Ho"], ["He", "Hu"], ["Hi", "Ho"])
            res3 @?= ["Hi", "He", "Hi"]
        ],
      testGroup
        "FileSystem Tests"
        [ testCase "Extract metadata from FileSystem using Prism" $ do
            let myFileSystem = File $ Doc Text (Metadata "config.json" "admin") ""
            -- Must use ^? (preview) because _File is a Prism and might fail if the node is a Folder!
            myFileSystem ^? _File . metadata @?= Just (Metadata "config.json" "admin")
            myFileSystem ^? _File . metadata . fileName @?= Just "config.json"
            example ^? _File . metadata @?= Nothing
            example ^? _Folder . _2 . ix 0 . _File . metadata . fileName @?= Just ".zshenv",
          testCase "search" $ do
              -- Use our custom recursive Traversal!
              let res = toListOf (allDocuments . metadata . fileName) example
              res @?= [".zshenv", ".zshenv", ".zsh_history"]
        ],
      testGroup
        "Recursive Search Tests"
        [ testCase "searchFile finds all matching documents" $ do
            let found = searchFile example ".zshenv"
            length found @?= 2
            -- Verify they are actually the correct documents
            (found ^.. traversed . metadata . owner) @?= ["root", "luke"],
          
          testCase "documentFlatList extracts all documents" $ do
            let allDocs = documentFlatList example
            length allDocs @?= 3
            (allDocs ^.. traversed . metadata . fileName) @?= [".zshenv", ".zshenv", ".zsh_history"],

          testCase "documentExist correctly identifies existing files" $ do
            documentExist example ".zsh_history" @?= True
            documentExist example "does_not_exist.txt" @?= False
        ]
    ]

example :: FileSystem
example = Folder "root" 
  [ File $ Doc Text (Metadata ".zshenv" "root") ""
  , Folder "home" 
    [ Folder "luke" 
      [ File $ Doc Text (Metadata ".zshenv" "luke") "export EDITOR=nvim"
      , File $ Doc Text (Metadata ".zsh_history" "luke") "sudo dnf rm java"
      ]
    ]
  ]
