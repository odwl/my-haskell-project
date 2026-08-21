{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.DataKindsTest (dataKindsTests) where

import Lambda.DataKinds (Door (..), DoorState (..), HList (..), Vec (..), closeDoor, openDoor, vappend, vhead)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

dataKindsTests :: TestTree
dataKindsTests =
  testGroup
    "DataKinds & GHC 9.10 ListTuplePuns Crash Course"
    [ testGroup
        "Level 1: Promoted ADTs (DoorState & Permission)"
        [ testCase "can close an open door and open it back safely" $ do
            let openOak = MkDoor "Oak" :: Door 'Opened
                closedOak = closeDoor openOak
                reOpenedOak = openDoor closedOak
            reOpenedOak @?= openOak
        ],
      testGroup
        "Level 2: Promoted Peano Nats & Fixed-Length Vectors (Vec)"
        [ testCase "vappend adds type-level lengths statically and preserves elements" $ do
            let v1 = VCons (10 :: Int) VNil            -- Length 1 ('S 'Z)
                v2 = VCons 20 (VCons 30 VNil)          -- Length 2 ('S ('S 'Z))
                v3 = vappend v1 v2                     -- Length 1 + 2 = 3 ('S ('S ('S 'Z)))
            vhead v3 @?= 10
            -- If you tried calling `vhead VNil`, GHC would reject it at compile time because
            -- VNil has kind/type `Vec 'Z a`, but `vhead` strictly requires `Vec ('S n) a`!
        ],
      testGroup
        "Level 3: GHC 9.10 ListTuplePuns (Unticked Promoted Lists in HList)"
        [ testCase "heterogeneous list with unticked type list [Int, String, Bool]" $ do
            -- Look at this type signature! Notice there are NO leading ticks `'` on `[Int, String, Bool]`.
            -- GHC 9.10 ListTuplePuns unifies list syntax across values and types!
            let myHList :: HList [Int, String, Bool]
                myHList = HCons 42 (HCons "Hello, GHC 9.10!" (HCons True HNil))
            show myHList @?= "HCons 42 (HCons \"Hello, GHC 9.10!\" (HCons True HNil))"
        ]
    ]
