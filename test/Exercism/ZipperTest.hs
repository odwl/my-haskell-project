{-# OPTIONS_GHC -Wno-orphans #-}

module Exercism.ZipperTest (zipperTests) where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Exercism.Zipper
  ( BinTree (BT),
    Zipper,
    asLeftChild,
    asRightChild,
    focusedTree,
    fromTree,
    left,
    mirror,
    next,
    prev,
    right,
    setLeft,
    setRight,
    setValue,
    toTree,
    up,
    value,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    conjoin,
    elements,
    frequency,
    property,
    sized,
    testProperty,
  )

-- | The master test tree for Zipper functionality.
zipperTests :: TestTree
zipperTests = testGroup "Zipper Tests" [tests, customTests, learningExerciseTests, quickCheckTests]

tests :: TestTree
tests =
  let leaf v = node v Nothing Nothing
      node v l r = Just (BT v l r :: BinTree Int)
      -- t1 is:
      --      1
      --     / \
      --    2   4
      --     \
      --      3
      t1 = BT 1 (node 2 Nothing $ leaf 3) $ leaf 4
      t2 = BT 1 (node 5 Nothing $ leaf 3) $ leaf 4
      t3 = BT 1 (node 2 (leaf 5) $ leaf 3) $ leaf 4
      t4 = BT 1 (leaf 2) $ leaf 4
      t5 = BT 6 (leaf 7) $ leaf 8
      t6 = BT 1 (node 2 Nothing $ leaf 3) $ node 6 (leaf 7) (leaf 8)
      t7 = BT 1 (node 2 Nothing $ leaf 5) $ leaf 4
   in testGroup
        "Tree Operations"
        [ testGroup
            "zipper"
            [ testCase "data is retained" $
                toTree (fromTree t1) @?= t1,
              testCase "left, right and value" $
                (value . fromJust . right . fromJust . left . fromTree) t1 @?= 3,
              testCase "dead end" $
                (left . fromJust . left . fromTree) t1 @?= Nothing,
              testCase "traversing up from top" $
                (up . fromTree) t1 @?= Nothing,
              testCase "left, right, and up" $
                (value . fromJust . right . fromJust . left . fromJust . up . fromJust . right . fromJust . up . fromJust . left . fromTree) t1 @?= 3,
              testCase "tree from deep focus" $
                (toTree . fromJust . right . fromJust . left . fromTree) t1 @?= t1,
              testCase "setValue" $
                (toTree . setValue 5 . fromJust . left . fromTree) t1 @?= t2,
              testCase "setValue after traversing up" $
                (toTree . setValue 5 . fromJust . up . fromJust . right . fromJust . left . fromTree) t1 @?= t2,
              testCase "setLeft with Just" $
                (toTree . setLeft (leaf 5) . fromJust . left . fromTree) t1 @?= t3,
              testCase "setRight with Nothing" $
                (toTree . setRight Nothing . fromJust . left . fromTree) t1 @?= t4,
              testCase "setRight with subtree" $
                (toTree . setRight (Just t5) . fromTree) t1 @?= t6,
              testCase "setValue on deep focus" $
                (toTree . setValue 5 . fromJust . right . fromJust . left . fromTree) t1 @?= t7,
              testCase "different paths to same zipper" $
                (right . fromJust . up . fromJust . left . fromTree) t1 @?= (right . fromTree) t1
            ],
          testGroup
            "mirror"
            [ testCase "Single Node" $
                mirror (BT 1 Nothing Nothing :: BinTree Int) @?= BT 1 Nothing Nothing,
              testCase "Left-Only Node" $
                mirror (BT 1 (Just (BT 2 Nothing Nothing)) Nothing :: BinTree Int)
                  @?= BT 1 Nothing (Just (BT 2 Nothing Nothing)),
              testCase "Node With Children" $
                mirror (BT 1 (Just (BT 2 Nothing Nothing)) (Just (BT 3 Nothing Nothing)) :: BinTree Int)
                  @?= BT 1 (Just (BT 3 Nothing Nothing)) (Just (BT 2 Nothing Nothing)),
              testCase "Left-Left Node" $
                mirror (BT 1 (Just (BT 2 (Just (BT 3 Nothing Nothing)) Nothing)) Nothing :: BinTree Int)
                  @?= BT 1 Nothing (Just (BT 2 Nothing (Just (BT 3 Nothing Nothing))))
            ]
        ]

customTests :: TestTree
customTests =
  testGroup
    "custom invariant tests"
    [ testCase "prev and next siblings" $
        let tree = BT 1 (Just (BT 2 Nothing Nothing)) (Just (BT 3 Nothing Nothing)) :: BinTree Int
            zip1 = fromTree tree
         in (value <$> (right zip1 >>= prev >>= next)) @?= Just 3
    ]

quickCheckTests :: TestTree
quickCheckTests =
  testGroup
    "QuickCheck properties"
    [ testProperty "Zipper structural invariants" prop_ZipperInvariant,
      testProperty "BinTree structural invariants" prop_BinTreeInvariant
    ]

-- | QuickCheck property: BinTree structural invariants
prop_BinTreeInvariant :: BinTree Int -> Property
prop_BinTreeInvariant tree =
  conjoin
    [ property $ toTree (fromTree tree) == tree,
      property $ mirror (mirror tree) == tree
    ]

-- | Property: Zipper satisfies fundamental laws:
prop_ZipperInvariant :: Zipper Int -> Property
prop_ZipperInvariant z =
  conjoin
    [ property $ all (liftA2 (&&) (isJust . asLeftChild) (isNothing . asRightChild)) (left z),
      property $ all (liftA2 (&&) (isJust . asRightChild) (isNothing . asLeftChild)) (right z),
      property $ null (up z) == null (asLeftChild z <|> asRightChild z),
      property $ all ((== Just z) . next) (prev z),
      property $ all ((== Just z) . prev) (next z)
    ]

-- ----------------------------------------------------------------------------
-- Test To Practices
-- ----------------------------------------------------------------------------

-- | Tests specifically added for practice exercises.
learningExerciseTests :: TestTree
learningExerciseTests =
  testGroup
    "Learning Exercises"
    [ testCase "Selective Pruner exercise" $
        pruneAndModify initialTree @?= Just expectedTree,
      testCase "Mirror Deep to Deep exercise" $
        mirrorDeepToDeep mirrorInitialTree @?= Just mirrorExpectedTree
    ]
  where
    initialTree =
      BT
        "1"
        (Just $ BT "2" (Just $ BT "5" Nothing Nothing) (Just $ BT "6" Nothing Nothing))
        (Just $ BT "3" (Just $ BT "4" Nothing Nothing) (Just $ BT "7" Nothing Nothing))

    expectedTree =
      BT
        "1"
        (Just $ BT "2" (Just $ BT "5" Nothing Nothing) (Just $ BT "6 Modified" Nothing Nothing))
        (Just $ BT "3 Revised" Nothing (Just $ BT "7" Nothing Nothing))

    pruneAndModify tree = do
      node3 <- right (fromTree tree)
      let modNode3 = setLeft Nothing (setValue "3 Revised" node3)
      node6 <- up modNode3 >>= left >>= right
      return $ toTree (setValue "6 Modified" node6)

    mirrorDeepToDeep tree = do
      z6 <- left (fromTree tree) >>= right
      let t6Mirrored = mirror (focusedTree z6)
      z3 <- up z6 >>= up >>= right
      return $ toTree (setRight (Just t6Mirrored) z3)

    mirrorInitialTree =
      BT
        (1 :: Int)
        ( Just $
            BT
              2
              (Just $ BT 5 Nothing Nothing)
              (Just $ BT 6 (Just $ BT 8 Nothing Nothing) (Just $ BT 9 Nothing Nothing))
        )
        (Just $ BT 3 Nothing Nothing)

    mirrorExpectedTree =
      BT
        (1 :: Int)
        ( Just $
            BT
              2
              (Just $ BT 5 Nothing Nothing)
              (Just $ BT 6 (Just $ BT 8 Nothing Nothing) (Just $ BT 9 Nothing Nothing))
        )
        ( Just $
            BT
              3
              Nothing
              (Just $ BT 6 (Just $ BT 9 Nothing Nothing) (Just $ BT 8 Nothing Nothing))
        )

-- ----------------------------------------------------------------------------
-- Arbitrary Instances & Generators
-- ----------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (BinTree a) where
  arbitrary = sized arbTree
    where
      arbTree 0 = BT <$> arbitrary <*> pure Nothing <*> pure Nothing
      arbTree n =
        BT
          <$> arbitrary
          <*> frequency [(1, pure Nothing), (3, Just <$> arbTree (n `div` 2))]
          <*> frequency [(1, pure Nothing), (3, Just <$> arbTree (n `div` 2))]

data Move = GoLeft | GoRight | GoUp | GoPrev | GoNext deriving (Eq, Show)

instance Arbitrary Move where
  arbitrary = elements [GoLeft, GoRight, GoUp, GoPrev, GoNext]

-- | Helper to safely apply a 'Move' to a zipper, bouncing off boundaries.
applyMove :: Move -> Zipper a -> Zipper a
applyMove GoLeft z = Data.Maybe.fromMaybe z (left z)
applyMove GoRight z = Data.Maybe.fromMaybe z (right z)
applyMove GoUp z = Data.Maybe.fromMaybe z (up z)
applyMove GoPrev z = Data.Maybe.fromMaybe z (prev z)
applyMove GoNext z = Data.Maybe.fromMaybe z (next z)

instance (Arbitrary a) => Arbitrary (Zipper a) where
  arbitrary = do
    tree <- arbitrary
    moves <- arbitrary
    return $ foldl (flip applyMove) (fromTree tree) (moves :: [Move])
