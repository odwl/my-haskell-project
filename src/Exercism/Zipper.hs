module Exercism.Zipper
  ( BinTree (..),
    Zipper,
    fromTree,
    left,
    right,
    prev,
    next,
    setLeft,
    setRight,
    setValue,
    toTree,
    up,
    value,
    modifyTree,
    setTree,
    focusedTree,
    mirror,
  )
where

import Data.List (foldl')

data BinTree a = BT
  { btValue :: a,
    btLeft :: Maybe (BinTree a),
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show)

data Crumb a
  = LeftCrumb a (Maybe (BinTree a))
  | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show)

data Zipper a = Zip [Crumb a] (BinTree a) deriving (Eq, Show)

-- | Create a zipper from a binary tree.
fromTree :: BinTree a -> Zipper a
fromTree = Zip []

-- | Reconstruct a binary tree from a zipper.
-- toTree :: Zipper a -> BinTree a
-- toTree z@(Zip _ tree) = maybe tree toTree (up z)

applyCrumb :: BinTree a -> Crumb a -> BinTree a
applyCrumb tree (LeftCrumb v r) = BT v (Just tree) r
applyCrumb tree (RightCrumb v l) = BT v l (Just tree)

toTree :: Zipper a -> BinTree a
toTree (Zip crumbs tree) = foldl' applyCrumb tree crumbs 

-- | Get the value of the node in focus.
value :: Zipper a -> a
value (Zip _ tree) = btValue tree

-- | Move to the focus's previous sibling (e.g. from right child to left child).
prev :: Zipper a -> Maybe (Zipper a)
prev (Zip (RightCrumb v (Just l) : crumbs) tree) = Just $ Zip (LeftCrumb v (Just tree) : crumbs) l
prev _ = Nothing

-- | Move to the focus's next sibling (e.g. from left child to right child).
next :: Zipper a -> Maybe (Zipper a)
next (Zip (LeftCrumb v (Just r) : crumbs) tree) = Just $ Zip (RightCrumb v (Just tree) : crumbs) r
next _ = Nothing

-- | Move the focus to the left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zip crumbs (BT v ml mr)) = Zip (LeftCrumb v mr : crumbs) <$> ml

-- | Move the focus to the right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zip crumbs (BT v ml mr)) = Zip (RightCrumb v ml : crumbs) <$> mr

-- | Move the focus to the parent. 
up :: Zipper a -> Maybe (Zipper a)
up (Zip (crumb : crumbs) tree) = Just $ Zip crumbs (applyCrumb tree crumb) 
up _ = Nothing

-- | Apply a modification function to the focused subtree.
modifyTree :: (BinTree a -> BinTree a) -> Zipper a -> Zipper a
modifyTree f (Zip crumbs tree) = Zip crumbs (f tree)

-- | Replace the entire focused subtree.
setTree :: BinTree a -> Zipper a -> Zipper a
-- setTree newTree (Zip crumbs _) = Zip crumbs newTree
setTree = modifyTree . const 

-- | Set the value of the node in focus.
setValue :: a -> Zipper a -> Zipper a
setValue x = modifyTree (\t -> t {btValue = x})

-- | Set the left child of the focused node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l = modifyTree (\t -> t {btLeft = l})

-- | Set the right child of the focused node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r = modifyTree (\t -> t {btRight = r})

-- | Get the entire binary tree currently under focus.
focusedTree :: Zipper a -> BinTree a
focusedTree (Zip _ tree) = tree

-- | Recursively swap all left and right children.
mirror :: BinTree a -> BinTree a
mirror (BT v l r) = BT v (fmap mirror r) (fmap mirror l)
