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
    asLeftChild,
    asRightChild,
    mirror,
  )
where

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
toTree :: Zipper a -> BinTree a
toTree z@(Zip _ tree) = maybe tree toTree (up z)

-- | Get the value of the node in focus.
value :: Zipper a -> a
value (Zip _ tree) = btValue tree

-- | Internal helper: Checks if the zipper is currently at a left child.
asLeftChild :: Zipper a -> Maybe (Zipper a)
asLeftChild z@(Zip (LeftCrumb {} : _) _) = Just z
asLeftChild _ = Nothing

-- | Internal helper: Checks if the zipper is currently at a right child.
asRightChild :: Zipper a -> Maybe (Zipper a)
asRightChild z@(Zip (RightCrumb {} : _) _) = Just z
asRightChild _ = Nothing

-- | Move to the focus's previous sibling (e.g. from right child to left child).
prev :: Zipper a -> Maybe (Zipper a)
prev z = asRightChild z >>= up >>= left

-- | Move to the focus's next sibling (e.g. from left child to right child).
next :: Zipper a -> Maybe (Zipper a)
next z = asLeftChild z >>= up >>= right

-- | Move the focus to the left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zip crumbs (BT v ml mr)) = Zip (LeftCrumb v mr : crumbs) <$> ml

-- | Move the focus to the right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zip crumbs (BT v ml mr)) = Zip (RightCrumb v ml : crumbs) <$> mr

-- | Move the focus to the parent.
up :: Zipper a -> Maybe (Zipper a)
up (Zip (LeftCrumb v r : crumbs) tree) = Just $ Zip crumbs (BT v (Just tree) r)
up (Zip (RightCrumb v l : crumbs) tree) = Just $ Zip crumbs (BT v l (Just tree))
up _ = Nothing

-- | Apply a modification function to the focused subtree.
modifyTree :: (BinTree a -> BinTree a) -> Zipper a -> Zipper a
modifyTree f (Zip crumbs tree) = Zip crumbs (f tree)

-- | Replace the entire focused subtree.
setTree :: BinTree a -> Zipper a -> Zipper a
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
