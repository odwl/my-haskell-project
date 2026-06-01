{-# LANGUAGE DeriveFunctor, PatternSynonyms, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric, FlexibleInstances #-}
module Exercism.Zipper
  ( BinTree (..),
    BinTreeZipper,
    Zipper,
    ListZipper,
    GenericZipper (..),
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
    Copointed (..),
  )
where
import Control.Category ((>>>))
import Control.Comonad (Comonad (..))
import Data.Copointed (Copointed (..))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

data BinTree a = BT
  { btValue :: a,
    btLeft :: Maybe (BinTree a),
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show, Functor)

instance Comonad BinTree where 
  extract (BT v _ _) = v
  duplicate tree@(BT _ ml mr) = BT tree (duplicate <$> ml) (duplicate <$> mr) 

-- For a deep dive into the algebra and calculus of data types (where the derivative 
-- of a type is its type of one-hole contexts), see:
-- https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types
data Crumb a
  = LeftCrumb a (Maybe (BinTree a))
  | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show, Functor)

type BinTreeZipper a = GenericZipper BinTree a
type Zipper a = BinTreeZipper a
type ListZipper a = GenericZipper NonEmpty a

pattern Zip :: [Crumb a] -> BinTree a -> BinTreeZipper a
pattern Zip c t = GenericZipper c t
{-# COMPLETE Zip #-}

-- | Type family representing the derivative/context of a Functor 'f'.
type family Context (f :: * -> *) a

data GenericZipper f a = GenericZipper
  { crumbs :: [Context f a]  -- The path/derivative context
  , focus  :: f a            -- The focused element/subtree
  }

type instance Context [] a = a
type instance Context NonEmpty a = a
-- For a Binary Tree, the context is exactly your custom 'Crumb' type:
type instance Context BinTree a = Crumb a

instance Eq a => Eq (GenericZipper BinTree a) where
  (GenericZipper c1 f1) == (GenericZipper c2 f2) = c1 == c2 && f1 == f2

instance Show a => Show (GenericZipper BinTree a) where
  show (GenericZipper c f) = "Zip " ++ show c ++ " " ++ show f

instance Functor (GenericZipper BinTree) where
  fmap f (GenericZipper c t) = GenericZipper (fmap (fmap f) c) (fmap f t)

instance Copointed (GenericZipper BinTree) where
  copoint = focus >>> btValue

instance Copointed (GenericZipper NonEmpty) where
  copoint = focus >>> NE.head

instance Eq a => Eq (GenericZipper NonEmpty a) where
  (GenericZipper c1 f1) == (GenericZipper c2 f2) = c1 == c2 && f1 == f2

instance Show a => Show (GenericZipper NonEmpty a) where
  show (GenericZipper c f) = "ListZip " ++ show c ++ " " ++ show f

instance Functor (GenericZipper NonEmpty) where
  fmap f (GenericZipper c t) = GenericZipper (fmap f c) (fmap f t)
  
  -- duplicate z = Zip (zipperCrumbs z) (zipperTree z)
  --   where
  --     zipperTree :: BinTreeZipper a -> BinTree (BinTreeZipper a)
  --     zipperTree x = BT x (zipperTree <$> left x) (zipperTree <$> right x)

  --     zipperCrumbs :: BinTreeZipper a -> [Crumb (BinTreeZipper a)]
  --     zipperCrumbs x = case up x of
  --       Nothing -> []
  --       Just p ->
  --         case x of
  --           Zip (LeftCrumb _ _ : _) _  -> LeftCrumb p (zipperTree <$> right p) : zipperCrumbs p
  --           Zip (RightCrumb _ _ : _) _ -> RightCrumb p (zipperTree <$> left p) : zipperCrumbs p
  --           _                          -> []

-- | Get the value of the node in focus.
value :: BinTreeZipper a -> a
value = copoint

-- | Create a zipper from a binary tree.
fromTree :: BinTree a -> BinTreeZipper a
fromTree = Zip []

-- | Reconstruct a binary tree from a zipper.
-- toTree :: BinTreeZipper a -> BinTree a
-- toTree z@(Zip _ tree) = maybe tree toTree (up z)

applyCrumb :: BinTree a -> Crumb a -> BinTree a
applyCrumb tree (LeftCrumb v r) = BT v (Just tree) r
applyCrumb tree (RightCrumb v l) = BT v l (Just tree)

toTree :: BinTreeZipper a -> BinTree a
toTree (Zip crumbs tree) = foldl' applyCrumb tree crumbs 

-- | Move to the focus's previous sibling (e.g. from right child to left child).
prev :: BinTreeZipper a -> Maybe (BinTreeZipper a)
prev (Zip (RightCrumb v (Just l) : crumbs) tree) = Just $ Zip (LeftCrumb v (Just tree) : crumbs) l
prev _ = Nothing

-- | Move to the focus's next sibling (e.g. from left child to right child).
next :: BinTreeZipper a -> Maybe (BinTreeZipper a)
next (Zip (LeftCrumb v (Just r) : crumbs) tree) = Just $ Zip (RightCrumb v (Just tree) : crumbs) r
next _ = Nothing

-- | Move the focus to the left child.
left :: BinTreeZipper a -> Maybe (BinTreeZipper a)
left (Zip crumbs (BT v ml mr)) = Zip (LeftCrumb v mr : crumbs) <$> ml

-- | Move the focus to the right child.
right :: BinTreeZipper a -> Maybe (BinTreeZipper a)
right (Zip crumbs (BT v ml mr)) = Zip (RightCrumb v ml : crumbs) <$> mr

-- | Move the focus to the parent. 
up :: BinTreeZipper a -> Maybe (BinTreeZipper a)
up (Zip (crumb : crumbs) tree) = Just $ Zip crumbs (applyCrumb tree crumb) 
up _ = Nothing

-- | Apply a modification function to the focused subtree.
modifyTree :: (BinTree a -> BinTree a) -> BinTreeZipper a -> BinTreeZipper a
modifyTree f (Zip crumbs tree) = Zip crumbs (f tree)

-- | Replace the entire focused subtree.
setTree :: BinTree a -> BinTreeZipper a -> BinTreeZipper a
-- setTree newTree (Zip crumbs _) = Zip crumbs newTree
setTree = modifyTree . const 

-- | Set the value of the node in focus.
setValue :: a -> BinTreeZipper a -> BinTreeZipper a
setValue x = modifyTree (\t -> t {btValue = x})

-- | Set the left child of the focused node.
setLeft :: Maybe (BinTree a) -> BinTreeZipper a -> BinTreeZipper a
setLeft l = modifyTree (\t -> t {btLeft = l})

-- | Set the right child of the focused node.
setRight :: Maybe (BinTree a) -> BinTreeZipper a -> BinTreeZipper a
setRight r = modifyTree (\t -> t {btRight = r})

-- | Get the entire binary tree currently under focus.
focusedTree :: BinTreeZipper a -> BinTree a
focusedTree (Zip _ tree) = tree

-- | Recursively swap all left and right children.
mirror :: BinTree a -> BinTree a
mirror (BT v l r) = BT v (fmap mirror r) (fmap mirror l)
