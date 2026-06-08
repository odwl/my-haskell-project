{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes, TypeApplications, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Exercism.Zipper
  ( BinTree (..),
    BinTreeZipper,
    Copointed (..),
    focusedTree,
    toZipper,
    GenericZipper (..),
    left,
    ListZipper,
    mirror,
    modifyTree,
    next,
    prev,
    right,
    setLeft,
    setRight,
    setTree,
    setValue,
    fromZipper,
    up,
    ListDirection (..),
    BinTreeDirection (..),
    Differentiable (..),
    Zipper,
    smooth,
    smoothZipper,
    TPossible (..),
    TChoice (..)
  )
where
import Control.Category ((>>>))
import Control.Comonad (Comonad (..))
import Data.Copointed (Copointed (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..), (<|), nonEmpty, toList)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.Functor.Const (Const (..))
import Lambda.Functor (MyIdentity (..), MyProxy (..))

data BinTree a = BT
  { btValue :: a,
    btLeft :: Maybe (BinTree a),
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show, Functor)

instance Comonad BinTree where 
  extract (BT v _ _) = v
  duplicate tree@(BT _ ml mr) = BT tree (duplicate <$> ml) (duplicate <$> mr) 

instance Copointed BinTree where
  copoint = extract 

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

data GenericZipper f a = GenericZipper
  { crumbs :: [Context f a]  -- The path/derivative context
  , focus  :: f a            -- The focused element/subtree
  }

-- | Type family representing the derivative/context of a Functor 'f'.
type family Context (f :: Type -> Type) a
-- This class provides the "natural function that acts as fmap for Context f"
class MapContext (f :: Type -> Type) where
  mapContext :: (a -> b) -> Context f a -> Context f b

type instance Context [] a = a
instance MapContext [] where 
  mapContext = id

type instance Context NonEmpty a = a
instance MapContext NonEmpty where 
  mapContext = id

-- For a Binary Tree, the context is exactly your custom 'Crumb' type:
type instance Context BinTree a = Crumb a
instance MapContext BinTree where 
  mapContext = fmap

data ListDirection = Next
  deriving (Eq, Show)

data BinTreeDirection = GoLeft | GoRight
  deriving (Eq, Show)

-- | Typeclass representing a differentiable functor with generic navigation.
class (Functor f, MapContext f) => Differentiable f where
  -- | The associated type for valid navigation branches
  type Direction f :: Type
  -- | Move down in a specific direction
  downGeneric :: Direction f -> GenericZipper f a -> Maybe (GenericZipper f a)
  -- | Move up generically using the first crumb
  upGeneric :: GenericZipper f a -> Maybe (GenericZipper f a)
  -- | Convert a focused structure into a structure of child zippers,
  -- accumulating crumbs as we descend.
  childrenZippers :: [Context f a] -> f a -> f (GenericZipper f a)
  -- | Reconstruct the list of parent contexts for duplicate
  duplicateCrumbs :: [Context f a] -> f a -> [Context f (GenericZipper f a)]

instance Differentiable [] where 
  type Direction [] = ListDirection

  downGeneric Next (GenericZipper cs (x : xs)) = Just $ GenericZipper (x : cs) xs 
  downGeneric Next (GenericZipper _ []) = Nothing 
  
  upGeneric (GenericZipper (c:cs) focus) = Just $ GenericZipper cs (c:focus)
  upGeneric (GenericZipper [] _) = Nothing 

  childrenZippers cs t@(x : xs) = GenericZipper cs t : childrenZippers (x : cs) xs
  childrenZippers _ [] = []

  duplicateCrumbs (c:cs) focus = p : duplicateCrumbs cs (c : focus)
    where
      p = GenericZipper cs (c : focus)
  duplicateCrumbs [] _ = []
  
instance Differentiable NonEmpty where 
  type Direction NonEmpty = ListDirection 

  downGeneric Next (GenericZipper cs (x :| xs)) = GenericZipper (x : cs) <$> nonEmpty xs

  upGeneric (GenericZipper (c:cs) focus) = Just $ GenericZipper cs (c <| focus)
  upGeneric (GenericZipper [] _) = Nothing

  childrenZippers cs (x :| xs) = GenericZipper cs (x :| xs) :| go (x : cs) xs
    where
      go _ [] = []
      go crumbs (y : ys) = GenericZipper crumbs (y :| ys) : go (y : crumbs) ys

  duplicateCrumbs (c:cs) focus = p : duplicateCrumbs cs (c <| focus)
    where
      p = GenericZipper cs (c <| focus)
  duplicateCrumbs [] _ = []

instance Differentiable BinTree where
  type Direction BinTree = BinTreeDirection 

  downGeneric GoLeft  (Zip cs (BT v ml mr)) = Zip (LeftCrumb v mr : cs) <$> ml
  downGeneric GoRight (Zip cs (BT v ml mr)) = Zip (RightCrumb v ml : cs) <$> mr

  upGeneric = up

  childrenZippers cs tree@(BT v ml mr) =
    BT (Zip cs tree)
       (childrenZippers (LeftCrumb v mr : cs) <$> ml)
       (childrenZippers (RightCrumb v ml : cs) <$> mr)

  duplicateCrumbs [] _ = []
  duplicateCrumbs (LeftCrumb v r : cs) tree =
    let p = Zip cs (applyCrumb tree (LeftCrumb v r))
        rZips = childrenZippers (RightCrumb v (Just tree) : cs) <$> r
    in LeftCrumb p rZips : duplicateCrumbs cs (applyCrumb tree (LeftCrumb v r))
  duplicateCrumbs (RightCrumb v l : cs) tree =
    let p = Zip cs (applyCrumb tree (RightCrumb v l))
        lZips = childrenZippers (LeftCrumb v (Just tree) : cs) <$> l
    in RightCrumb p lZips : duplicateCrumbs cs (applyCrumb tree (RightCrumb v l))
  

instance (Eq (f a), Eq (Context f a)) => Eq (GenericZipper f a) where
  (GenericZipper c1 f1) == (GenericZipper c2 f2) = c1 == c2 && f1 == f2
instance (Show (f a), Show (Context f a), Typeable f) => Show (GenericZipper f a) where
  show (GenericZipper c f) =
    "GenericZipper " ++ show (typeRep (Proxy :: Proxy f)) ++ " " ++ show c ++ " " ++ show f
instance (Functor f, MapContext f) => Functor (GenericZipper f) where
  fmap f (GenericZipper c t) = GenericZipper (map (mapContext @f f) c) (fmap f t)
instance (Differentiable f, Comonad f) => Comonad (GenericZipper f) where
  extract = focus >>> extract
  duplicate (GenericZipper cs f) = GenericZipper (duplicateCrumbs cs f) (childrenZippers cs f)
    
toZipper :: f a -> GenericZipper f a
toZipper root = GenericZipper [] root
 
applyCrumb :: BinTree a -> Crumb a -> BinTree a
applyCrumb tree (LeftCrumb v r) = BT v (Just tree) r
applyCrumb tree (RightCrumb v l) = BT v l (Just tree)

fromZipper :: Differentiable f => GenericZipper f a -> f a
fromZipper z = maybe (focus z) fromZipper (upGeneric z)

-- | Move to the focus's previous sibling (e.g. from right child to left child).
prev :: BinTreeZipper a -> Maybe (BinTreeZipper a)
prev (Zip (RightCrumb v (Just l) : cs) tree) = Just $ Zip (LeftCrumb v (Just tree) : cs) l
prev _ = Nothing

-- | Move to the focus's next sibling (e.g. from left child to right child).
next :: BinTreeZipper a -> Maybe (BinTreeZipper a)
next (Zip (LeftCrumb v (Just r) : cs) tree) = Just $ Zip (RightCrumb v (Just tree) : cs) r
next _ = Nothing

-- | Move the focus to the left child.
left :: BinTreeZipper a -> Maybe (BinTreeZipper a)
left (Zip cs (BT v ml mr)) = Zip (LeftCrumb v mr : cs) <$> ml

-- | Move the focus to the right child.
right :: BinTreeZipper a -> Maybe (BinTreeZipper a)
right (Zip cs (BT v ml mr)) = Zip (RightCrumb v ml : cs) <$> mr

-- | Move the focus to the parent. 
up :: BinTreeZipper a -> Maybe (BinTreeZipper a)
up (Zip (crumb : cs) tree) = Just $ Zip cs (applyCrumb tree crumb) 
up _ = Nothing

-- | Apply a modification function to the focused subtree.
modifyTree :: (BinTree a -> BinTree a) -> BinTreeZipper a -> BinTreeZipper a
modifyTree f (Zip cs tree) = Zip cs (f tree)

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

smooth :: Fractional a => [a] -> [a]
smooth [] = []
smooth xs = zipWith3 average (head xs : xs) xs (tail xs ++ [last xs])
    where average x y z = (x + y + z) / 3

smoothZipper :: Fractional a => ListZipper a -> ListZipper a
smoothZipper = extend getLocalAverage
  where
    getLocalAverage :: Fractional a => ListZipper a -> a
    getLocalAverage (GenericZipper cs (y :| ys)) =
      let leftVal  = fromMaybe y (listToMaybe cs)
          rightVal = fromMaybe y (listToMaybe ys)
      in (leftVal + y + rightVal) / 3

smoothZipperList :: Fractional a => [a] -> [a]
smoothZipperList xs = case nonEmpty xs of
  Nothing -> []
  Just ne -> toList $ fromZipper $ smoothZipper (toZipper ne)


data TPossible a = TPossible
  { leftward :: a
  , rightward :: a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data TChoice = L | R
  deriving (Show, Eq)

instance Distributive TPossible where
  distribute :: Functor f => f (TPossible a) -> TPossible (f a)
  distribute x = TPossible (fmap leftward x) (fmap rightward x)

instance Representable TPossible where
  type Rep TPossible = TChoice
  tabulate :: (TChoice -> a) -> TPossible a
  tabulate g = TPossible (g L) (g R)
  index :: TPossible a -> TChoice -> a
  index (TPossible x _) L = x
  index (TPossible _ y) R = y

