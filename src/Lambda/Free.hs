{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.Free
  ( -- Free Semigroup Adjunction
    unitSemigroup,
    counitSemigroup,
    phi,
    psi,

    -- Free Monoid Adjunction
    unitMonoid,
    counitMonoid,

    -- Free Functor
    Coyoneda (..),
    liftCoyoneda,
    lowerCoyoneda,
    Coyoneda2 (..),
    fn,
    len,
    coyExample,
    ContraCoyoneda (..),
    runContraCoyOp,
    len2,
    result,

    -- Free Monad
    Free (..),
    liftF,
    foldFree,
    TeletypeF (..),
    putStrLnF,
    getLineF,
    runTeletypePure,
    
    -- Cofree
    Cofree (..),
    ComonadCofree (..),
    section,
    exampleCofree,
    exampleCofree1,
    exampleCofree2,
    exampleCofree3,
    exampleCofree4,
    exampleCofree5
  ) where

import Data.Kind (Type)
import Control.Comonad (Comonad (..))
import Data.Distributive (Distributive (..))
import Data.Proxy (Proxy (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Coyoneda (Coyoneda (..), liftCoyoneda, lowerCoyoneda)
import Lambda.Functor (MyIdentity (..))
import Data.Functor.Contravariant (Contravariant (..), Op (..))
import Control.Category ((>>>))
import Data.Set (Set, fromList)
import qualified Data.Set as S
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)

--------------------------------------------------------------------------------
-- Free Semigroup.   NonEmpty -| U
--------------------------------------------------------------------------------

unitSemigroup :: a -> NonEmpty a 
unitSemigroup a = (a :| [])

counitSemigroup :: Semigroup a => NonEmpty a -> a 
counitSemigroup = sconcat 

phi :: Semigroup b => (NonEmpty a -> b) -> a -> b
phi f a = f (a :| [])

psi :: Semigroup b => (a -> b) -> NonEmpty a -> b
psi f list = sconcat (NE.map f list) 

--------------------------------------------------------------------------------
-- Free Monoid.   [] -| U
--------------------------------------------------------------------------------

unitMonoid :: a -> [a]
unitMonoid a = [a]

counitMonoid :: Monoid a => [a] -> a
counitMonoid = mconcat

--------------------------------------------------------------------------------
-- Free Functor - Coyoneda
--------------------------------------------------------------------------------

-- Suppose this is a library 
lenShow :: (Functor f, Show a) => f a -> f Int 
lenShow = fmap (show >>> length)

intSet :: Set Int 
intSet = fromList [1..1000]

lowerCoyonedaSet :: Ord a => Coyoneda Set a -> Set a
lowerCoyonedaSet (Coyoneda f s) = S.map f s

-- This is not a functor, but I would like to use lenShow  on Set Int
process :: Set Int -> Set Int 
process = liftCoyoneda >>> lenShow >>> lowerCoyonedaSet

result = process intSet




-- We can use Op String from Data.Functor.Contravariant as an example of a type constructor (Type -> Type) that is not a Functor.

data Coyoneda2 g a = forall b. Coyoneda2 (b -> a) (g b)

instance Functor (Coyoneda2 g) where
  fmap :: (a -> c) -> Coyoneda2 g a -> Coyoneda2 g c
  fmap f (Coyoneda2 h gb) = Coyoneda2 (h >>> f) gb

fn :: Show a => Op String a
fn = Op show

len :: Show a => a -> Int 
len = length . getOp fn

coyExample :: Coyoneda2 (Op String) Bool
coyExample = fmap even (Coyoneda2 id (fn :: Op String Int))

--------------------------------------------------------------------------------
-- Free Contravariant Functor - ContraCoyoneda
--------------------------------------------------------------------------------

data ContraCoyoneda g a = forall b. ContraCoyoneda (a -> b) (g b)

instance Contravariant (ContraCoyoneda g) where
  contramap :: (b -> a) -> ContraCoyoneda g a -> ContraCoyoneda g b
  contramap f (ContraCoyoneda h gb) = ContraCoyoneda (h . f) gb

runContraCoyOp :: ContraCoyoneda (Op String) a -> a -> String
runContraCoyOp (ContraCoyoneda h (Op g)) x = g (h x)

len2 :: Show a => a -> Int
len2 a = length (runContraCoyOp (ContraCoyoneda id fn) a)




-- data Coyoneda (f :: Type -> Type) a where
--   Coyoneda :: (b -> a) -> f b -> Coyoneda f a

-- instance Functor (Coyoneda f) where
--   fmap f (Coyoneda g fb) = Coyoneda (f . g) fb

-- liftCoyoneda :: f a -> Coyoneda f a
-- liftCoyoneda = Coyoneda id

-- lowerCoyoneda :: Functor f => Coyoneda f a -> f a
-- lowerCoyoneda (Coyoneda g fb) = fmap g fb

--------------------------------------------------------------------------------
-- Free Monad
--------------------------------------------------------------------------------

data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free fx) = Free (fmap (fmap f) fx)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Free ffx <*> x = Free (fmap (<*> x) ffx)

instance (Functor f) => Monad (Free f) where
  Pure x >>= f = f x
  Free ffx >>= f = Free (fmap (>>= f) ffx)

liftF :: (Functor f) => f a -> Free f a
liftF fa = Free (fmap Pure fa)

foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure x) = return x
foldFree nt (Free ffx) = nt ffx >>= foldFree nt

--------------------------------------------------------------------------------
-- Teletype DSL example
--------------------------------------------------------------------------------

data TeletypeF next
  = PutStrLn String next
  | GetLine (String -> next)
  deriving (Functor)

putStrLnF :: String -> Free TeletypeF ()
putStrLnF s = liftF (PutStrLn s ())

getLineF :: Free TeletypeF String
getLineF = liftF (GetLine id)

runTeletypePure :: [String] -> Free TeletypeF a -> ([String], a)
runTeletypePure _ (Pure x) = ([], x)
runTeletypePure inputs (Free (PutStrLn s next)) =
  let (outs, res) = runTeletypePure inputs next
   in (s : outs, res)
runTeletypePure [] (Free (GetLine _)) = error "runTeletypePure: not enough inputs"
runTeletypePure (i : is) (Free (GetLine g)) = runTeletypePure is (g i)

--------------------------------------------------------------------------------
-- Cofree
--------------------------------------------------------------------------------

infixr 5 :<
data Cofree f a = a :< f (Cofree f a)

instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a) where
  (x :< xs) == (y :< ys) = x == y && xs == ys

instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
  show (x :< xs) = "(" ++ show x ++ " :< " ++ show xs ++ ")"

class (Functor f, Comonad w) => ComonadCofree f w | w -> f where
  unwrap :: w a -> f (w a)

instance Functor f => ComonadCofree f (Cofree f) where
  unwrap (_ :< xs) = xs

exampleCofree :: Cofree Maybe Int
exampleCofree = 3 :< Nothing

instance Functor f => Functor (Cofree f) where
  fmap f (x :< xs) = f x :< fmap (fmap f) xs

instance Functor f => Comonad (Cofree f) where
  extract (x :< _) = x 
  duplicate c@(x :< xs) = c :< fmap duplicate xs

instance Distributive f => Distributive (Cofree f) where
  distribute :: Functor g => g (Cofree f a) -> Cofree f (g a)
  distribute gco = fmap extract gco :< fmap distribute (distribute (fmap unwrap gco))

exampleCofree1 :: Cofree Maybe Int -- same as NonEmpty
exampleCofree1 = 3 :< Just (5 :< Nothing)

exampleCofree2 :: Cofree Proxy Int -- same as Myidentity 
exampleCofree2 = 3 :< Proxy

exampleCofree3 :: Cofree MyIdentity Int  -- same as infinite list 
exampleCofree3 = 3 :< Id exampleCofree3

exampleCofree4 :: Cofree (Const String) Int -- same as product
exampleCofree4 = 3 :< Const "Hello" 

exampleCofree5 :: Cofree ((->) String) Int 
exampleCofree5 = 3 :< (\str -> length str :< const exampleCofree5)

section :: Comonad w => w a -> Cofree w a
section wa = extract wa :< fmap section (duplicate wa) 
