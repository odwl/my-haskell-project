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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Free
  ( -- Free Semigroup Adjunction
    unitSG,
    counitSG,
    phi,
    psi,

    -- Free Monoid Adjunction
    unitMon,
    counitMon,

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
import qualified Data.Category as C
import qualified Data.Category.Functor as F
import Data.Category.Functor ((:%), (%), (:.:)(..))
import Data.Category.Adjunction (Adjunction, mkAdjunctionInit)
import qualified Data.Category.Monoidal as M
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
-- Free Semigroup.   NonEmpty -| U
--------------------------------------------------------------------------------

data SemigroupCat a b where
  SemigroupArr :: (Semigroup a, Semigroup b) => (a -> b) -> SemigroupCat a b

instance C.Category SemigroupCat where
  src (SemigroupArr _) = SemigroupArr (\x -> x)
  tgt (SemigroupArr _) = SemigroupArr (\x -> x)
  (SemigroupArr g) . (SemigroupArr f) = SemigroupArr (f >>> g)

data ForgetSemigroup = ForgetSemigroup

instance F.Functor ForgetSemigroup where
  type Dom ForgetSemigroup = SemigroupCat
  type Cod ForgetSemigroup = (->)
  type ForgetSemigroup :% a = a
  ForgetSemigroup % (SemigroupArr f) = f

data FreeSemigroup = FreeSemigroup

instance F.Functor FreeSemigroup where
  type Dom FreeSemigroup = (->)
  type Cod FreeSemigroup = SemigroupCat
  type FreeSemigroup :% a = NonEmpty a
  FreeSemigroup % f = SemigroupArr (fmap f)

unitSG :: a -> NonEmpty a 
unitSG a = (a :| [])

counitSG :: Semigroup a => NonEmpty a -> a 
counitSG = sconcat 

freeForgetAdjunction :: Adjunction SemigroupCat (->) FreeSemigroup ForgetSemigroup
freeForgetAdjunction = mkAdjunctionInit FreeSemigroup ForgetSemigroup unit' universalLifting
  where
    -- The unit of the adjunction: a -> NonEmpty a
    unit' :: (a -> a) -> (a -> NonEmpty a)
    unit' _ = unitSG

    -- The Universal Property: Given a function `f : a -> b` where `b` is a Semigroup,
    -- there is a unique homomorphism `NonEmpty a -> b`.
    universalLifting :: SemigroupCat b b -> (a -> b) -> SemigroupCat (NonEmpty a) b
    universalLifting (SemigroupArr _) f = SemigroupArr (sconcat . fmap f)

-- The Composed Functor (Monad)
type T = ForgetSemigroup :.: FreeSemigroup

-- ==============================================================================
-- The Comonad in SemigroupCat: W = Free . Forget
-- W maps a Semigroup `a` to `NonEmpty a`.
-- ==============================================================================

semigroupComonad :: M.Comonad (FreeSemigroup :.: ForgetSemigroup)
semigroupComonad = M.adjunctionComonad freeForgetAdjunction

-- extract is the counit of the adjunction (epsilon)
-- Notice how this requires `Semigroup a`, which perfectly matches SemigroupCat!
extractSG :: Semigroup a => NonEmpty a -> a
extractSG = sconcat

-- duplicate is F(eta_{G(a)}). 
-- G(a) = a. eta_a = unitSG. F(f) = fmap f. 
-- Therefore, duplicate is `fmap unitSG`.
duplicateSG :: Semigroup a => NonEmpty a -> NonEmpty (NonEmpty a)
duplicateSG = fmap unitSG

-- Both of these functions are valid Semigroup Homomorphisms!
-- sconcat (xs <> ys) == sconcat xs <> sconcat ys
-- fmap unitSG (xs <> ys) == fmap unitSG xs <> fmap unitSG ys

-- CoKleisli composition in SemigroupCat
composeSG :: Semigroup a => (NonEmpty a -> b) -> (NonEmpty b -> c) -> (NonEmpty a -> c)
composeSG f g = fmap (unitSG >>> f) >>> g

phi :: Semigroup b => (NonEmpty a -> b) -> a -> b 
phi f = unitSG >>> f

psi :: Semigroup b => (a -> b) -> NonEmpty a -> b
psi f = fmap f >>> counitSG


--------------------------------------------------------------------------------
-- Free Monoid.   [] -| U
--------------------------------------------------------------------------------

unitMon :: a -> [a] -- aka pure/return in Monad
unitMon a = [a]

counitMon :: Monoid a => [a] -> a -- join = mconcat
counitMon = mconcat

-- [] is not a Control.Comonad but is a Comonad in the category of Monoid.
extractMon :: Monoid a => [a] -> a
extractMon = counitMon

duplicateMon :: [a] -> [[a]]
duplicateMon = fmap unitMon

-- CoKeisli 
idMon :: Monoid a => [a] -> a
idMon = counitMon

composeMon :: (Monoid a, Monoid b, Monoid c) => ([b] -> c) -> ([a] -> b) -> ([a] -> c)
composeMon g f = fmap unitMon >>> fmap f >>> g


--------------------------------------------------------------------------------
-- The Empty Diagonal Adjunction (Initial -| Delta -| Terminal)
--------------------------------------------------------------------------------
-- Let `Delta` be the unique functor from Hask to the Terminal Category `1`.
-- `Delta(a) = ()`.
--
-- This functor has BOTH a Left Adjoint and a Right Adjoint!
-- 1. Left Adjoint (Initial Object): `F_initial(()) = Void`
-- 2. Right Adjoint (Terminal Object): `F_terminal(()) = ()`

-- === Left Adjoint: Initial -| Delta ===
-- F_initial(()) = Void
-- Delta(a) = ()

-- Unit (eta : () -> Delta(F_initial(())))
-- eta : () -> ()
unitInitial :: () -> ()
unitInitial () = ()

-- Counit (epsilon : F_initial(Delta(a)) -> a)
-- epsilon : Void -> a
counitInitial :: Void -> a
counitInitial = absurd

-- === Right Adjoint: Delta -| Terminal ===
-- Delta(a) = ()
-- F_terminal(()) = ()

-- Unit (eta : a -> F_terminal(Delta(a)))
-- eta : a -> ()
unitTerminal :: a -> ()
unitTerminal _ = ()

-- Counit (epsilon : Delta(F_terminal(())) -> ())
-- epsilon : () -> ()
counitTerminal :: () -> ()
counitTerminal () = ()

--------------------------------------------------------------------------------
-- Free Monoid on a Semigroup.   Maybe -| U
--------------------------------------------------------------------------------
-- Crucial point, when s is a semigroup, Maybe s is a monoid.

unitMaybe :: Semigroup s => s -> Maybe s -- eta
unitMaybe = Just

counitMaybe :: Monoid m => Maybe m -> m  -- epsilon (evaluates the freely added unit to mempty)
counitMaybe = fromMaybe mempty

-- The Monad in Semigroup (T = U . F = Maybe)
-- join is G(epsilon_{F(s)}). Because F(s) = Maybe s is a Monoid, we can just use counit!
joinMaybe :: Semigroup s => Maybe (Maybe s) -> Maybe s
joinMaybe = counitMaybe

-- The Comonad in MonoidCat (W = F . U = Maybe)
extractMaybe :: Monoid m => Maybe m -> m
extractMaybe = counitMaybe

duplicateMaybe :: Monoid m => Maybe m -> Maybe (Maybe m)
duplicateMaybe = fmap unitMaybe

-- CoKleisli in MonoidCat
idMaybe :: Monoid m => Maybe m -> m
idMaybe = counitMaybe

composeMaybe :: (Monoid a, Monoid b, Monoid c) => (Maybe b -> c) -> (Maybe a -> b) -> (Maybe a -> c)
composeMaybe g f = fmap unitMaybe >>> fmap f >>> g

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
