{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambda.Limit where

import Control.Category
import Data.Kind (Type)
import Prelude hiding (id, (.))



--------------------------------------------------------------------------------
-- TerminalMonad \delta -| 1 :: C <-> 1
--------------------------------------------------------------------------------

data OneMorphism obj1 obj2 where
  OneMorphism :: OneMorphism () ()

deltaT :: (a -> b) -> OneMorphism () ()
deltaT _ = OneMorphism

constT :: OneMorphism () () -> () -> ()
constT _ = id

monadT :: (a -> b) -> () -> ()
monadT _ = id

unitT :: a -> ()
unitT _ = ()

muT :: () -> ()
muT _ = ()

-- Binary product
deltaP :: (a -> b) -> (a -> b, a -> b)
deltaP f = (f, f)

prodP :: (a -> c) -> (b -> d) -> (a, b) -> (c,d)
prodP f g (x, y) = (f x, g y)

monadP :: (a -> b) -> (a, a) -> (b, b)
monadP f (x, y) = (f x, f y)

unitP :: a -> (a, a)
unitP x = (x, x)

muP :: ((a, a), (a, a)) -> (a, a)
muP ((x, _), (_, y)) = (x, y)

comonadP :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d), (a, b) -> (c, d))
comonadP f g = (prodP f g, prodP f g)

counitP :: ((a, b) -> a, (a, b) -> b)
counitP = (fst, snd)

comuP :: ((a, b) -> ((a, b), (a, b)), (a, b) -> ((a, b), (a, b)))
comuP = (\p -> (p, p), \p -> (p, p))




-- Equalizer 
deltaE :: (a -> b) -> (a -> b, a -> b)
deltaE f = (f, f)

equalizerE :: (a -> a') -> (b -> b') -> a -> a'
equalizerE fnE _ = fnE 

monadE :: (a -> b) -> a -> b 
monadE = id

unitE :: a -> a
unitE = id

muE :: a -> a 
muE = id










class FunctorCategory0 mor where
  type Obj0 mor :: Type
  identity0 :: Obj0 mor -> mor (Obj0 mor) (Obj0 mor)
  compose0  :: mor o2 o3 -> mor o1 o2 -> mor o1 o3

-- | The unique empty diagram (functor from 0 to Hask)
data EmptyDiagram = EmptyDiagram
  deriving (Show, Eq)

-- | Morphisms between empty diagrams
data EmptyMorphism obj1 obj2 where
  EmptyMorphism :: EmptyMorphism EmptyDiagram EmptyDiagram

instance FunctorCategory0 EmptyMorphism where
  type Obj0 EmptyMorphism = EmptyDiagram
  identity0 EmptyDiagram = EmptyMorphism
  compose0 EmptyMorphism EmptyMorphism = EmptyMorphism

-- | Functor category representation class for 1-object index categories
class FunctorCategory1 mor where
  type Obj1 mor :: Type -> Type
  identity1 :: Obj1 mor a -> mor (Obj1 mor a) (Obj1 mor a)
  compose1  :: mor (Obj1 mor a2) (Obj1 mor a3)
            -> mor (Obj1 mor a1) (Obj1 mor a2)
            -> mor (Obj1 mor a1) (Obj1 mor a3)

-- | A diagram with one object (functor from 1 to Hask)
newtype SingleObject a = SingleObject { runSingleObject :: a }
  deriving (Show, Eq)

-- | Morphisms between single-object diagrams
data SingleMorphism obj1 obj2 where
  SingleMorphism :: (a -> b) -> SingleMorphism (SingleObject a) (SingleObject b)

instance FunctorCategory1 SingleMorphism where
  type Obj1 SingleMorphism = SingleObject
  identity1 _ = SingleMorphism id
  compose1 (SingleMorphism g) (SingleMorphism f) = SingleMorphism (g . f)


newtype TerminalM a = TerminalM ()
  deriving (Show, Eq)

instance Functor TerminalM where 
  fmap _ _  = TerminalM () 

instance Applicative TerminalM where 
  pure _ = TerminalM ()  
  _ <*> _ = TerminalM () 

instance Monad TerminalM where 
  _ >>= _ = TerminalM ()


--------------------------------------------------------------------------------
-- Equalizer
--------------------------------------------------------------------------------

-- a. Define Category [J, C] where J is the category m,m': 0 -> 1 and C any category 



-- | Functor category representation class
class FunctorCategory mor where
  type Obj mor :: Type -> Type -> Type
  identity :: Obj mor a b -> mor (Obj mor a b) (Obj mor a b)
  compose  :: mor (Obj mor a2 b2) (Obj mor a3 b3)
           -> mor (Obj mor a1 b1) (Obj mor a2 b2)
           -> mor (Obj mor a1 b1) (Obj mor a3 b3)

-- | A parallel pair of morphisms (arrows)
data DoubleArrow a b = DoubleArrow (a -> b) (a -> b)

-- | A morphism in the category of parallel pairs (commuting square).
--
-- Laws:
-- For CommutingSquare (DoubleArrow f1 g1) (DoubleArrow f2 g2) alpha beta,
-- the following must hold:
--
-- [Law 1]  f2 . alpha == beta . f1
-- [Law 2]  g2 . alpha == beta . g1
data CommutingSquare obj1 obj2 where 
  CommutingSquare :: {
    source :: DoubleArrow a1 b1,
    target :: DoubleArrow a2 b2,
    alpha :: a1 -> a2,
    beta :: b1 -> b2
  } -> CommutingSquare (DoubleArrow a1 b1) (DoubleArrow a2 b2)


instance FunctorCategory CommutingSquare where
  type Obj CommutingSquare = DoubleArrow
  identity pair = CommutingSquare
    { source = pair
    , target = pair
    , alpha  = id
    , beta   = id
    }
  compose g f = CommutingSquare
    { source = source f
    , target = target g
    , alpha  = alpha g . alpha f
    , beta   = beta g . beta f
    }

-- | The Equalizer construct in Haskell (Hask category)
-- For a parallel pair of morphisms f, g :: a -> b,
-- the equalizer consists of:
-- 1. An object (represented as an existential type `c`)
-- 2. An inclusion morphism `c -> a`
-- 3. A factorization function: for any `d` and morphism `h :: d -> a` such that `f . h == g . h`,
--    we can factorise `h` through `inclusion` to get `d -> c`.
data Equalizer a b = forall c. Equalizer
  { inclusion :: c -> a
  , factorise :: forall d. (d -> a) -> Maybe (d -> c)
  }

-- | The concrete equalizer object representation
newtype EqualizerOb a = EqualizerOb { runEqualizerOb :: a }
  deriving (Show, Eq)

instance Functor EqualizerOb where
  fmap f (EqualizerOb x) = EqualizerOb (f x)

instance Applicative EqualizerOb where
  pure = EqualizerOb
  EqualizerOb f <*> EqualizerOb x = EqualizerOb (f x)

instance Monad EqualizerOb where
  EqualizerOb x >>= f = f x

-- | Equalize a double arrow
equalize :: Eq b => DoubleArrow a b -> Equalizer a b
equalize (DoubleArrow _ _) = Equalizer
  { inclusion = runEqualizerOb
  , factorise = \h -> Just (\x -> EqualizerOb (h x))
  }

-- | Identity morphism for a parallel pair
idCommutingSquare :: DoubleArrow a b -> CommutingSquare (DoubleArrow a b) (DoubleArrow a b)
idCommutingSquare = identity

-- | Composition of commuting squares
compCommutingSquare 
  :: CommutingSquare (DoubleArrow a2 b2) (DoubleArrow a3 b3)
  -> CommutingSquare (DoubleArrow a1 b1) (DoubleArrow a2 b2)
  -> CommutingSquare (DoubleArrow a1 b1) (DoubleArrow a3 b3)
compCommutingSquare = compose

-- | Map a parallel pair morphism (commuting square) to a morphism between their equalizers.
-- This defines the action of the equalizer functor on morphisms.
mapEqualizer :: CommutingSquare (DoubleArrow a1 b1) (DoubleArrow a2 b2) -> EqualizerOb a1 -> EqualizerOb a2
mapEqualizer square (EqualizerOb x) = EqualizerOb (alpha square x)

-- | Map a commuting square of parallel pairs to a commuting square between their equalizers.
-- This represents the action of the EqualizerOb functor on the category of parallel pairs.
mapEqualizerM 
  :: CommutingSquare (DoubleArrow a1 b1) (DoubleArrow a2 b2)
  -> CommutingSquare (DoubleArrow (EqualizerOb a1) (EqualizerOb b1)) (DoubleArrow (EqualizerOb a2) (EqualizerOb b2))
mapEqualizerM square = CommutingSquare
  { source = let DoubleArrow f g = source square in DoubleArrow (fmap f) (fmap g)
  , target = let DoubleArrow f g = target square in DoubleArrow (fmap f) (fmap g)
  , alpha  = fmap (alpha square)
  , beta   = fmap (beta square)
  }

-- | The unit of the EqualizerOb monad on parallel pairs.
-- For any parallel pair, it constructs the commuting square to its equalizer-lifted parallel pair.
unitEqualizerM :: DoubleArrow a b -> CommutingSquare (DoubleArrow a b) (DoubleArrow (EqualizerOb a) (EqualizerOb b))
unitEqualizerM pair = CommutingSquare
  { source = pair
  , target = let DoubleArrow f g = pair in DoubleArrow (fmap f) (fmap g)
  , alpha  = pure
  , beta   = pure
  }

-- | The join (multiplication) of the EqualizerOb monad on parallel pairs.
joinEqualizerM 
  :: DoubleArrow a b 
  -> CommutingSquare (DoubleArrow (EqualizerOb (EqualizerOb a)) (EqualizerOb (EqualizerOb b))) (DoubleArrow (EqualizerOb a) (EqualizerOb b))
joinEqualizerM pair = CommutingSquare
  { source = let DoubleArrow f g = pair in DoubleArrow (fmap (fmap f)) (fmap (fmap g))
  , target = let DoubleArrow f g = pair in DoubleArrow (fmap f) (fmap g)
  , alpha  = joinEqualizerOb
  , beta   = joinEqualizerOb
  }
  where
    joinEqualizerOb (EqualizerOb (EqualizerOb x)) = EqualizerOb x


--------------------------------------------------------------------------------
-- Binary Product (Category version)
--------------------------------------------------------------------------------

-- | A diagram representing two independent objects (discrete diagram of 2 objects).
-- The types of the two objects are a and b.
data PairObj a b = PairObj
  deriving (Show, Eq)

-- | A morphism in the functor category [Two, Hask] (natural transformation between pair diagrams)
data PairMorphism obj1 obj2 where
  PairMorphism :: {
    alphaP :: a1 -> a2,
    betaP  :: b1 -> b2
  } -> PairMorphism (PairObj a1 b1) (PairObj a2 b2)

instance FunctorCategory PairMorphism where
  type Obj PairMorphism = PairObj
  identity _ = PairMorphism id id
  compose g f = PairMorphism (alphaP g . alphaP f) (betaP g . betaP f)

-- | The Product construct in Haskell (Hask category)
-- For two objects a and b, the product consists of:
-- 1. An object (represented as an existential type `c`)
-- 2. Two projection morphisms `projA :: c -> a` and `projB :: c -> b`
-- 3. A factorization function: for any `d` and morphisms `f :: d -> a`, `g :: d -> b`,
--    we can factorise them through the projections to get `d -> c`.
data Product a b = forall c. Product
  { projA :: c -> a
  , projB :: c -> b
  , factoriseP :: forall d. (d -> a) -> (d -> b) -> (d -> c)
  }

-- | The concrete product object representation
newtype ProductOb a b = ProductOb { runProductOb :: (a, b) }
  deriving (Show, Eq)

-- | Construct the product of a pair of objects
produce :: PairObj a b -> Product a b
produce _ = Product
  { projA = fst . runProductOb
  , projB = snd . runProductOb
  , factoriseP = \h1 h2 d -> ProductOb (h1 d, h2 d)
  }

-- | Map a pair diagram morphism to a morphism between their products.
-- This defines the action of the product functor on morphisms.
mapProduct :: PairMorphism (PairObj a1 b1) (PairObj a2 b2) -> ProductOb a1 b1 -> ProductOb a2 b2
mapProduct square (ProductOb (x, y)) = ProductOb (alphaP square x, betaP square y)

-- | Map a pair diagram morphism to a morphism between their product-lifted diagrams.
-- This represents the action of the ProductOb functor on the category of pair diagrams.
mapProductM
  :: PairMorphism (PairObj a1 b1) (PairObj a2 b2)
  -> PairMorphism (PairObj (ProductOb a1 b1) (ProductOb a1 b1)) (PairObj (ProductOb a2 b2) (ProductOb a2 b2))
mapProductM morphism = PairMorphism (mapProduct morphism) (mapProduct morphism)

-- | The extract (counit) of the Product comonad on pair diagrams.
-- It maps the product diagram (ProductOb a b, ProductOb a b) to the original diagram (a, b).
extractProductM :: PairObj a b -> PairMorphism (PairObj (ProductOb a b) (ProductOb a b)) (PairObj a b)
extractProductM _ = PairMorphism
  { alphaP = fst . runProductOb
  , betaP  = snd . runProductOb
  }

-- | The duplicate (cojoin) of the Product comonad on pair diagrams.
duplicateProductM 
  :: PairObj a b 
  -> PairMorphism (PairObj (ProductOb a b) (ProductOb a b)) (PairObj (ProductOb (ProductOb a b) (ProductOb a b)) (ProductOb (ProductOb a b) (ProductOb a b)))
duplicateProductM _ = PairMorphism
  { alphaP = \p -> ProductOb (p, p)
  , betaP  = \p -> ProductOb (p, p)
  }






