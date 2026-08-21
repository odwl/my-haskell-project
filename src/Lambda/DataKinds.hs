{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

-- | A hands-on, progressive crash course in 'DataKinds' and GHC 9.10 'ListTuplePuns'.
--
-- Before DataKinds, Haskell had a strict separation between terms (values) and types.
-- Values lived in terms, and types lived in kinds.
-- 'DataKinds' bridges this divide by lifting data constructors to the type level,
-- allowing us to encode state machines, fixed-length vectors, and rich domain rules
-- directly inside GHC's type checker!
module Lambda.DataKinds where

import Control.Arrow ((>>>))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))

-- ========================================================================= --
-- LEVEL 1: PROMOTED ALGEBRAIC DATA TYPES & TYPE-SAFE STATE MACHINES         --
-- ========================================================================= --

-- | We define a normal algebraic data type representing a door's physical state.
-- With 'DataKinds' enabled, GHC promotes:
-- 1. 'DoorState' to a kind (the type-level universe of door states).
-- 2. 'Opened' and 'Closed' to distinct types of kind 'DoorState'.
data DoorState = Opened | Closed
  deriving (Show, Eq)

-- | A GADT (Generalized Algebraic Data Type) indexed by our promoted 'DoorState' kind.
-- By indexing 'Door' with 's :: DoorState', we statically lock down what operations
-- can be performed on the door at compile time.
data Door (s :: DoorState) where
  -- | A door can only be constructed in either the 'Opened' or 'Closed' state.
  MkDoor :: String -> Door s

deriving instance Show (Door s)
deriving instance Eq (Door s)

-- | EXERCISE 1 (Classic DataKinds):
-- Write functions that transition door states safely.
-- Notice in classic DataKinds we historically wrote `'Opened` and `'Closed` with leading ticks.
-- In GHC 9.10, ticks on promoted constructors are optional when unambiguous, but let's see how
-- the type system enforces transitions!


closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (MkDoor material) = MkDoor material

openDoor :: Door 'Closed -> Door 'Opened
openDoor (MkDoor material) = MkDoor material

-- -- | A phantom type representing permission levels in a secure system.
data Permission = ReadOnly | ReadWrite
  deriving (Show, Eq)

data FileHandle (p :: Permission) = FileHandle FilePath
  deriving (Show, Eq)

-- -- | EXERCISE 1b: Only handles with 'ReadWrite permission can write to a file!
-- -- Try passing a FileHandle 'ReadOnly to this in tests and observe GHC reject it at compile time.
writeFileSafe :: FileHandle 'ReadWrite -> String -> IO ()
writeFileSafe (FileHandle path) content = writeFile path content

readFileSafe :: FileHandle 'ReadOnly -> IO String
readFileSafe (FileHandle path) = readFile path


-- ========================================================================= --
-- LEVEL 2: PROMOTED PEANO NUMBERS & FIXED-LENGTH VECTORS (VEC)              --
-- ========================================================================= --

-- | Peano natural numbers: zero and successor.
-- When promoted by 'DataKinds', 'Z becomes a type of kind 'Nat',
-- and 'S becomes a type-level constructor of kind: 'Nat -> Nat'.
data Nat = Z | S Nat
  deriving (Show, Eq)

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S two

-- | A type-safe length-indexed list (Vector).
-- The kind of the index 'n' is precisely our promoted 'Nat'!
data Vec (n :: Nat) a where
  VNil  :: Vec 'Z a
  VCons :: a -> Vec n a -> Vec ('S n) a

-- | A typeclass to convert a promoted 'Nat' into a runtime 'Int'.
class KnownNat (n :: Nat) where
  natVal :: Int

instance KnownNat 'Z where
  natVal = 0

instance KnownNat n => KnownNat ('S n) where
  natVal = 1 + natVal @n

-- | Helper class to recursively read ('peek') and write ('poke') a fixed-length 'Vec n a' across unmanaged C memory.
class KnownNat n => StorableVec (n :: Nat) where
  peekVec :: forall a. Storable a => Ptr (Vec n a) -> IO (Vec n a)
  pokeVec :: forall a. Storable a => Ptr (Vec n a) -> Vec n a -> IO ()

instance StorableVec 'Z where
  peekVec _ = pure VNil
  pokeVec _ VNil = pure ()

instance StorableVec n => StorableVec ('S n) where
  peekVec :: forall a. Storable a => Ptr (Vec ('S n) a) -> IO (Vec ('S n) a)
  peekVec ptr = do
    x  <- peek (castPtr ptr)
    xs <- peekVec @n (castPtr (ptr `plusPtr` sizeOf @a undefined))
    pure (VCons x xs)

  pokeVec :: forall a. Storable a => Ptr (Vec ('S n) a) -> Vec ('S n) a -> IO ()
  pokeVec ptr (VCons x xs) = do
    poke (castPtr ptr) x
    pokeVec @n (castPtr (ptr `plusPtr` sizeOf @a undefined)) xs

-- | The Storable instance for our length-indexed Vec!
-- Because the length 'n' is statically known at compile time via 'KnownNat n',
-- and 'a' has a static byte footprint via 'Storable a', 'Vec n a' serializes
-- directly to and from contiguous unboxed C pointer memory (`n * sizeOf a` bytes)!
instance (StorableVec n, Storable a) => Storable (Vec n a) where
  sizeOf _    = natVal @n * sizeOf @a undefined
  alignment _ = alignment @a undefined
  peek ptr    = peekVec @n ptr
  poke ptr xs = pokeVec @n ptr xs

vec0 :: Vec 'Z Int 
vec0 = VNil 

type One = 'S 'Z 
vec1 :: Vec One String
vec1 = VCons "hello" VNil 

type Two = 'S One

vec2 :: Vec Two String
vec2 = VCons "hello" (VCons "world" VNil)

type Three = One + Two
vec3 :: Vec Three String 
vec3 = vappend vec1 vec2 

instance Functor (Vec n) where 
  fmap = fmap'
  -- fmap _ VNil = VNil 
  -- fmap f (VCons x xs) = VCons (f x) $ f <$> xs

instance Foldable (Vec n) where
  foldMap :: Monoid m => (a -> m) -> Vec n a -> m
  -- foldMap g vec = 
  foldMap _ VNil = mempty
  foldMap f (VCons x xs) = f x <> foldMap f xs

instance Traversable (Vec n) where 
  traverse :: Applicative f => (a -> f b) -> Vec n a -> f (Vec n b) 
  traverse _ VNil = pure VNil
  traverse g (VCons x xs) = do
    y  <- g x
    ys <- traverse g xs
    pure (VCons y ys)

fmap' :: (a -> b) -> (Vec n a) -> (Vec n b)
fmap' = (Identity .) >>> traverse >>> fmap runIdentity

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)

-- | EXERCISE 2: Safe head and safe append (`vappend`).
-- Unlike standard 'Prelude.head', 'vhead' is mathematically total:
-- it accepts ONLY vectors of length at least 1 ('S n), making empty-list crashes impossible!
vhead :: Vec ('S n) a -> a
vhead (VCons x _) = x

-- | Type-level addition family right across our promoted 'Nat' kind.
type family (n :: Nat) + (m :: Nat) :: Nat where
  'Z     + m = m
  ('S n) + m = 'S (n + m)

-- | Appends two length-indexed vectors together, adding their type-level lengths statically!
vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- ========================================================================= --
-- LEVEL 3: EVOLVING TO GHC 9.10 'ListTuplePuns' (UNTICKED LISTS & TUPLES)   --
-- ========================================================================= --

-- | In classic DataKinds, promoting built-in lists '[]' and tuples '(,)' required awkward leading ticks:
--   type ClassicTypes = '[Int, Bool, String]
--   type ClassicPair  = '(Int, Bool)
--
-- With GHC 9.10 'ListTuplePuns' (`{-# LANGUAGE ListTuplePuns #-}`), GHC unifies the namespace!
-- You can write type-level lists and tuples right without ticks, punning the exact value syntax!

-- | A type-level heterogenous schema using unticked list syntax [Type] and tuple syntax (Type, Type).
type Schema = [(Type, Type)]

-- | A type family that looks up the value type corresponding to a key type in an unticked schema list!
type family LookupType (k :: Type) (schema :: Schema) :: Type where
  LookupType k ('(k, v) ': rest) = v
  LookupType k (_ ': rest)      = LookupType k rest

-- | An HList (Heterogeneous List) indexed by our unticked promoted list kind [Type]!
data HList (ts :: [Type]) where
  HNil  :: HList '[]
  HCons :: t -> HList ts -> HList (t : ts)

deriving instance Show (HList '[])
deriving instance (Show t, Show (HList ts)) => Show (HList (t : ts))
deriving instance Eq (HList '[])
deriving instance (Eq t, Eq (HList ts)) => Eq (HList (t : ts))

------ Sandbox stuff ----
-- DataKinds
-- Custom tags to represent the status
data Unauthenticated -- Kind = Type. This is unhabited, like Void.
data Authenticated

-- A session wrapper holding a string token or username
-- status is an unconstrained parameter of kind Type
-- Here status is a phantom type. default kind is type. 
-- So session has kind Type -> Type. 
newtype Session status = Session String -- Kind = Type -> Type 
createGuest :: Session Unauthenticated
createGuest = Session "guest_token_abc123"

getToken :: Session s -> String
getToken (Session token) = token

s :: String
s = getToken createGuest

-- deleteDatabase :: Session Authenticated -> IO () -- Got it.

mySession :: Session Authenticated
mySession = Session "token-12345"

newtype Session' (status :: Type -> Type) = Session' String
-- aSession' :: Session' Maybe

-- This works, but there is a major flaw...

-- {-# LANGUAGE DataKinds #-}

-- 1. Declare a normal-looking data type
data Status = Unauth | Auth

newtype SessionSafe (status :: Status) = SessionSafe String
safeGuest :: SessionSafe 'Unauth
safeGuest = SessionSafe "guest_token_abc123"

safeAdmin :: SessionSafe 'Auth
safeAdmin = SessionSafe "admin_token_xyz789"

