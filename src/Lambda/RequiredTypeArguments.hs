{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Chapter 3: A progressive crash course in GHC 9.10 'RequiredTypeArguments' and 'ExplicitNamespaces'.
--
-- In GHC 9.10, we can pass types as first-class positional arguments to value-level functions
-- without using awkward 'Proxy' values or invisible '@TypeApplications'.
module Lambda.RequiredTypeArguments where

import Data.Proxy (Proxy (..))
import Foreign.Storable (Storable (..))
import Lambda.DataKinds (KnownNat (..), Nat (..))

-- ========================================================================= --
-- LEVEL 1: REQUIRED TYPE ARGUMENTS VS TRADITIONAL PROXY                      --
-- ========================================================================= --



-- NOTE: Can we write `x :: Storable Int`?
-- No! `Storable` is a TYPECLASS (a constraint/interface), not a concrete data type.
-- If you write `x :: Storable Int`, GHC throws: "Expected a type, but Storable Int has kind Constraint".
-- Instead, you instantiate a value whose type `Int` *implements* `Storable`:
--   let x = 42 :: Int in sizeOf x

-- NOTE: Why did people invent `sizeOfViaProxy` (and how do you call it)?
-- Suppose you are allocating raw C-style RAM for an array of 1,000 Doubles (`mallocBytes (1000 * sizeOf ...)`).
-- At this exact moment, you do NOT have a real value `x = 42.0 :: Double` lying around yet!
-- If you only have standard `sizeOf :: Storable a => a -> Int`, how do you tell GHC you want the size of Double?
-- You are forced to invent a scary-looking dummy value: `sizeOf (undefined :: Double)`.
--
-- To avoid writing `undefined`, library authors invented `Data.Proxy` (`data Proxy t = Proxy`).
-- You call `sizeOfViaProxy` by passing a Proxy tagged with your target type:
--   sizeOfViaProxy (Proxy :: Proxy Int)
--
-- And GHC 9.10 makes BOTH `undefined` and `Proxy` completely obsolete by letting you pass `(type Int)`!

-- | Traditional approach using 'Proxy' to pass a type argument:
sizeOfViaProxy :: forall a. Storable a => Proxy a -> Int
sizeOfViaProxy _ = sizeOf (undefined :: a)

-- | Modern GHC 9.10 approach using 'RequiredTypeArguments' (`forall a ->`):
-- Notice the arrow `->` right after `forall a`. This tells GHC that `a` is not an invisible
-- type variable (`forall a.`), but rather a REQUIRED positional argument of kind 'Type'!
sizeOfDirect :: forall a -> Storable a => Int
sizeOfDirect (type a) = sizeOf (undefined :: a)

-- | EXERCISE 1: Calling 'sizeOfDirect' with explicit type namespaces.
-- To pass 'Int' or 'Double' into 'sizeOfDirect' at the value level, we use the '(type T)' syntax
-- enabled by 'ExplicitNamespaces':
sizeOfInt :: Int
sizeOfInt = sizeOfDirect (type Int)

sizeOfDouble :: Int
sizeOfDouble = sizeOfDirect (type Double)

-- | Demonstrates running 'sizeOf' across the 4 different instantiation styles:
printStorableExamples :: IO ()
printStorableExamples = do
  putStrLn "=== 1. Passing a real instantiated value (x = 42 :: Int) ==="
  let x = 42 :: Int
  putStrLn $ "sizeOf (42 :: Int) = " ++ show (sizeOf x) ++ " bytes"

  putStrLn "\n=== 2. Passing a dummy undefined value (traditional hack) ==="
  putStrLn $ "sizeOf (undefined :: Int) = " ++ show (sizeOf (undefined :: Int)) ++ " bytes (notice it never crashes!)"

  putStrLn "\n=== 3. Passing a Proxy value (traditional library approach) ==="
  putStrLn $ "sizeOfViaProxy (Proxy :: Proxy Int) = " ++ show (sizeOfViaProxy (Proxy :: Proxy Int)) ++ " bytes"

  putStrLn "\n=== 4. Passing the type namespace directly (GHC 9.10 RequiredTypeArguments) ==="
  putStrLn $ "sizeOfDirect (type Int)    = " ++ show (sizeOfDirect (type Int)) ++ " bytes"
  putStrLn $ "sizeOfDirect (type Double) = " ++ show (sizeOfDirect (type Double)) ++ " bytes"

-- ========================================================================= --
-- LEVEL 2: TYPE-LEVEL COMPUTATION PASSED VIA REQUIRED TYPE ARGUMENTS        --
-- ========================================================================= --

-- | A typeclass that produces the string representation of a type's name.
class TypeName a where
  typeName :: String

instance TypeName Int where
  typeName = "Int"

instance TypeName Bool where
  typeName = "Bool"

instance TypeName String where
  typeName = "String"

instance (TypeName a, TypeName b) => TypeName (a, b) where
  typeName = "(" ++ typeName @a ++ ", " ++ typeName @b ++ ")"

-- | Using 'RequiredTypeArguments' (`forall a ->`) to inspect and return a type's name directly!
describeType :: forall a -> TypeName a => String
describeType (type a) = "The type is: " ++ typeName @a

-- | EXERCISE 2: Call 'describeType' directly on compound types using `(type ...)`:
describeIntBoolPair :: String
describeIntBoolPair = describeType (type (Int, Bool))

-- ========================================================================= --
-- LEVEL 3: COMBINING REQUIRED TYPE ARGUMENTS WITH DATAKINDS & VEC            --
-- ========================================================================= --

-- | Traditional way to query the static length of a 'Vec' required passing the vector value
-- or using '@TypeApplications'. With 'RequiredTypeArguments', we can query the promoted
-- length 'n' directly by passing `(type n)` as a positional argument!
vecLengthDirect :: forall (n :: Nat) -> KnownNat n => Int
vecLengthDirect (type n) = natVal @n

-- | EXERCISE 3: Querying Peano vector lengths using required type arguments!
type ZeroNat = 'Z
type TwoNat  = 'S ('S 'Z)

lengthZero :: Int
lengthZero = vecLengthDirect (type ZeroNat)

lengthTwo :: Int
lengthTwo = vecLengthDirect (type TwoNat)
