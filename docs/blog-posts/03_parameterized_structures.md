# Part 3: The Parameterized Structures (No Laws)

**Author:** Olivier De Wolf, odewolf@gmail.com

## Chapter 2: Parameterized Types of Kind `Type -> Type`

These are type constructors that require one type argument `a` before they become concrete types. Because they take another type as an argument, they are categorically referred to as **Higher-Kinded Types (HKTs)**. The number of inhabitants discussed here applies *regardless* of what `a` is instantiated to (i.e. the type parameter `a` is completely ignored at the value level).

### Section 2.1: `VoidFoldable` (0 Inhabitants)

These parameterized types cannot be constructed, no matter what `a` is. 

#### 1. Standard Parameterized Empty Data
```haskell
data VoidFoldable a
```

#### 2. Using GADT Syntax
```haskell
{-# LANGUAGE GADTs #-}
data VoidFoldable a where {}
```

#### 3. Phantom Wrapping `Data.Void`
```haskell
import Data.Void (Void)
newtype VoidFoldable a = VoidFoldable Void
```

#### 4. Reusing Standard Library Structures
GHC provides existing parameterized empty types for generic programming, like `V1`, or we can combine `Const` and `Void`.
```haskell
import GHC.Generics (V1)
import Data.Functor.Const (Const)
import Data.Void (Void)

-- V1 a 
-- Const Void a
```

### Section 2.2: `Proxy` (1 Inhabitant)

These parameterized types have exactly one value, irrespective of `a`.

#### 1. `Data.Proxy`
`Proxy` is used to pass *type-level* information around at runtime without needing an actual value of that type.
```haskell
import Data.Proxy (Proxy(..))
-- The type is `Proxy a`, the only value is `Proxy`
myProxy :: Proxy Int
myProxy = Proxy
```

#### 2. `Constants` and `Generics`
GHC generic programming uses `U1` to represent constructors with no fields. Alteratively, `Const () a` yields exactly 1 inhabitant.
```haskell
import GHC.Generics (U1(..))
import Data.Functor.Const (Const(..))

-- U1 a (value is U1)
-- Const () a (value is Const ())
```

### Section 2.3: `Const Bool a` (2 Inhabitants)

These parameterized types have precisely two values, regardless of `a`.

#### 1. Custom Parameterized Tags
```haskell
data TwoOptions a = Option1 | Option2
```

#### 2. `Const Bool a`
The `Const` functor holding a `Bool` gives exactly two possible states.
```haskell
import Data.Functor.Const (Const(..))

-- Const False :: Const Bool a
-- Const True  :: Const Bool a
```

***
