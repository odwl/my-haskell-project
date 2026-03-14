# Minimal Types
**Author:** Olivier De Wolf, odewolf@gmail.com

## Introduction

In type theory and functional programming, we can classify types by the number of distinct values (inhabitants) they can hold at runtime. Let's call "minimal types" the types that have very few inhabitants (that is, possible distinct values). Understanding these "minimal types" provides a strong foundation for building robust and mathematically sound abstractions.

While minimal types are omnipresent in pure functional languages like Haskell, they can often feel counter-intuitive or overly abstract to newcomers. Why would we want a type that holds zero values? What is the point of a type with exactly one? This document aims to demystify these concepts by exploring the simplest possible types and demonstrating their immense practical value. To aid in your learning journey, a hands-on exercise section has been provided at the end of this guide.

## A Quick Primer: What is a "Kind"?

Before we dive into counting inhabitants, we need to clarify what we mean by "Kind". 
Just as **types** classify **values** (e.g., `True` is a value of type `Bool`), **kinds** classify **types**.
- **`*` (pronounced "Type")**: This is the kind of a concrete type that can actually hold values at runtime. For example, `Int`, `Bool`, `String`, and `Maybe Int` are all of kind `*`.
- **`* -> *`**: This is the kind of a *type constructor* that takes one concrete type and returns a new concrete type. For example, `[]` (List) and `Maybe` are of kind `* -> *` because they need a type argument (like `Int`) to become a concrete type (`[Int]` or `Maybe Int`) of kind `*`.

With that in mind, the first chapter will focus on the minimal types of the simplest kind `*` while the second chapter will be devoted to the minimal types of the second simplest kind `* -> *`.

## Chapter 1: Types of Kind `*`

These are standard, concrete types that take no parameters. A type of kind `*` can contain any finite number of inhabitants (and here we will focus specifically on types containing exactly 0, 1, or 2 inhabitants) or a countably infinite number of inhabitants (we will also later have a section devoted to minimal countably infinite types).

Furthermore, it is a fundamental property of type theory that any finite types with exactly $n$ inhabitants are strictly **isomorphic** to each other. Because they have the exact same number of possible distinct runtime values, they can be perfectly mapped back and forth without losing any information. For example, the `Bool` type (which has 2 inhabitants: `True` and `False`) is perfectly isomorphic to the type `Either () ()` (which also has 2 inhabitants: `Left ()` and `Right ()`). *(Note: As we talked about in the Annex, as long as we treat Haskell as a total language and ignore `_|_`, this isomorphism holds perfectly true in the Haskell type-checker!)*

### 1. 0 Inhabitants (Uninhabited Type)

An uninhabited type (or empty type) is a type that has absolutely no data constructors. Because there are no constructors, it is impossible to create a value of this type at runtime.

#### 1. Custom Empty Data
You can construct your own empty type with no constructors simply by providing no right-hand side.
```haskell
data Never

-- This is impossible! There is no constructor to provide.
-- myNever :: Never
-- myNever = ???
```
*(Note for beginners: The name `Never` is completely arbitrary! You could name this `Void`, `Empty`, `MyImpossibleType`, or anything else; the compiler only cares that it lacks data constructors.)*

#### Functions Returning an Uninhabited Type

What happens if we try to write a function that *returns* our empty type `Never`?
For a standard function like `createNever :: a -> Never`, it is **impossible** to provide a valid total implementation (for a discussion on computations that crash or loop indefinitely, see the section on "Bottom" in the Annex). Because we cannot instantiate a `Never`, we can never return one.

But **importantly**, it *is* possible to implement functions returning `Never` **if one of the inputs is also impossible to provide!**
For example, consider the signature `f :: a -> Never -> b -> Never`. Because the caller can never properly supply the second argument (`Never`), it is mathematically sound (based on the **Principle of Explosion**, or *ex falso quodlibet*, which states that from a false premise any conclusion can be drawn) to return a `Never`. This principle is a direct application of the Curry-Howard correspondence [1]. We do this by proving to the compiler that the code branch is unreachable.


Here is how you can implement such a function using an empty case expression:
```haskell
-- Requires {-# LANGUAGE EmptyCase #-}
f :: a -> Never -> b -> Never
f _ impossibleValue _ = case impossibleValue of {}
```

There are also a few other ways to implement this without explicitly writing `case ... of {}`:
1. **Using `LambdaCase`**: With `{-# LANGUAGE LambdaCase #-}`, you can drop the variable name and write `\case {}`.
2. **Using standard library tools (`absurd`)**: If you use the standard library's `Void` type instead of a custom `Never`, the library provides a function called `absurd :: Void -> a`. Because `absurd` can return *any* type `a`, you can simply use it to return `Void` right back! `f _ imposs _ = absurd imposs`.

*(For the deep technical details on how the compiler handles matching on uninhabited types, refer to the [GHC User Guide on EmptyCase](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/empty_case.html)).*

However, we rarely need to write our own custom empty types because Haskell's standard library provides a built-in one!

#### 2. `Data.Void`
Haskell provides a standard empty type called `Void` in the `Data.Void` module (see the [Hackage `Data.Void` documentation](https://hackage.haskell.org/package/base/docs/Data-Void.html)).
```haskell
import Data.Void (Void)
-- Void has 0 constructors.

-- Like Never, it cannot be instantiated.
-- myVoid :: Void
-- myVoid = ???
```
Because `Data.Void` has exactly 0 inhabitants just like our custom `Never` type, they are strictly **isomorphic**. However, `Data.Void` is overwhelmingly preferred in practice because it is the standard empty type and comes bundled with powerful tooling for working with impossible values:
- `absurd :: Void -> a`: Since it is impossible to ever actually have a value of type `Void`, a function taking it can return *any* type `a`. This is used to exhaustively handle impossible code branches (see the Annex).
- `vacuous :: Functor f => f Void -> f a`: If you map over a functor that "contains" `Void` (which means it's structurally empty, like `Right` on an `Either Void a`), you can safely cast it to contain any type `a` instead.

**Common Idioms:**
One of the most frequent patterns when dealing with impossible states is collapsing a sum type where one branch can never happen.
```haskell
-- Logic: If it's a Left, it contains a Void (impossible).
--        If it's a Right, it contains an 'a' (the value we want).
collapse :: Either Void a -> a
collapse (Left v)  = absurd v  -- Use the "impossible" handler
collapse (Right x) = x         -- Just return the value
```

#### 3. The Usefulness of Uninhabited Types

Uninhabited (Empty) types might seem useless at first glance since you can never construct them. However, they are incredibly powerful tools for the compiler.

##### Example 1: Proving Unreachability

Imagine a function that performs a computation that either yields a value of type `a` or fails with an error string:
```haskell
import Data.Void (Void, absurd)

type Result e a = Either e a
```
What if we have a computation that is *guaranteed* to succeed and never throw an error? We can enforce this at the type level using `Void`:
```haskell
safeComputation :: Result Void Int
safeComputation = Right 42
```
Because the `Left` branch requires a `Void`, the caller *knows with absolute certainty* that `safeComputation` will only ever return `Right`. If the caller pattern matches on it, they can safely ignore the `Left` case using `absurd`:
```haskell
extractSafe :: Result Void a -> a
extractSafe (Right val) = val
extractSafe (Left v) = absurd v -- The compiler knows this branch is unreachable
```

##### Example 2: Phantom Types for Type Safety

Empty type declarations are commonly used as tags for **Phantom Types**. A phantom type parameter is one that appears on the left side of a type definition but not on the right.

```haskell
data USD -- 0 inhabitants
data EUR -- 0 inhabitants

newtype Money currency = Money Double

fiveDollars :: Money USD
fiveDollars = Money 5.0

fiveEuros :: Money EUR
fiveEuros = Money 5.0

-- This will result in a compile-time error:
-- error: Couldn't match type ‘EUR’ with ‘USD’
-- illegalSum = fiveDollars `addMoney` fiveEuros
```
Because `USD` and `EUR` have no constructors, we never intended to instantiate them. We only use them as "labels" at compile-time to prevent mixing up currencies. The compiler will now throw an error if we accidentally try to add dollars and euros together, completely eliminating a whole class of bugs at zero runtime cost!

#### 4. Exercises: Building the Impossible

It is actually a great exercise to understand how to implement the standard empty type tooling yourself!

**Exercise 1: Implementing the Impossible**

1. Given your own custom empty type `data Never`, how would you implement your own `absurd :: Never -> a`?
2. Now, using your defined `absurd` function, how would you implement `vacuous :: Functor f => f Never -> f a`?

<details>
<summary><b>View Solutions</b></summary>

**1.** By enabling the `EmptyCase` language extension, we can pattern match on the impossible value. Since the compiler sees there are 0 constructors for `Never`, we don't even have to provide a right-hand side for the case expression!

```haskell
{-# LANGUAGE EmptyCase #-}

data Never

absurd :: Never -> a
absurd v = case v of {}
```

**2.** Because `absurd` can turn a `Never` into any type `a`, all we need to do is map it over the functor!

```haskell
vacuous :: Functor f => f Never -> f a
vacuous = fmap absurd
```
</details>

**Exercise 2: Unreachable branches**
Define a function `requireRight :: Either Void a -> a` that extracts the value safely without using `error` or getting non-exhaustive pattern warnings.

<details>
<summary><b>View Solution</b></summary>

```haskell
requireRight :: Either Void a -> a
requireRight (Right x) = x
requireRight (Left v)  = absurd v
```
</details>

**Exercise 3: Phantom Status**
Imagine a `Document status` type where `status` can be `Draft` or `Published` (both uninhabited types). Write a function signature `publish :: Document Draft -> Document Published` and explain why you cannot accidentally pass a `Published` document to `publish`.

<details>
<summary><b>View Solution</b></summary>
Because `publish` explicitly requires a `Document Draft`, providing a `Document Published` will result in a compile-time type mismatch error. This guarantees at compile time that we only publish drafts, and prevents re-publishing already published documents!
</details>

**Exercise 4: A Tree Without Leaves**
Consider a simple parameterised binary tree:
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```
If we use the `Void` type as the type parameter `a` to declare the type `Tree Void`, what happens when we try to construct a value of this type? What does this imply about the shape of the tree?

<details>
<summary><b>View Solution</b></summary>
Because it is impossible to instantiate a `Void`, we can never use the `Leaf` constructor (which requires a `Void` value). This means any valid value of type `Tree Void` can *only* be constructed using `Node`s. Therefore, a `Tree Void` must be an **infinitely deep tree** containing no leaves!
</details>

### 2. 1 Inhabitant (Unit Type)

A 1-inhabitant type has exactly one possible value. Inspecting the value tells you nothing new—it simply conveys "this computation finished" or acts as a structural placeholder.

#### 1. Custom Unit Types
You can easily define your own single-inhabitant types if you want them to carry a specific semantic meaning in your application rather than using generic units.
```haskell
data Acknowledged = Acknowledged

-- We can instantiate it using its single constructor:
myAck :: Acknowledged
myAck = Acknowledged
```

#### 2. The Standard Unit `()`
The most common 1-inhabitant type in Haskell is `()` (pronounced "unit"). Its only value is also written as `()`.
```haskell
-- The type is (), the value is ()
myUnit :: ()
myUnit = ()
```

#### 3. Other Library Unit Types
While `()` is standard, Haskell libraries often use specialized 1-inhabitant types for specific contexts:
- **`Data.Functor.Identity`**: The `Identity ()` type has only one inhabitant (`Identity ()`). It is used as a base functor that doesn't add any effects.
- **Type Equality `(:~:)`**: From `Data.Type.Equality`, a value of type `a :~: a` has exactly one inhabitant, `Refl`, representing a proof that two types are equal.

Because `Acknowledged`, `()`, `Identity ()`, and `a :~: a` all have an identical cardinality of 1 (a single constructor), they are all formally **isomorphic** to one another.

### 3. 2 Inhabitants (Boolean Type)

A 2-inhabitants type represents a binary choice. It contains exactly two distinct values.

#### 1. The Standard `Bool`
The most ubiquitous 2-inhabitant type is `Bool`, which represents logical truth.
```haskell
data Bool = False | True
```

#### 2. Using `Either () ()`
In type algebra, a sum type adds the inhabitants of its branches. Since `()` has 1 inhabitant, `Either () ()` has exactly 1 + 1 = 2 inhabitants: `Left ()` and `Right ()`. It is structurally isomorphic to `Bool`.
```haskell
type IsTrue = Either () ()
```

#### 3. Custom Enumerations
Instead of using `Bool` everywhere (which can lead to "boolean blindness"), it's highly idiomatic in Haskell to define custom 2-inhabitant types.
```haskell
data SwitchState = On | Off
data AccessLevel = Admin | User
```

---

## Chapter 2: Parameterized Types of Kind `* -> *`

These are type constructors that require one type argument `a` before they become concrete types. The number of inhabitants discussed here applies *regardless* of what `a` is instantiated to (i.e. the type parameter `a` is completely ignored at the value level).

### 1. 0 Inhabitants 

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

### 2. 1 Inhabitant

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

### 3. 2 Inhabitants

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

---

## Annex 

### The Secret Inhabitant: Bottom (`_|_`)

In mathematical logic, it is impossible to return a value from a function without inputs if there are no values to return. However, Haskell is not a pure mathematical language; it is a real programming language. Because of this, every single type in Haskell has a secret, hidden inhabitant known as `_|_` (pronounced "bottom").

"Bottom" represents a computation that fails to complete successfully. This can happen in two main ways:
1. The program crashes (e.g., by throwing an error, or by evaluating `undefined`).
2. The program gets stuck in an infinite loop and never returns.

If you wrote `createNever :: a -> Never`, you could technically "implement" it by writing `createNever x = undefined` or `createNever x = createNever x`. It would compile and satisfy the type checker, but it's fundamentally cheating because it avoids returning altogether by crashing or looping forever! Because `_|_` inhabits every type, you can use it to satisfy any signature, even impossible ones.

However, Haskellers typically reason about their code by assuming it terminates and doesn't crash, treating it as if it were a total language. This approach is formally justified in the well-known paper *"Fast and Loose Reasoning is Morally Correct"* [2].

---

### Bibliography

1. Howard, W. A. (1980). *The formulae-as-types notion of construction*. In To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism (pp. 479-490). Academic Press. (The seminal work establishing the Curry-Howard correspondence connecting uninhabited types to logical falsity).
2. Danielsson, N. A., Hughes, J., Jansson, P., & Gibbons, J. (2006). *Fast and Loose Reasoning is Morally Correct*. ACM SIGPLAN Notices, 41(1), 273-284. (A formal justification for reasoning about Haskell programs while ignoring `_|_`).
3. Leijen, D., & Meijer, E. (1999). *Domain Specific Embedded Compilers*. ACM SIGPLAN Notices, 35(1), 109-122. (An early and influential paper showcasing the use of Phantom Types in Haskell).
4. King, A. (2019). *[Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)*. (A highly influential post demonstrating how to use the type system, including uninhabited types, to prove properties and prevent invalid states).
5. Diehl, S. *[What I Wish I Knew When Learning Haskell](https://smunix.github.io/dev.stephendiehl.com/hask/tutorial.pdf)*. (A comprehensive guide to practical Haskell, covering many advanced type-level mechanics including `Void` and phantom types).

5. Diehl, S. *[What I Wish I Knew When Learning Haskell](https://smunix.github.io/dev.stephendiehl.com/hask/tutorial.pdf)*. (A comprehensive guide to practical Haskell, covering many advanced type-level mechanics including `Void` and phantom types).
