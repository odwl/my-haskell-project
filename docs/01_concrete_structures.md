# Part 1: The Structures (No Laws)
**Author:** Olivier De Wolf, odewolf@gmail.com

## Table of Contents
- [Introduction](#introduction)
- [A Quick Primer: What is a "Kind"?](#a-quick-primer-what-is-a-kind)
- [Chapter 1: Types of Kind `Type`](#chapter-1-types-of-kind-type)
  - [Section 1.1: `Void` (0 Inhabitants / Initial Object)](#section-11-void-0-inhabitants--initial-object)
    - [1. Custom Empty Data](#1-custom-empty-data)
    - [2. `Data.Void` - the built-in equivalent.](#2-datavoid---the-built-in-equivalent)
      - [Exercise 1: Implementing the Impossible](#exercise-1-implementing-the-impossible)
    - [3. Type-Level Guarantees](#3-type-level-guarantees)
      - [Exercise 2: Avoiding `fromRight` and Partiality](#exercise-2-avoiding-fromright-and-partiality)
      - [Exercise 3: Traversing Without Failure (Bonus)](#exercise-3-traversing-without-failure-bonus)
      - [Exercise 4: Safe List Processing](#exercise-4-safe-list-processing)
    - [4. Type-Level Phantom Types for Type Safety](#4-type-level-phantom-types-for-type-safety)
      - [Exercise 5: Phantom Status](#exercise-5-phantom-status)
      - [Exercise 6: A Tree Without Leaves](#exercise-6-a-tree-without-leaves)
  - [Section 1.2: `()` (1 Inhabitant / Terminal Object)](#section-12--1-inhabitant--terminal-object)
    - [1. Custom Unit Types](#1-custom-unit-types)
    - [2. The Standard Unit `()`](#2-the-standard-unit-)
    - [3. The "0-Tuple" Intuition](#3-the-0-tuple-intuition)
    - [4. Other Library Unit Types](#4-other-library-unit-types)
    - [5. Exercises: The Power of One](#5-exercises-the-power-of-one)
      - [Exercise 7: A Safe `head`](#exercise-7-a-safe-head)
      - [Exercise 8: Avoiding `fromJust` with `Either`](#exercise-8-avoiding-fromjust-with-either)
  - [Section 1.3: `Bool` (2 Inhabitants / Coproduct of Terminal Objects)](#section-13-bool-2-inhabitants--coproduct-of-terminal-objects)
    - [1. The Standard `Bool`](#1-the-standard-bool)
    - [2. Using `Either () ()`](#2-using-either--)
    - [3. Custom Enumerations](#3-custom-enumerations)
  - [Section 1.4: Infinite Inhabitants (Countable and Uncountable)](#section-14-infinite-inhabitants-countable-and-uncountable)
    - [1. Countably Infinite ($\aleph_0$)](#1-countably-infinite-aleph_0)
    - [2. Uncountably Infinite ($2^{\aleph_0}$)](#2-uncountably-infinite-2aleph_0)
      - [A. Infinite Streams](#a-infinite-streams)
      - [B. Infinite Functions](#b-infinite-functions)
- [Annex ](#annex-)
  - [The Secret Inhabitant: Bottom (`_|_`)](#the-secret-inhabitant-bottom-__)
    - [Interacting with Bottom safely using `IO`](#interacting-with-bottom-safely-using-io)

## Introduction

In type theory and functional programming, a profound dichotomy exists that mirrors mathematics: the distinction between **Structures** (Data Types without inherent laws) and **Algebras** (Typeclasses with mathematical laws).

1. **The Structures (Nouns):** A concrete data type (like `Bool`, `Maybe`, or `Void`) simply defines a "shape in memory" by explicitly declaring how many distinct values (inhabitants) it can hold. It has no strict mathematical laws governing how it must behave; its only rules are structural.
2. **The Algebras (Verbs/Adjectives):** A typeclass (like `Eq`, `Semigroup`, or `Functor`) defines an interface of behavior mapping across these structures. Because these define behavior, they explicitly come with **Mathematical Laws** to ensure that behavior is predictable and compositionally sound.

This document focuses firmly on the first half of that dichotomy: **The Structures.** We will explore how classifying types purely by the number of distinct values they can hold at runtime provides a phenomenally strong foundation for building robust abstractions.

While minimal types (like those with 0 or 1 inhabitant) are omnipresent in pure functional languages like Haskell, they can often feel counter-intuitive or overly abstract to newcomers. Why would we want a type that holds zero values? What is the point of a type with exactly one? This document aims to demystify these concepts. To aid in your learning journey, several hands-on exercises are suggested throughout this guide.

**Intended Audience:** From a pedagogical perspective, this guide is tailored for intermediate Haskell learners and practical software engineers. If you've ever struggled to understand *why* concepts like `Void` or `Proxy` exist in the standard library—rather than just *how* to compile them—this resource provides the foundation. After mastering the *Structures* outlined here, you will be perfectly prepared to study the *Algebras* (Typeclasses and their laws) that bring them to life.

## A Quick Primer: What is a "Kind"?

Before we dive into counting inhabitants, we need to clarify what we mean by "Kind". 
Just as **types** classify **values** (e.g., `True` is a value of type `Bool`), **kinds** classify **types**.
- **`Type`**: This is the kind of a concrete type that can actually hold values at runtime. For example, `Int`, `Bool`, `String`, and `Maybe Int` are all of kind `Type`. *(Note: Older Haskell code uses `*` for this, but modern Haskell uses `Type` imported from `Data.Kind` to avoid conflict with the multiplication operator).*
  - **Also known as**: Concrete Types, Fully Applied Types, Nullary Type Constructors.
  - **Typeclasses**: Typeclasses that operate on fully realized data (like `Eq`, `Ord`, `Semigroup`, and `Monoid`) expect variables of kind `Type`. You can append two lists of strings, but you cannot append an abstract `Maybe`.
- **`Type -> Type`**: This is the kind of a *type constructor* that takes one concrete type and returns a new concrete type. For example, `[]` (List) and `Maybe` are of kind `Type -> Type` because they need a type argument (like `Int`) to become a concrete type (`[Int]` or `Maybe Int`) of kind `Type`.
  - **Also known as**: Higher-Kinded Types (HKTs), Unary Type Constructors.
  - **Typeclasses**: Typeclasses that operate on "shapes" or "containers" (like `Functor`, `Applicative`, `Monad`, `Foldable`, and `Traversable`) expect variables of kind `Type -> Type`.

With that in mind, the first chapter will focus on the minimal types of the simplest kind `Type` while the second chapter will be devoted to the minimal types of the second simplest kind `Type -> Type`.

## Chapter 1: Types of Kind `Type`

These are standard, concrete types that take no parameters. A type of kind `Type` can contain any finite number of inhabitants (and here we will focus specifically on types containing exactly 0, 1, or 2 inhabitants) or an infinite number of inhabitants (we will also later have a section devoted to minimal countably infinite types).

Furthermore, it is a fundamental property of type theory that any finite types with exactly $n$ inhabitants are strictly **isomorphic** to each other. Because they have the exact same number of possible distinct runtime values, they can be perfectly mapped back and forth without losing any information. For example, the `Bool` type (which has 2 inhabitants: `True` and `False`) is perfectly isomorphic to the type `Either () ()` (which also has 2 inhabitants: `Left ()` and `Right ()`). *(Note: As we talked about in the Annex, as long as we treat Haskell as a total language and ignore `_|_`, this isomorphism holds perfectly true in the Haskell type-checker!)*

### Section 1.1: `Void` (0 Inhabitants / Initial Object)

**Categorical Analog:** *The Initial Object* (denoted as $0$). In Category Theory, an initial object has exactly one unique morphism *to* every other object in the category, but none coming in from populated objects. In Haskell, this corresponds to the `absurd` function (which can produce any type from `Void`).

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


#### 2. `Data.Void` - the built-in equivalent.
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

> [!NOTE]
> #### Functions Returning an Uninhabited Type
>
> What happens if we try to write a function that *returns* our empty type `Never`?
> For a standard function like `createNever :: a -> Never`, it is **impossible** to provide a valid total implementation (for a discussion on computations that crash or loop indefinitely, see the section on "Bottom" in the Annex). Because we cannot instantiate a `Never`, we can never return one.
>
> But **importantly**, it *is* possible to implement functions returning `Never` **if one of the inputs is also impossible to provide!**
> For example, consider the signature `f :: a -> Never -> b -> Never`. Because the caller can never properly supply the second argument (`Never`), it is mathematically sound (based on the **Principle of Explosion**, or *ex falso quodlibet*, which states that from a false premise any conclusion can be drawn) to return a `Never`. This principle is a direct application of the Curry-Howard correspondence [1]. We do this by proving to the compiler that the code branch is unreachable.
>
>
> Here is how you can implement such a function using an empty case expression:
> ```haskell
> -- Requires {-# LANGUAGE EmptyCase #-}
> f :: a -> Never -> b -> Never
> f _ impossibleValue _ = case impossibleValue of {}
> ```
>
> There are also a few other ways to implement this without explicitly writing `case ... of {}`:
> 1. **Using `LambdaCase`**: With `{-# LANGUAGE LambdaCase #-}`, you can drop the variable name and write `\case {}`.
> 2. **Using standard library tools (`absurd`)**: If you use the standard library's `Void` type instead of a custom `Never`, the library provides a function called `absurd :: Void -> a`. Because `absurd` can return *any* type `a`, you can simply use it to return `Void` right back! `f _ imposs _ = absurd imposs`.
>
> *(For the deep technical details on how the compiler handles matching on uninhabited types, refer to the [GHC User Guide on EmptyCase](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/empty_case.html)).*
>
> However, we rarely need to write our own custom empty types because Haskell's standard library provides a built-in one!
##### Exercise 1: Implementing the Impossible
It is actually a great exercise to understand how to implement the standard empty type tooling yourself!

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

*(Note on safety: Why does `vacuous` not crash if calling `absurd` crashes? Because `absurd` only crashes if you actually give it a `Never` value! If an `f Never` exists at runtime, the structure `f` must logically be "empty" of values—such as `Nothing`, an empty list `[]`, or a `Right`. Consequently, `fmap` traverses the container but never actually finds a `Never` value to apply `absurd` to, meaning the code safely evaluates without crashing!)*
</details>

Uninhabited (Empty) types might seem useless at first glance since you can never construct them. However, they are incredibly powerful tools for the compiler. Here are some very useful common idioms using uninhabited types: Type-Level Guarantees and Type-Level Phantom Types for Type Safety.

#### 3. Type-Level Guarantees

One of the most frequent patterns when dealing with impossible states is safely extracting a value from a sum type where one branch can never happen. This section will demonstrate how to elegantly establish and resolve these type-level guarantees by leveraging the `Either Void a` structure alongside the `either absurd id` idiom. *(Note: you can seamlessly apply the exact same logic to the right side using `Either a Void` and `either id absurd`!)*

By encoding the impossibility of failure directly into the type signature (e.g. `Either Void a`), you mathematically prove a computation is *guaranteed* to succeed! This approach is a far safer alternative to relying on notorious partial functions—like `head`, `fromJust`, `read`, or the list index operator `!!`—that will crash your entire program at runtime if handed unexpected input.

> [!NOTE]
> If we have an `Either Void a`, we know the `Left` branch is impossible. The compiler is actually smart enough to know that since `Void` cannot exist, the *entire `Left` constructor* also cannot exist because it demands a `Void` at runtime! If you enable the `EmptyCase` extension (or use newer compiler versions with it built-in), you can pattern match exclusively on the `Right` side. You can completely omit the `Left` case, and the compiler will cleanly accept it *without* issuing any "incomplete pattern match" warnings!

```haskell
{-# LANGUAGE EmptyCase #-}
collapseLeft :: Either Void a -> a
collapseLeft (Right x) = x
-- The compiler mathematically proves Left is impossible and doesn't require us to write it!
```

Instead of writing custom pattern-matching functions like this, it is highly idiomatic in general to just use the standard library's `either` function combined with `id` and `absurd` to collapse the "impossible" branch. 

In fact, a dedicated `collapseLeft` function is almost never explicitly defined in real Haskell codebases because developers simply use `either absurd id` directly inline! This provides a one-line mathematical proof to the compiler, allowing safe extraction without ever using dangerous partial functions like `fromRight`:

```haskell
-- In practice, developers just use `either absurd id` inline!
collapseLeft :: Either Void a -> a
collapseLeft = either absurd id
```

##### Exercise 2: Avoiding `fromRight` and Partiality
Imagine you inherit a codebase where a previous developer used `Data.Either.fromRight` to extract a value from a guaranteed computation by passing a runtime `error` as the default value:
```haskell
import Data.Either (fromRight)

unsafeExtract :: Either Void a -> a
unsafeExtract comp = fromRight (error "This should be impossible!") comp
```
Why is this approach dangerous even if the `Left` branch is `Void`, and how can you write a new `safeExtract` function in a single line using standard library combinators, completely removing the `error`?

<details>
<summary><b>View Solution</b></summary>
By relying on `error` as a default value, the developer bypassed the type-checker and introduced a potential program crash (a bottom value, `_|_`) if the code was ever refactored to something other than `Void`!

You can replace the entirely unsafe helper with the idiomatic mathematical proof we learned! By passing `absurd` to the `Left` handler, we safely prove to the compiler that the left branch is unreachable, which would instantly fail to compile if anyone ever changed `Void` to a real type later on.

```haskell
safeExtract :: Either Void a -> a
safeExtract = either absurd id
```

*(Tip: If the type is ever flipped so the impossible `Void` is on the right side, like `Either a Void`, you can simply flip the handlers to extract your valid value: `either id absurd`!)*
</details>

##### Exercise 3: Traversing Without Failure (Bonus)
The `traverse` function is commonly used to map a fallible function over a sequence of elements:
`traverse :: Applicative f => (a -> f b) -> [a] -> f [b]`
When specialized to `Either e`, its signature effectively becomes:
`traverse :: (a -> Either e b) -> [a] -> Either e [b]`
Imagine you have a list of valid inputs `[a]`, and a specific function `process :: a -> Either Void b` that is *mathematically guaranteed* to succeed (perhaps reusing a parser or computation that theoretically *could* fail on some data, but not on this specific data).
How can you use `either absurd id` to write a function `processAll :: [a] -> [b]` that completely sheds the `Either` wrapper from the resulting list?

<details>
<summary><b>View Solution</b></summary>
Because `process` returns `Either Void b`, mapping it via `traverse process` will return an `Either Void [b]`. Since the type system proves the sequence of computations cannot possibly fail, we can safely extract our final list of results using the exact same idiom!

```haskell
processAll :: [a] -> [b]
processAll xs = either absurd id (traverse process xs)
```
</details>

##### Exercise 4: Safe List Processing
Suppose a trusted API parser guarantees that a deeply-nested JSON payload sequence will never return an error branch, returning `[Either Void User]`. Using the idiom we just learned, how can you elegantly extract a clean `[User]` list?

<details>
<summary><b>View Solution</b></summary>
Since `either absurd id` is a basic, total function `Either Void a -> a`, we just pass it to `map` (or `fmap`) to unwrap every single element! Because it leverages absolute mathematical proofs, it's significantly safer than `Data.Either.rights`, which filters elements without statically proving none were lost.

```haskell
extractUsers :: [Either Void User] -> [User]
extractUsers = map (either absurd id)
```
</details>

#### 4. Type-Level Phantom Types for Type Safety

In many mainstream languages like Java, C++, or Python, developers often rely on `const` modifiers, `final` keywords, or empty "marker interfaces" to tag data and enforce invariants at compile-time. Haskell achieves a much more powerful and flexible version of this exact same concept using **Phantom Types**. 

Empty type declarations are commonly used as tags for these phantom types. A phantom type parameter is one that appears on the left side of a type definition but not on the right.

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


##### Exercise 5: Phantom Status
Imagine a `Document status` type where `status` can be `Draft` or `Published` (both uninhabited types). Write a function signature `publish :: Document Draft -> Document Published` and explain why you cannot accidentally pass a `Published` document to `publish`.

<details>
<summary><b>View Solution</b></summary>
Because `publish` explicitly requires a `Document Draft`, providing a `Document Published` will result in a compile-time type mismatch error. This guarantees at compile time that we only publish drafts, and prevents re-publishing already published documents!
</details>

##### Exercise 6: A Tree Without Leaves
Consider a simple parameterised binary tree:
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```
If we use the `Void` type as the type parameter `a` to declare the type `Tree Void`, what happens when we try to construct a value of this type? What does this imply about the shape of the tree?

<details>
<summary><b>View Solution</b></summary>
Because it is impossible to instantiate a `Void`, we can never use the `Leaf` constructor (which requires a `Void` value). This means any valid value of type `Tree Void` can *only* be constructed using `Node`s. Therefore, a `Tree Void` must be an **infinitely deep tree** containing no leaves!

*(Bonus thought: Any attempt to manually construct a finite `Tree Void` in Haskell would require "cheating" the type system by explicitly placing a bottom (`_|_`) value, such as `undefined` or `error`, in the `Leaf` position!)*
</details>

### Section 1.2: `()` (1 Inhabitant / Terminal Object)

**Categorical Analog:** *The Terminal Object* (denoted as $1$). In Category Theory, a terminal object has exactly one unique morphism coming in *from* every other object. In Haskell, this corresponds to the fact that you can map any arbitrary Type into the Unit type simply by throwing away the input value (`\x -> ()`).

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

#### 3. The "0-Tuple" Intuition
It is highly insightful to understand *why* the unit type is written with empty parentheses `()`. In type algebra, tuples represent multiplication (Product types). For example:
- A pair `(a, b)` has an inhabitant cardinality of $n \times m$.  
- A triple `(a, b, c)` has an inhabitant cardinality of $n \times m \times p$.

If we extend this pattern downwards to an "empty tuple" `()`, it formally represents the **empty product**. In mathematics, the empty product evaluates exactly to **1**. This beautifully explains why `()` has exactly 1 inhabitant and fits perfectly into our mathematical theme! 

Another brilliant consequence of this relates to function arity. Mathematically, an $n$-ary function requires $n$ elements to provide one result.
- A **2-ary** function takes 2 inputs to provide 1 output, written as `(a, a) -> a` or curried as `a -> a -> a`. (Examples: `(+)` or `(*)`).
- A **1-ary** function takes 1 input to provide 1 output, written as `(a) -> a` or simply `a -> a`. (Examples: `not` or `negate`).

So what is a **0-ary** function? It must provide an output out of *no* input. It is equivalent to choosing an element from a set with no prior information. Mathematically, this is modeled via a function from a **singleton set** (a set with exactly one element, like `{*}`) to a target type `a`. The function `f : {*} -> a` simply picks exactly one value of type `a` when you evaluate `f(*)`. 

Because all singleton sets are strictly **isomorphic** to one another (i.e., `()` $\cong$ `{myElement}` $\cong$ `{*}`), the actual value of their single element is completely irrelevant. In Haskell, we use the unit type `()` as the standard, generic representation of this singleton concept. Therefore, a 0-ary computation has the exact signature `() -> a`. This beautifully explains why we use `()` to represent an action or computation that requires no meaningful input to provide a result!

#### 4. Other Library Unit Types
While `()` is standard, Haskell libraries often use specialized 1-inhabitant types for specific contexts:
- **`Data.Functor.Identity`**: The `Identity ()` type has only one inhabitant (`Identity ()`). It is used as a base functor that doesn't add any effects.
- **Type Equality `(:~:)`**: From `Data.Type.Equality`, a value of type `a :~: a` has exactly one inhabitant, `Refl`, representing a proof that two types are equal.

Because `Acknowledged`, `()`, `Identity ()`, and `a :~: a` all have an identical cardinality of 1 (a single constructor), they are all formally **isomorphic** to one another.

#### 5. Exercises: The Power of One

##### Exercise 7: A Safe `head`
The standard library's `head :: [a] -> a` function is notorious for crashing if given an empty list because it lacks a value to return. How could you write a total, non-crashing `safeHead` function using `Either`? What minimal type is the most appropriate for the `Left` error branch if you don't actually need to provide an error message?

<details>
<summary><b>View Solution</b></summary>
When we simply want to signal that a computation failed without attaching any explanation, the Unit type `()` (which has exactly 1 inhabitant) is the perfect minimal error type!

```haskell
safeHead :: [a] -> Either () a
safeHead []    = Left ()
safeHead (x:_) = Right x
```
*(Note: Haskell's `Maybe a` is effectively isomorphic to `Either () a` and is historically preferred for this exact scenario!)*
</details>

##### Exercise 8: Avoiding `fromJust` with `Either`
Similarly, `fromJust :: Maybe a -> a` forces an extraction and crashes if given `Nothing`. If you need to integrate a function returning a `Maybe a` into a pipeline that strictly uses `Either` to handle failures safely, how would you convert it without risking a crash?

<details>
<summary><b>View Solution</b></summary>
We can eliminate the risk of a crash by pattern matching to convert the `Nothing` case into our 1-inhabitant minimal error type `()`, and wrapping the `Just` value into a `Right`!

```haskell
safeFromJust :: Maybe a -> Either () a
safeFromJust Nothing  = Left ()
safeFromJust (Just x) = Right x
```
</details>

### Section 1.3: `Bool` (2 Inhabitants / Coproduct of Terminal Objects)

**Categorical Analog:** *The Coproduct (Sum) of two Terminal Objects* (denoted as $1 + 1 = 2$). In Category theory, adding two single-element sets together creates a two-element set. This fundamentally models branching logic (e.g., "Left or Right", "True or False").

A 2-inhabitants type represents a binary choice. It contains exactly two distinct values.

#### 1. The Standard `Bool`
The most ubiquitous 2-inhabitant type is `Bool`, which represents logical truth.
```haskell
data Bool = False | True
```

#### 2. Using `Either () ()`
Just as tuples `(a, b)` represent **multiplication** (Product types) in type algebra, `Either a b` represents **addition** (Sum types). A sum type simply adds the number of inhabitants of its branches. 

Since `()` has exactly 1 inhabitant, `Either () ()` has $1 + 1 = 2$ inhabitants: `Left ()` and `Right ()`. It is perfectly structurally isomorphic to `Bool`.
```haskell
type IsTrue = Either () ()
```

#### 3. Custom Enumerations
Instead of using `Bool` everywhere (which can lead to "boolean blindness"), it's highly idiomatic in Haskell to define custom 2-inhabitant types.
```haskell
data SwitchState = On | Off
data AccessLevel = Admin | User
```

### Section 1.4: Infinite Inhabitants (Countable and Uncountable)

When we step beyond finite algebraic structures, we enter the realm of infinity. However, in mathematics and type theory, not all infinities are equal in size!

#### 1. Countably Infinite ($\aleph_0$)

Countably infinite types have exactly as many inhabitants as the Natural numbers ($0, 1, 2, 3\dots$). You can theoretically line them up and count them one by one.

**The Base Standard:**
Haskell's `Integer` is the canonical countably infinite type, as it models unbounded whole numbers. 

**The Structural Example (`[()]`):**
Structurally, we can construct a countably infinite type using a recursive Sum type. Consider a standard list containing *only* the Unit type: `[()]`.

Because `()` carries zero information, the only piece of information an entire `[()]` list can possibly hold is its length. It is perfectly isomorphic to the Natural numbers:
* `[]` represents 0
* `[()]` represents 1
* `[(), ()]` represents 2
* ...and so on.

**The Unordered Pair (`UnorderedPair Integer`):**
Another simple example is the unordered pair we discussed previously. Consider the type:
```haskell
data UnorderedPair a = UPair a a
```
If we instantiate this with a countably infinite type like `Integer`, the resulting type `UnorderedPair Integer` is also countably infinite. Even though it pairs two numbers (like `UPair 1 2`), because order doesn't mathematically matter in an *unordered* set (meaning `UPair 1 2` is equivalent to `UPair 2 1`), the total number of distinct pairs remains countably infinite!

**Other Utterly Useful Envelopes:**
If you need a countably infinite type but must conform to specific structural API shapes, you will frequently see basic infinite types wrapped in "containers" that preserve their exact cardinality (*Note: these containers are of kind `Type -> Type` which we will explore in Chapter 2, but when instantiated with an `Integer` they return to kind `Type`!*):
* **The Ordered Pair (`(Integer, Integer)`):** Unlike our unordered pair, the standard tuple distinguishes order, meaning `(1, 2)` and `(2, 1)` are distinct values. Because mathematically $\aleph_0 \times \aleph_0 = \aleph_0$, the standard tuple remains perfectly countably infinite. It's the ubiquitous foundation for returning multiple values, representing 2D Euclidean coordinates, and building Rational numbers (fractions).
* **The Constant Payload (`Const Integer String`):** `Const` is a highly specialized, absolutely pervasive wrapper from `Data.Functor.Const` that physically holds a value of its first type parameter (e.g. `Integer`) while completely ignoring and pretending to hold its second type parameter (`String`). It actually has exactly the same number of inhabitants as `Integer`! It is utterly indispensable in advanced Haskell (like building Lenses or writing Traversals), as it lets you "hijack" a mapping operation that expects to modify a `String`, and instead use it to accumulate a constant `Integer`.
* **The Transparent Box (`Identity Integer`):** Often, complex abstractions like Monad Transformers require a value to be wrapped in a structural shape. The `Identity` type perfectly fills this requirement without adding any branching logic or effects, acting as a transparent zero-cost box around an `Integer`.

#### 2. Uncountably Infinite ($2^{\aleph_0}$)

Haskell's laziness and function semantics allow us to define types that contain an **uncountably infinite** number of possible inhabitants (specifically $2^{\aleph_0}$, possessing the same cardinality as the real numbers).

To achieve an uncountable number of distinct values, a type must represent an *infinite sequence of choices*, where every single step offers at least two branching options.

##### A. Infinite Streams

```haskell
-- Structurally infinite sequence of choices
data Stream a = Cons a (Stream a)

-- Uncountably infinite inhabitants
type CoinFlips = Stream Bool
```

Every `CoinFlips` stream is an infinite sequence of `True`/`False` decisions (e.g. `True, False, False, True...`). Because this represents an infinite branching path of binary choices, it is mathematically isomorphic to a real number between $0.0$ and $1.0$ in binary. There is no mapping that can linearly "count" every possible infinite stream; it is strictly uncountably infinite.

##### B. Infinite Functions

A mathematically pure function mapping from a countably infinite domain (like `Integer`) to a type with at least two inhabitants (like `Bool`) is also uncountably infinite.

```haskell
-- Uncountably infinite inhabitants
type IntegerPredicate = Integer -> Bool
```

This is because defining a single complete function of type `Integer -> Bool` requires making an infinite number of binary choices (the outputs corresponding to the inputs `0, 1, 2...`). Thus, the total number of unique `Integer -> Bool` functions is exactly $2^{\aleph_0}$.

***


## Annex 

### The Secret Inhabitant: Bottom (`_|_`)

In mathematical logic, it is impossible to return a value from a function without inputs if there are no values to return. However, Haskell is not a pure mathematical language; it is a real programming language. Because of this, every single type in Haskell has a secret, hidden inhabitant known as `_|_` (pronounced "bottom").

"Bottom" represents a computation that fails to complete successfully. This can happen in two main ways:
1. The program crashes (e.g., by throwing an error, or by evaluating `undefined`).
2. The program gets stuck in an infinite loop and never returns.

If you wrote `createNever :: a -> Never`, you could technically "implement" it by writing `createNever x = undefined` or `createNever x = createNever x`. It would compile and satisfy the type checker, but it's fundamentally cheating because it avoids returning altogether by crashing or looping forever! Because `_|_` inhabits every type, you can use it to satisfy any signature, even impossible ones.

However, Haskellers typically reason about their code by assuming it terminates and doesn't crash, treating it as if it were a total language. This approach is formally justified in the well-known paper *"Fast and Loose Reasoning is Morally Correct"* [2].

#### Interacting with Bottom safely using `IO`

Because pure functions in Haskell must evaluate consistently, you **cannot** catch a crash (a bottom value like `error`) in pure code. Doing so would make the pure function's behavior depend on exactly *when* the crash was evaluated, breaking referential transparency.

If you absolutely must interact with legacy code that might crash, you must acknowledge that observing a crash is a side effect. Therefore, catching it must happen in the `IO` monad! You can use `Control.Exception.try` combined with `Control.Exception.evaluate` (which forces the pure computation to run so its exceptions can be caught) to wrap a dangerous pure function into a safe `Either`:

```haskell
import Control.Exception (try, SomeException, evaluate)

legacyCrashingCall :: String -> Book
legacyCrashingCall = undefined -- Imagine this throws an error

-- We must wrap it in IO to observe the crash safely
safeWrapper :: String -> IO (Either SomeException Book)
safeWrapper title = try (evaluate (legacyCrashingCall title))
```
By doing this, a complete crash (`_|_`) is safely intercepted and converted into a `Left SomeException` within the `IO` boundary.

***
> For references, papers, and further reading on these algebraic structures, refer to [Part 9: Bibliography](09_bibliography.md).
