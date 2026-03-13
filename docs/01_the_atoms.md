# The Minimal Haskell Series: Part 1 - The Atoms

## 1. Introduction

Walking through the exercise of constructing "minimal" instances is one of the best ways to deeply understand Functors, Applicatives, and Monads in Haskell. By stripping away domain-specific noise (like state management, I/O, or failure), we demystify a lot of features that initially look like magic. It reveals the underlying mechanics at play.

In mathematics, there is a beautiful, recurring pattern: we like to start with the absolute simplest "atoms" (axiomatic primitives) and establish a clear set of combinators to build more complicated structures. We then look for the *closure*—the minimal set that contains all those starting axioms and remains perfectly valid under every possible combination of those operations. 

In this document, we will apply exactly that mathematical lens. We will start by defining the absolute minimal "atomic" Functors and Bifunctors. Next, we will introduce the combinators of our algebra (Sums and Products). Finally, by exploring the closure of these operations, we will demonstrate how you can transparently build incredibly complex, robust algebraic data types (Molecules) without ever breaking the foundational laws of the atoms.

### The Updated $\to$ Ranking (Simplicity to Power)

| Rank | Type Class | Core Idea | Why it's here |
|---|---|---|---|
| 1 | Functor | Mapping | Only 1 law/method. Purely transformative. |
| 2 | Foldable | Reducing | High utility, intuitive "summary" logic. |
| 3 | Applicative | Multi-context | Applying functions across multiple containers. |
| 4 | Alternative | Selection | Adds "OR" logic and "Failure" to Applicative. |
| 5 | Monad | Sequencing | Introduces "Flattening" and step-by-step dependency. |
| 6 | Traversable | Commuting | The most abstract; requires understanding all above. |

While the core concepts structured here are foundational to modern Haskell, this specific teaching narrative—starting with absolute minimalism to actually "prove" the forced hand of parametricity—is something usually only found scattered across different resources. We will synthesize foundational ideas found in Philip Wadler's *"Theorems for free!"* and Sandy Maguire's *"Thinking with Types"*.

**Intended Audience:** This journey is designed for mathematicians, computer scientists, or intermediate Haskell programmers who already grasp the basic syntax and perhaps have a surface-level intuition of Category Theory or Abstract Algebra. If you have ever used a Functor or a Monad but felt a lingering desire to derive them from the absolute mathematical "scratch"—to build an unshakeable, axiomatic understanding of *why* they must exist and behave exactly as they do—this exploration is for you!

In this exploration, our scope is specific: we are focusing entirely on Endofunctors operating within the category of Haskell types (from `Hask` to `Hask`). 

> **Note on `Hask`**: Technically, `Hask` is not a strict mathematical category due to non-terminating programs (represented by `_|_` or "bottom"). For the purposes of reasoning about types, we generally ignore this, a practice validated by the famous paper ["Fast and Loose Reasoning is Morally Correct"](https://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.pdf). 

The true protagonist of this journey is **Parametricity**. Due to parametric polymorphism (the inability to inspect types at runtime), the implementation of most functor, applicative, and monad instances for simple structures is mathematically forced to be unique. This provides immense "intellectual economy" for Haskell developers: operations like `bind`, `pure`, `fmap`, `apply` (`<*>`), and Kleisli composition (`>=>`) generally have exactly one possible correct implementation for simple structural types. The compiler practically writes the code for you. 

*(Note: There are rare counterexamples where multiple valid implementations might exist—for example, traversing a complex tree structure in different orders, or the `List` monad which actually has exactly two valid implementations for `bind`—but these are atypical for the minimal types we are exploring).*

---

## Chapter 1: The Foundations of Functors and Bifunctors

### Section 1.1: What is a Functor?

If you ask a mathematician, they will point you to Saunders Mac Lane, one of the founders of Category Theory. In Category Theory, a functor is a structure-preserving mapping between two categories. It is a ubiquitous concept in mathematics; for instance, you have *Forgetful functors* (which strip algebraic structure) and *Free functors* (which automatically build algebraic structure).

In Haskell, the `Functor` typeclass is a specific implementation of a categorical functor. To be a valid `Functor` in Haskell, you must satisfy three distinct conditions:

#### 1. A Well-Kinded Type Constructor (`* -> *`)
You must be an *Endofunctor* on the category `Hask`. This means you map from `Hask` back to `Hask`. 

*   *Invalid Kind*: `Int` (kind `*`) or `(,)` (kind `* -> * -> *`) are not functors on their own. They don't have the right "shape" to be a container/wrapper. A functor must be a "context" that can hold any type `a`.

#### 2. Unconstrained Morphism Mapping (`fmap`)
You must provide a function `fmap :: (a -> b) -> f a -> f b`. This is the implementation of how "arrows" are mapped between categories. Crucially, in Haskell, this mapping must be **unconstrained**: it must work for *any* type `a` and `b`. You cannot require headers or properties (like `Eq` or `Ord`).

This restriction is forced by **Parametricity**. When we write a polymorphic function in Haskell, the function must be completely ignorant of the types going into it. 

If we have a generic type `a` and need to produce a generic type `b`, we cannot inspect the value, switch on its type, or conjure a `b` out of thin air. This drastic restriction essentially forces our implementations to **preserve structure**. This concept is famously codified in Philip Wadler's paper ["Theorems for free!"](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf), which proves that simply reading the type signature of a polymorphic function tells you almost everything about what the function physically *must* do.



#### 3. Mathematical Laws
You must satisfy the Identity and Composition laws:
1.  **Identity Law**: `fmap id == id`
2.  **Composition Law**: `fmap (f . g) == (fmap f) . (fmap g)`

> [!IMPORTANT]
> **The Parametricity Shortcut**: A remarkable result from Category Theory and Haskell's type system is that **if a parametric function satisfies the Identity Law, it automatically satisfies the Composition Law.** 
>
> This stems from the fact that `fmap` is a parametrically polymorphic function. Its behavior is so constrained by its type signature that it cannot "sneak in" extra logic that would specifically target composed functions differently than identity. This is a core result of Philip Wadler's famous paper: [**"Theorems for free!"**](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf). A formal proof of this is provided in the [Annex](#proof-of-identity-implies-composition).

While parametricity gives us elegant mathematical proofs, we can also automate verification empirically. Using property testing libraries like `quickcheck-classes`, verifying `Maybe` is reduced to one simple line:

```haskell
import Test.QuickCheck.Classes

-- Automatically tests all Functor laws!
testProperties "Maybe Functor" $ functor (Proxy :: Proxy Maybe)
```

> [!WARNING]
> #### Caveat: Testing vs. Proof (Property-Based Testing)
> While `testBatch` provides extreme confidence by checking thousands of random inputs, it is not a formal mathematical proof. Because it relies on **Property-Based Testing** (QuickCheck), it is probabilistic. 
>
> In Haskell, there is a fundamental difference between:
> 1.  **Verification (Testing)**: Checking that the законы hold for *many* random cases.
> 2.  **Proof (Types/Parametricity)**: Using the compiler and Category Theory (the "Shortcut") to guarantee the laws hold for *all* cases.
>
> It is possible (though rare in practice) to write a "malicious" law-breaking functor that passes these tests by only failing on very specific, ungenerated inputs—a concept explored in the [Law-Breaking Functors](#5-law-breaking-functors-non-valid-functors) section.

*(Moved to Section 1.6: The Parallel Functor Ecosystem)*

#### 4. Almost Functors
Many structures look like Functors but fail one of the strict Haskell criteria or the mathematical laws. 

##### 4.1. The Constrained Functors
Many structures in Haskell *are* valid functors in Category Theory but fail the Haskell unconstrained mapping condition.

1.  **The Forgetful Functor (`Monoid` -> `Hask`)**:
    ```haskell
    -- In Category Theory, this is a functor between different categories.
    forget :: Monoid a => a -> a
    forget = id
    ```
    *   *Haskell Status*: Not a `Functor` instance because it requires the `Monoid a` constraint.
2.  **The Type Inspector (`isInt`)**:
    ```haskell
    import Data.Typeable (Typeable, cast)
    import Data.Maybe (isJust)

    -- This is a functor that inspects the object in Category Theory.
    isInt :: Typeable a => a -> Bool
    isInt x = isJust (cast x :: Maybe Int)
    ```
    *   *Haskell Status*: Not a `Functor` instance because it requires `Typeable a`. It won't work for *any* `a`, only those the compiler can reify. This is why we call it a "backdoor": it bypasses the intentional "blindness" of parametric polymorphism.
3.  **The Balanced Tree (`Data.Set`)**:
    ```haskell
    -- Rebuilds a BST based on new values.
    mapSet :: Ord b => (a -> b) -> Set a -> Set b
    ```
    *   *Logic*: A `Set` is implemented as a balanced **Binary Search Tree (BST)**. To maintain the invariant (ordered and unique), every map operation must rebuild the tree using comparisons of the *new* values `b`. Since this requires `Ord b`, it is a **Restricted Functor** mapping to the subcategory of ordered types.

**The Great Synthesis: Everything is a Restricted Functor**
In all three cases above, we can "fix" the problem by adding a constraint like `Monoid a =>`, `Typeable a =>`, or `Ord b =>`. 

**In Haskell, almost every "non-functor" is actually just a functor on a subcategory.** 
By adding a constraint, you are explicitly telling the compiler: "I am no longer operating on the category of all types (`Hask`); I am now operating only on a subcategory." The standard `Functor` typeclass is simply the special case where that subcategory is the entire category `Hask`.

If we look at valid candidates in Haskell:
*   `Maybe` is a valid functor candidate (Kind `* -> *`).
*   `Identity` is a valid functor candidate (Kind `* -> *`).

##### 4.2. The Malicious Functor (Hidden Law-Breaker)
This example illustrates why testing alone isn't proof. It has the correct signature and is parametric, but it "hides" its law-breaking behavior behind a conditional:

```haskell
data MyBox a = MyBox Int a

instance Functor MyBox where
    fmap f (MyBox x val) 
      | x == 12345 = MyBox (x + 1) (f val) -- Breaking Identity
      | otherwise  = MyBox x (f val)      -- Looking Lawful
```
If `testBatch` never randomly generates the integer `12345`, this structure will **pass all your tests** while remaining mathematically invalid!

---

### Section 2.2: Minimal Functors and Bifunctors

Now that we have explored several examples of types that are *not* valid functors, let's reverse the approach. We will define the absolute simplest, most minimal structural types we can physically imagine building in Haskell. We will conduct this exercise for both standard **Functors** (types with a single parameter, `* -> *`) and **Bifunctors** (types with two parameters, `* -> * -> *`). 

The beautiful consequence of choosing structures this simple is that it perfectly demonstrates the "forced hand" of **parametricity**. Because these minimal types contain almost no data, there is mathematically only a single possible way to map over them without violating the type signature. Once we define the type, the compiler practically writes the unique `Functor` and `Bifunctor` instances for us!

These minimal structures act as the "atoms" from which the rest of the algebraic universe is built.

#### Minimal Functors

#### 1. The Absolute Bottom: `Zero`
*(Zero constructors, Zero computational data, Zero contextual data. Mathematically, it uniquely forms the **Initial Object** of the `Hask` category, with the usual caveat of bottom/undefined values (`_|_`) slightly muddying strict categorical purity).*

The mathematically absolute smallest possible Functor has no constructors at all. It represents an uninhabited type—it's mathematically impossible to construct a value of this type. It represents total "nothingness".

```haskell
{-# LANGUAGE EmptyCase #-}

data Zero a -- No constructors!

instance Functor Zero where
    fmap :: (a -> b) -> Zero a -> Zero b
    fmap _ z = case z of {} 
```

**The "Why"**: Because `Zero a` has no constructors, we can never actually instantiate it at runtime. However, the type signature `(a -> b) -> Zero a -> Zero b` is perfectly valid. Actually, any data type with zero constructors (an uninhabited type), regardless of how many type parameters it takes (like `data Zero a b c`), is ALWAYS guaranteed to be a perfectly lawful Functor, Bifunctor, Profunctor, etc. If we were somehow handed a value `z` of type `Zero a`, we prove to the compiler we can produce a `Zero b` by pattern matching on its non-existent constructors, leading to an empty case. Parametricity holds because the transformation is forced by the absolute absence of data.

**Law Verification**:
*   *Identity*: `fmap id z` where `z :: Zero a`. Pattern matching on `z` (empty case) immediately satisfies the law as no value exists to violate it.
*   *Composition*: Guaranteed automatically by parametricity ("Theorems for free!") since the Identity law is satisfied.

#### 2. The Empty Box: `Proxy`
*(One constructor, Zero computational data, Zero contextual data).*

The smallest possible Functor holds absolutely the minimum amount of data: **none**.
```haskell
data Proxy a = Proxy
```
The key here is that there is only one possible way to construct a `Proxy a`: using the empty constructor that produces an empty `Proxy`. It maps any phantom type `a` to a constructor that contains zero term-level data. The type `a` exists only at compile time; at runtime, the box is completely empty.

**Functor Implementation**:
```haskell
instance Functor Proxy where
    fmap :: (a -> b) -> Proxy a -> Proxy b
    fmap _ Proxy = Proxy
```
**The "Why"**: Due to parametricity, there is exactly one possible implementation that compiles. We are given a function `(a -> b)`. We have a `Proxy a` (value `Proxy`). We must return a `Proxy b` (value `Proxy`). We have no `a` to feed into the function. Therefore, the function *must* be ignored.

**Law Verification**:
*   *Identity*: `fmap id Proxy == Proxy == id Proxy`
*   *Composition*: Guaranteed automatically by parametricity ("Theorems for free!") since the Identity law is satisfied.

*(Note: As proven by Wadler's "Theorems for free!", satisfying the Identity law automatically guarantees the Composition law for any parametrically polymorphic functor. We explicitly verify both here and throughout this section purely for the sake of a complete, explicit proof).*

#### 3. The Constant Context: `Const r`
*(Zero computational data, Some contextual data `r`).*

If `Proxy` holds no data, `Const` holds zero *computational* data `a`, but stores an orthogonal contextual value `r`. (We will see later in Chapter 2 that this structure acts as an "Accumulator" once it is upgraded to an `Applicative`). 

```haskell
newtype Const r a = Const r
```
**Functor Implementation**:
```haskell
instance Functor (Const r) where
    fmap :: (a -> b) -> Const r a -> Const r b
    fmap _ (Const r) = Const r
```
**The "Why"**: We need to create an instance of `Const r b`. To do this, we need an instance of `r`. The mapping function `f` cannot help us because we don't have any `a` to feed it! So the only way is to extract the `r` from the passed instance of `Const r a` (via `getConst` or, as done here, simple pattern matching). There is mathematically no other choice. Note that at the Functor level, `r` requires no special structure (it doesn't need to be a `Monoid`).

**Law Verification**:
*   *Identity*: `fmap id (Const r) == Const r == id (Const r)`
*   *Composition*: Guaranteed automatically by parametricity ("Theorems for free!") since the Identity law is satisfied.

**Notes on Specializing `Const`:**
*   **`Const Void`**: If we specialize `r` to `Void` (a type with zero inhabitants), `Const Void` becomes impossible to instantiate at runtime. Thus, `Const Void` is mathematically isomorphic to our completely empty `Zero` functor. It is actually very common in real-world Haskell to write `Const Void` instead of defining a custom `Zero`!
*   **`Const ()`**: If we specialize `r` to the unit type `()` (a type with exactly one inhabitant), we get a functor that safely exists but carries zero bits of information. Thus, `Const ()` is mathematically isomorphic to our empty box `Proxy`! You can translate back and forth between `Proxy` and `Const ()` without losing any data.
*   **`Const Bool`**: If we specialize `r` to `Bool` (a type with exactly two inhabitants), we get a functor that safely exists and carries exactly one bit of information (True or False).

*(We will see in Section 1.3 how these three specific specializations intimately link to the numbers $0$, $1$, and $2$ in algebraic arithmetic!)*

#### 4. The Wrapper: `Identity`
*(One computational data, Zero contextual data).*

Next is the minimal structure with exactly *one* value: a transparent wrapper.
```haskell
newtype Identity a = Identity a
```
**Functor Implementation**:
```haskell
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
```
**The "Why"**: The type signature demands we produce an `Identity b`. We possess an `x :: a` and a function `f :: a -> b`. The *only* mathematical way to obtain a `b` is to apply `f` to `x`.

**Law Verification**:
*   *Identity*: `fmap id (Identity x) == Identity (id x) == Identity x == id (Identity x)`
*   *Composition*: `fmap (f . g) (Identity x) == Identity ((f . g) x) == Identity (f (g x)) == fmap f (Identity (g x)) == fmap f (fmap g (Identity x))`

#### 4. The Exponential: `(->) r` (The Reader)
*(Infinite computational data, delayed by domain `r`).*

While `Either` and `(,)` represent algebra's polynomial addition ($+$) and multiplication ($\times$), functions represent exponents ($a^r$). This forms the "Reader" functor: an environment `r` waiting to produce our `a`.
```haskell
-- The type constructor is `(->) r`. The parameter is `a`.
instance Functor ((->) r) where
    fmap f g = f . g 
    -- Equivalently: fmap = (.)
```
**The "Why"**: We need to produce a function of type `(r -> b)`. We possess a function `g :: r -> a` and a mapping function `f :: a -> b`. The only mathematical way to obtain a `b` from an `r` without cheating is to pipe the argument `r` through `g` to get an `a`, and then pipe that `a` into `f`. This is exactly function composition `(.)`.

**Law Verification**:
*   *Identity*: `fmap id g == id . g == g == id g`
*   *Composition*: `fmap (f . h) g == (f . h) . g == f . (h . g) == f . fmap h g == fmap f (fmap h g)`

##### Exponential Blends and Higher-Order Exponentials
To truly illustrate the power of parametricity, consider what happens when we combine our building blocks (Sums, Products, and Exponentials). Even for these complex concepts, parametricity completely forces the only mathematically valid implementation!

*   **1. The Blended Exponent/Product (`State s`)**: Mathematically $(A \times S)^S$. It computes an $a$ while modifying an environment $s$. 
    ```haskell
    newtype State s a = State (s -> (a, s))

    instance Functor (State s) where
        fmap f (State g) = State $ \s -> 
            let (a, new_s) = g s 
            in (f a, new_s)
    ```
    *The "Why"*: We must produce a function returning `(b, s)`. We possess an initial state `s` and a function `g` returning `(a, s)`. The only legal move is to apply `s` to `g`, extract the resulting `a`, hit that `a` with our `f`, and return it bundled tightly with the new `s`! The state piping is practically written for us by the type system.

*   **2. The Higher-Order Exponential (`Cont r`)**: Mathematically $R^{(R^A)}$. It is a function that takes a callback `(a -> r)` and eventually produces an `r`.
    ```haskell
    newtype Cont r a = Cont ((a -> r) -> r)

    instance Functor (Cont r) where
        fmap f (Cont g) = Cont $ \callback_b -> 
            g (\a -> callback_b (f a))
    ```
    *The "Why"*: This is a brain-bender, but parametricity saves us. We must return an `r`. We possess `callback_b :: (b -> r)` and `g :: ((a -> r) -> r)`. We are forced to pass *something* to `g` that looks like `(a -> r)`. Since we possess a `b -> r`, and an `a -> b`, the only legal move is to compose them: `callback_b . f` is of type `a -> r`. We feed that exact composition to `g`. The types dictate the entire callback logic!

#### Minimal Bifunctors

Just as we started Chapter 1 by looking at the simplest possible Functors (`Proxy`, `Const`, `Identity`), we can apply the exact same "shrinking" exercise to Bifunctors (`* -> * -> *`). While `Either` (Sum) and `(,)` (Product) are the fundamental operations of our algebra, they both contain term-level data. We can go simpler in three distinct ways:

##### 1. The Absolute Simplest: The "Bi-Proxy" (Zero Data)
Just like `Proxy` ignoring its `a`, the simplest Bifunctor ignores *both* `a` and `b`. It is essentially an empty box with two phantom types.

```haskell
data BiProxy a b = BiProxy
```
**Bifunctor Implementation**:
```haskell
instance Bifunctor BiProxy where
    bimap _ _ BiProxy = BiProxy
```
**The "Why"**: The signature demands we produce a `BiProxy c d` (value `BiProxy`). We are given two functions `(a -> c)` and `(b -> d)`. Because we possess neither an `a` nor a `b` to apply the functions to, parametricity forces us to ignore both functions entirely.

**Law Verification**:
*   *Identity*: `bimap id id BiProxy == BiProxy == id BiProxy`
*   *Composition*: `bimap (f . g) (h . i) BiProxy == BiProxy == bimap f h BiProxy == bimap f h (bimap g i BiProxy)`

##### 2. The Unrelated Constant (Context Data Only)
Just like `Const r a` holds an `r` but ignores `a`, we can have a Bifunctor that holds an `r` but ignores both `a` and `b`. *(Notice the exact same parallel here: if we specialize `r` to the unit type `()`, we get `ConstContext ()`, which is mathematically isomorphic to `BiProxy`!)*

```haskell
newtype ConstContext r a b = ConstContext r
```
**Bifunctor Implementation**:
```haskell
instance Bifunctor (ConstContext r) where
    bimap _ _ (ConstContext r) = ConstContext r
```
**The "Why"**: We must produce a `ConstContext r c d`. We possess an orthogonal context value `r`. Because we have no `a` or `b` to transform, we are forced to discard the mapping functions and return the unadulterated context.

**Law Verification**:
*   *Identity*: `bimap id id (ConstContext r) == ConstContext r == id (ConstContext r)`
*   *Composition*: `bimap (f . g) (h . i) (ConstContext r) == ConstContext r == bimap f h (ConstContext r) == bimap f h (bimap g i (ConstContext r))`

##### 3. The One-Sided Constants (Left and Right)
A Bifunctor takes two arguments. We can define Bifunctors that act like `Identity` on one side, and `Proxy` on the other.

**The Left identity (ignoring the right):**
```haskell
newtype ConstLeft a b = ConstLeft a
```
**Bifunctor Implementation**:
```haskell
instance Bifunctor ConstLeft where
    bimap f _ (ConstLeft a) = ConstLeft (f a)
```
**The "Why"**: We need a `ConstLeft c d`. We possess an `a` and a function `(a -> c)`. We are mathematically forced to apply `f` to `a` to produce the required `c`. Since we possess no `b`, the second function is ignored.

**Law Verification**:
*   *Identity*: `bimap id id (ConstLeft a) == ConstLeft (id a) == ConstLeft a == id (ConstLeft a)`
*   *Composition*: `bimap (f . g) (h . i) (ConstLeft a) == ConstLeft ((f . g) a) == ConstLeft (f (g a)) == bimap f h (ConstLeft (g a)) == bimap f h (bimap g i (ConstLeft a))`

**The Right identity (ignoring the left):**
```haskell
newtype ConstRight a b = ConstRight b
```
**Bifunctor Implementation**:
```haskell
instance Bifunctor ConstRight where
    bimap _ g (ConstRight b) = ConstRight (g b)
```
**The "Why"**: We need a `ConstRight c d`. We possess a `b` and a function `(b -> d)`. Parametricity dictates we must apply `g` to `b` to produce the required `d`. The first function is ignored.

**Law Verification**:
*   *Identity*: `bimap id id (ConstRight b) == ConstRight (id b) == ConstRight b == id (ConstRight b)`
*   *Composition*: `bimap (f . g) (h . i) (ConstRight b) == ConstRight ((h . i) b) == ConstRight (h (i b)) == bimap f h (ConstRight (i b)) == bimap f h (bimap g i (ConstRight b))`

##### 4. The Sum Molecule: `Either`
The fundamental co-product of two types.

**Bifunctor Implementation**:
```haskell
instance Bifunctor Either where
    bimap f _ (Left a)  = Left (f a)
    bimap _ g (Right b) = Right (g b)
```
**The "Why"**: `Either` encapsulates a choice. If the constructor contains an `a` (`Left`), we are forced to apply `f` to obtain a `c`. If it contains a `b` (`Right`), we are forced to apply `g` to obtain a `d`.

**Law Verification** (The Developer's Responsibility!):

It is crucial to remember that the Haskell compiler **only checks types, not math**. It will perfectly compile a `Bifunctor` instance as long as the type signatures align, even if it completely violates the Identity and Composition laws! You, the developer, are solely responsible for ensuring your instance mathematically preserves the shape of your data.

While we can easily prove these properties mathematically by hand for simple types (as shown below), in Haskell we can actually automate this verification! Using property testing libraries like `tasty-quickcheck` (and typeclass rule validators like `quickcheck-classes`), we can generate thousands of random instances to guarantee our Bifunctor truly behaves correctly. 

A test suite verifying `Either` can be reduced to one simple line:
```haskell
import Test.QuickCheck.Classes

-- Automatically tests both Identity and Composition!
testProperties "Either Bifunctor" $ bifunctor (Proxy :: Proxy Either)
```

This ensures we never break the two fundamental rules:
*   *Identity*:
    ```haskell
    bimap id id (Left a) == Left (id a) == Left a == id (Left a)
    bimap id id (Right b) == Right (id b) == Right b == id (Right b)
    ```
*   *Composition*:
    ```haskell
    bimap (f . g) (h . i) (Left a) == Left ((f . g) a) == Left (f (g a)) == bimap f h (Left (g a)) == bimap f h (bimap g i (Left a))
    bimap (f . g) (h . i) (Right b) == Right ((h . i) b) == Right (h (i b)) == bimap f h (Right (i b)) == bimap f h (bimap g i (Right b))
    ```

##### 5. The Product Molecule: `(,)`
The fundamental product of two types.

**Bifunctor Implementation**:
```haskell
instance Bifunctor (,) where
    bimap f g (a, b) = (f a, g b)
```
**The "Why"**: A Tuple constructor definitively contains both an `a` *and* a `b`. To produce a tuple of type `(c, d)`, we must apply `f` to the left element and `g` to the right element.

**Law Verification**:
*   *Identity*: `bimap id id (a, b) == (id a, id b) == (a, b) == id (a, b)`
*   *Composition*: `bimap (f . g) (h . i) (a, b) == ((f . g) a, (h . i) b) == (f (g a), h (i b)) == bimap f h (g a, i b) == bimap f h (bimap g i (a, b))`

##### 6. The Dual Exponential: `BiReader r`
Just as we saw functions pull us out of polynomial algebras at the 1D Functor level, an exponential delays computation at the 2D Bifunctor level. Mathematically, it is $(A \times B)^R$. 

**Bifunctor Implementation**:
```haskell
newtype BiReader r a b = BiReader (r -> (a, b))

instance Bifunctor (BiReader r) where
    bimap f g (BiReader h) = BiReader $ \r -> 
        let (a, b) = h r 
        in (f a, g b)
```
**The "Why"**: We are returning a delayed computation of a tuple. We possess a function `h :: r -> (a, b)`. We are given two mapping functions `f :: a -> c` and `g :: b -> d`. The only legal mathematical move is to intercept the environment `r` the moment it arrives, feed it to `h` to obtain our `a` and `b`, apply `f` to `a`, apply `g` to `b`, and return the newly bundled tuple. The entire pipeline is rigidly defined by the types involved.

**Law Verification**:
*   *Identity*:
    ```haskell
    bimap id id (BiReader h) 
    == BiReader (\r -> let (a, b) = h r in (id a, id b))
    == BiReader (\r -> h r)
    == BiReader h
    ```
*   *Composition*:
    ```haskell
    bimap (f . j) (g . k) (BiReader h)
    == BiReader (\r -> let (a, b) = h r in ((f . j) a, (g . k) b))
    == BiReader (\r -> let (a, b) = h r in (f (j a), g (k b)))
    -- Which is equivalent to:
    == bimap f g (BiReader (\r -> let (a, b) = h r in (j a, k b)))
    == bimap f g (bimap j k (BiReader h))
    ```

### Section 2.3: Bifunctors as Binary Operations on Functors

Because a Bifunctor maps two types into a new type, we can think of it mathematically as a **binary operator** on the category of Functors! By taking two existing Functors, $F$ and $G$, and combining them using a Bifunctor operator $B$, we generate an entirely new Functor: $H(x) = B(F(x), G(x))$. 

Let's explore this using our minimal atomic functors (`Zero` and `Proxy`) and our fundamental binary operators: Sum (`Either` or $+$) and Product (`(,)` or $\times$). By interacting them, we see the algebra mirror elementary arithmetic perfectly:

#### 1. Zero + Proxy = Proxy
**Math**: $0 + 1 = 1$.
**Haskell**: `Either (Zero a) (Proxy a)`. 
Since `Zero` is mathematically uninhabited, it is impossible to construct the `Left` side of the `Either`. Therefore, the only possible inhabited value of this structure is `Right Proxy`. Because there is exactly 1 state, it holds zero computational data and precisely zero *bits* of contextual data. It is perfectly isomorphic to `Proxy`.

#### 2. Zero * Proxy = Zero
**Math**: $0 \times 1 = 0$.
**Haskell**: `(Zero a, Proxy a)`.
To construct a tuple, you MUST provide both the left and right sides. Because we can never construct a `Zero`, it becomes impossible to *ever* construct the tuple as a whole. The type is uninhabited, making it perfectly isomorphic to `Zero`.

#### 3. Proxy + Proxy = Const Bool
**Math**: $1 + 1 = 2$.
**Haskell**: `Either (Proxy a) (Proxy a)`.
Since `Proxy` on both sides is an empty box, this structure holds absolutely no computational data `a`. However, it *does* hold exactly 1 bit of information: whether it is the `Left` empty box or the `Right` empty box! Because a Bool has exactly 2 states (True/False), this structure is isomorphic to `Const Bool a`. $1 + 1$ successfully yielded $2$!

#### 4. Proxy * Proxy = Proxy
**Math**: $1 \times 1 = 1$.
**Haskell**: `(Proxy a, Proxy a)`.
We must provide an empty box for the left side and an empty box for the right side. The state `(Proxy, Proxy)` is the *only* possible state this structure can ever be in. Since it has only one state, it yields zero bits of contextual information and holds zero data, bringing us right back to 1. It is isomorphic to `Proxy`.

#### 5. Proxy * Identity = Identity
**Math**: $1 \times X = X$.
**Haskell**: `(Proxy a, Identity a)`.
A tuple containing an empty box and a single `a`. The left side adds no data and has no alternative states. The entire structure simply holds precisely one `a`, making it perfectly isomorphic to `Identity a`.

*(Notice that all the examples in this section were specifically chosen to demonstrate mathematical relations between the exact minimal Functors we have already defined. We haven't built any "new" ADTs yet!)*

#### 6. Constant Functors as The Ordinals
Now that we have seen how `+` and `\times` interact with our minimal atoms, we can finally understand a profound property of the `Const r a` functor. By changing the embedded type `r`, `Const` mathematically represents the discrete numbers (Ordinals) based solely on the number of inhabited states of `r`:
*   **$0$**: `Const Void` (zero inhabitants, isomorphic to `Zero`)
*   **$1$**: `Const ()` (one inhabitant, isomorphic to `Proxy`)
*   **$2$**: `Const Bool` (two inhabitants, exactly as derived by $1 + 1$)
*   **$3$**: `Const Ordering` (three inhabitants: `LT`, `EQ`, `GT`, exactly matching $1 + 1 + 1$)
*   **$4$**: `Const (Bool, Bool)` (four inhabitants, exactly matching $2 \times 2$)
*   **$5$**: `Const (Either Bool Ordering)` (five inhabitants, exactly matching $2 + 3$)
*   **$6$**: `Const (Bool, Ordering)` (six inhabitants, exactly matching $2 \times 3$)
*   **$7$**: `Const (Either (Bool, Bool) Ordering)` (seven inhabitants, exactly matching $4 + 3$)
*   **$n$**: Any `Const r` where `r` is a finite enum with $n$ states...

This conceptually proves why `Const Void` acts as the true algebraic identity for Sum ($0$), and `Const ()` acts as the true algebraic identity for Product ($1$) when subjected to actual Bifunctor addition and multiplication!

By treating Bifunctors as binary operators running on simple atomic Functors, we observe the foundation of Algebraic Data Types emerging exactly like fundamental school arithmetic.

### Section 2.4: Deriving the Atoms from Bifunctors

In mathematical systems, we often don't just invent the "atomic" elements out of thin air. We derive them from the operations themselves. Here, we are deeply interested in extracting "natural" atomic Functors directly out of our foundational Bifunctors.

**The Big Picture**: Our grand architectural goal is to select a minimal set of fundamental Bifunctor binary operations (like `+` and `*`). From this selected set of Bifunctors, we want to "naturally" extract simple, atomic Functors (like $0$ and $1$). Once we have derived these foundational atoms, we can combine them iteratively with our Bifunctors to form their mathematical *closure*. This exact generative process—using Bifunctor operations to compose simple extracted atoms—is the traditional mathematical mechanism for defining entire sub-categories of Functors. This is exactly how we generate the infinitely rich families of everyday Algebraic Data Types we use in programming! In particular, we are deeply interested in extracting atomic Functors that perfectly preserve *parametricity*. By doing so, the type system strictly forces our hand to yield a single, mathematically unique, "correct by construction" implementation for each structure—a profound intellectual economy that we will explore below.

Let's break down exactly how this natural extraction works.
#### 1. Extracting a Functor from a Bifunctor

How do we extract a standard Functor out of a generic Bifunctor? Technically, we can *always* extract a Functor simply by fixing one of the two type arguments to an arbitrary type $T$ (so $F(A) = B(T, A)$). This is mathematically just partial application!

For example:
*   Instead of $A + B$, we fix the left side to `String`: `Either String a`. This yields a Functor representing a computation that either succeeds with an `a` or fails with a `String` error.
*   Instead of $A \times B$, we fix the left side to `Int`: `(Int, a)`. This yields a Functor that simply packages an arbitrary integer alongside an `a`.

*(Note: Because a true Bifunctor is mathematically covariant in both arguments, fixing either the left side $B(T, A)$ or the right side $B(A, T)$ yields a perfectly valid Functor! However, in Haskell, type lambdas are partially applied left-to-right, making fixing the left side the native default syntax).*

However, making a random, arbitrary choice of $T$ (like picking `String` or `Int` out of millions of possible types) is not a "natural" mathematical progression. When you arbitrarily choose a type $T$ to partially apply, you are making an ad-hoc, manual decision. There are infinite possible choices, and none of them are mathematically "more correct" than the others. 

Crucially, **this breaks parametricity if we try to extract the inner data!** Because `String` contains actual data, we cannot write a parametrically polymorphic, total function to extract `a` from `Either String a` without either handling the string (which requires specific knowledge of `String`) or crashing. We lose the ability to generically and losslessly map our structure.

For a completely generic Bifunctor with no special algebraic properties, making an arbitrary choice like this might be the only way to extract a Functor.

#### 2. Bifunctors with Identity ("Naturality")

But if the Bifunctor has a special structural property—such as possessing a left and/or right identity element—then it is better to find a more natural way to extract a Functor! 

At its absolute bare minimum, we just need a **left identity** or a **right identity**. What does this actually mean mathematically? It means there must exist a specific type $I$ along with a perfect two-way mapping—a structural isomorphism—that proves combining $I$ with any type $A$ leaves $A$ completely unchanged (neither losing nor inventing any data):
*   **A Left Identity** requires a structural isomorphism known as the **Left Unitor** (often denoted $\lambda$): proving $B(I, A) \cong A$.
*   **A Right Identity** requires a structural isomorphism known as the **Right Unitor** (often denoted $\rho$): proving $B(A, I) \cong A$.

**Crucial Distinction**: Do not confuse these properties with the `Bifunctor` laws! The Functor/Bifunctor laws (Identity and Composition) govern the *behavior of mapping functions* and must hold via **strict equality** (e.g., `fmap id == id`). In contrast, possessing a Left or Right Identity type is a property of the *data structure itself*, proven via **structural isomorphism** ($\cong$, meaning the shapes can losslessly map to each other even if they aren't strictly identical types).

*(Note: If a binary operation has both, math dictates they must be the identical type $I$. See the [Annex: Proof of Identity Uniqueness](#proof-of-identity-uniqueness) for the derivation!)*

When you use the identity $I$ to perform your partial application, the choice is no longer yours—the inherent structure of the Bifunctor *forces* its own unique canonical choice onto you! That uniqueness is exactly what "naturality" refers to in this context: it arises purely from the structure itself, independent of arbitrary external choices.

This **"forced hand"** is exactly what we are aiming for. In functional programming, we are deeply interested in this kind of **intellectual economy**: we want to identify and produce foundational Functors that have exactly *one* mathematically unique implementation. By relying on naturality, we eliminate arbitrary decisions and derive primitive structures that are completely "correct by construction."

By taking that uniquely canonical identity $I$ and turning it into a constant mapping, we establish the fundamental "Atomic" Functor for that operation naturally. We create a Constant Functor $C(A) = I$.

*(Technical Note: In Haskell, a "Natural Transformation" between two Functors `f` and `g` is exactly the type signature `forall a. f a -> g a`. When we say $B(I, A)$ naturally resolves to $A$, it means we can write a perfect Natural Transformation mapping without losing or inventing data. For example, for the Product `(,)` with identity `()`, the natural transformation to `Identity` is literally just `snd :: forall a. ((), a) -> a`! For Sum `Either` with identity `Void`, it is `extract :: forall a. Either Void a -> a` via absurd. It is a mathematical guarantee encoded seamlessly into the language.)*

Let's classify the "zoo" of Bifunctors we have seen so far based on this profound property:
*   **No Identity**: Bifunctors like `BiProxy` or `ConstContext` have neither a left nor a right identity. To extract a Functor from them, you are forced to make an arbitrary, non-natural choice! *(Why? Because if a left identity `I` existed, then `BiProxy I Bool` must be perfectly isomorphic to `Bool`. But `BiProxy` always has exactly 1 inhabitant, which makes it mathematically impossible to form a two-way mapping with `Bool`'s 2 inhabitants!)*
*   **Left Identity Only**: The function arrow `(->)` is a profound binary operation. It only possesses a left identity `()` (since `() -> a` is isomorphic to exactly one `a`, but `a -> ()` is not `a`). 
*   **Full Identity**:
    *   The **Sum Bifunctor** (`Either` or $+$) has the two-sided mathematical identity $0$ (the `Void` type, since $A + 0 \cong A$). From this, we gracefully extract the constant functor `Const Void` (or `Zero`).
    *   The **Product Bifunctor** (`(,)` or $\times$) has the two-sided mathematical identity $1$ (the `()` type, since $A \times 1 \cong A$). From this, we extract the constant functor `Const ()` (or `Proxy`).

*(Note: This means mathematically, `Proxy` is not truly the "simplest"—it is simply $1$. `Const Void` is strictly smaller as it is exactly $0$!)*

#### 3. The Power of "Families" (Sub-Category Closures)
What happens if we iteratively apply a Bifunctor and its identity? 
By definition, if we only take a single Bifunctor (like $\times$) and its identity ($1$), the mathematical closure is fairly trivial. We can only generate structures like $1$, $1 \times 1$, $1 \times A$, $A \times A$, etc. This forms a flat lineage (just tuples of identical shape or empty structures). If we just take the closure of the identity itself with $A$, we trivially just get the Identity functor.

#### 4. The Magic of Polynomial Functors
However, things get deeply interesting when we take a *set* of two orthogonal interacting Bifunctors—like $+$ and $\times$—and their respective identities. By mixing Sums, Products, Zeros, and Ones, we generate an infinitely rich family of structures. This exact closure is the **Category of Polynomial Functors** (e.g., $1 + A + A \times A...$). This interplay is what allows us to define lists, trees, and essentially every Algebraic Data Type (ADT) in programming. 

#### 5. Is an Identity strictly required?
Must every Bifunctor in our set have an identity? Not necessarily! It is mathematically perfectly valid to consider a set of Bifunctors where only some (or none) have identities (this essentially forms a non-unital algebraic structure). 

But does this restricted set generate an *interesting* subcategory of functors? Absolutely! Let's say we have our two fundamental bifunctors ($+$ and $\times$). Let's assume we possess the Sum Identity $0$ (the `Void` type) but we **do not possess** the Product Identity $1$ (the `()` type/`Proxy`). 

By missing $1$, we can never create a "Nil" or an "Empty" constructor to terminate our recursive shapes. As a profound result, the closure of our variables with merely $\{+, \times, 0\}$ mathematically generates the incredibly restrictive *Subcategory of Non-Empty Data Structures*:
*   **The Non-Empty List**: $NEL(A) = A + A \times NEL(A)$. (Haskell's `Data.List.NonEmpty`).
*   **The Un-emptyable Tree**: $Tree(A) = A + Tree(A) \times Tree(A)$. (A tree where every leaf must have a value).

This subcategory guarantees—at the compiler level—that every single structure geometrically contains at least one $A$. The absence of the mathematical $1$ identity is exactly what powers this profound property!

However, to form the full "Polynomial" category that exactly matches the power of general computer science ADTs, *both* of our fundamental operations ($+$ and $\times$) require their natural identities ($0$ and $1$) to terminate data structures (like using $1$ as the empty `Nil` constructor ending a `List`). 

#### 6. Examples of Deriving Compounds
By leveraging combinations of our extracted identities (`Zero`, `Proxy`) and fundamental functors (`Identity`), we systematically generate powerful structures using the Bifunctor operations.
*   **Optional Data**: $1 + X$. Using Sum: `Either (Proxy a) (Identity a)` is isomorphic to `Maybe a`.
*   **Error Context**: $E + X$. `Either (Const e a) (Identity a)` gives us a computation that succeeds with an `a` or fails with an error `e`.
*   **Logging Context**: $E \times X$. `(Const e a, Identity a)` perfectly mirrors a `Writer` log context bundled with an `a`.

#### The Ultimate Closure: Bicartesian Closed Categories (BCC)
So, we have established our two algebraic bifunctors (Sum and Product) and derived their natural identity atoms ($0$ and $1$). What happens if we take exactly these, and add our third non-algebraic bifunctor: the **Exponential** (`->`)?

If a category contains exactly those three foundational Bifunctor operations (`Either`, `(,)`, and `->`) along with their identities (`Void` and `()`), it fulfills the mathematical requirements to be called a **Bicartesian Closed Category** (BCC).

*   **"Cartesian"**: The category possesses Products ($\times$) and a Terminal Object ($1$).
*   **"Bi-"**: The category *also* possesses Coproducts ($+$) and an Initial Object ($0$).
*   **"Closed"**: The category possesses Exponentials (`->`), allowing functions to be treated as values and evaluated.

This completely "closed" loop of operations is extraordinarily profound. According to the Curry-Howard isomorphism, a Bicartesian Closed Category is the exact mathematical equivalent of **Simply Typed Lambda Calculus**, the theoretical foundation of intuitionistic propositional logic. 

The closure built by these three simple Bifunctors creates the entire logical framework that strongly typed programming languages like Haskell rely on!

### Section 2.5: Generating Functor Subcategories (The Algebra as a Special Case)

*(Note on Terminology: When mathematicians or Haskell programmers say a structure is "algebraic" — as in Algebraic Data Types or ADTs — they mean it is constructed strictly using only polynomial combinations: Sums `+` and Products `*`. Function arrows `->` represent Exponentials, which are conceptually a tier "above" simple algebra!
To make this concrete:
*   **Algebraic**: Things defined exclusively by values and their geometry. This includes types like `Bool` ($1 + 1$), `Maybe` ($1 + X$), `List`, and `Tree`, as well as mathematical structures like **Monoids** and **Groups**.
*   **Non-Algebraic (Exponentials)**: Things that require an execution environment or delayed computation (`->`). This includes types like the `Reader` ($A^R$), `State`, and `Cont`, which are structurally higher-order).*

#### 1. The Algebra of Functors

When you build an algebraic equation in mathematics, like $f(x) = 2x + 1$, you only need two foundational components to start building: your numbers (constants like 1, 2) and your variable ($x$).

For standard Endofunctors (`* -> *`), it is incredibly obvious what our two "atomic" building blocks must therefore be:
1.  **The Constants ($C$)**: `Const r` represents any constant value independent of `x`. At its absolute simplest scale, `Proxy` (or `Const ()`) represents the mathematical constant $1$.
2.  **The Single Variable ($X$)**: `Identity` rigidly represents the single parameter/variable $x$ itself.

Every other single-variable algebraic data type in Haskell can be built by taking these primitives, **summing** them (using Alternative constructors, representing $+$), and **multiplying** them (using Multiple fields, representing $\times$)!

But are Sums and Products Functors themselves? Yes! In Category Theory, operations like Sum ($+$) and Product ($\times$) are specifically known as **Bifunctors** because they map *two* categories (or a product of categories) into one. In Haskell, these are represented by `Either` (Sum) and `(,)` (Product). 

Because they are Bifunctors, if you fix one of their arguments, they immediately become standard Endofunctors (`* -> *`). Furthermore, the category of Functors is closed over these operations: the sum or product of two Functors is inherently a Functor (like `Data.Functor.Sum` and `Data.Functor.Product`).

*(Note: The formal laws governing how these products and sums associate and interact are a bit more complex, requiring them to verify the **pentagon** and **triangle** laws from Monoidal Categories. We will refer to the details of these laws in [Chapter 5](#chapter-5-monoidal-categories) at the end of this journey).*

**Functors entirely out of Proxy:**
To see these Bifunctors in action with our simplest atomic functor, `Proxy`:
*   **Proxy + Proxy = Const Bool**: Summing two Proxies creates two possible empty states. `Either () ()` is isomorphic to a Boolean. Mathematically: $1 + 1 = 2$.
*   **Proxy * Proxy = Proxy**: A product of two empty boxes remains an empty box. Mathematically: $1 \times 1 = 1$.

#### 2. The Algebra of Bifunctors

Is there an algebra for Bifunctors just as there is for standard Functors? Absolutely! Because the category of Functors is closed over Products and Sums, we can combine our foundational Bifunctor atoms exactly the same way to build incredibly complex Bifunctors.

If standard Functors (`* -> *`) are single-variable polynomials like $f(x) = x^2 + 1$, then Bifunctors (`* -> * -> *`) are simply two-variable polynomials like $f(a, b) = a \times b + a$. 

This means it becomes very obvious what our two "atomic variables" are:
*   **The First Variable ($A$)**: `ConstLeft a b = ConstLeft a` (ignoring the right).
*   **The Second Variable ($B$)**: `ConstRight a b = ConstRight b` (ignoring the left).

Equipped with our two atomic variables, we can perform any algebraic operation:
*   **Bifunctor Sums ($+$)**: We can wrap a Bifunctor inside `Either` (e.g. `Either (BiProxy a b) (a, b)`).
*   **Bifunctor Products ($\times$)**: We can tuple Bifunctors together (e.g. `(Either a b, ConstContext String a b)`).
*   **Bifunctor Fixed Points**: Just like `List` recursively nests standard Functors, structures like a `Bifunctor Tree` can recursively nest Bifunctors (e.g. `data BiTree a b = Leaf a b | Node (BiTree a b) (BiTree a b)`).

Anything you can do in one dimension (`* -> *`), Category Theory allows you to transparently extend into two dimensions (`* -> * -> *`) using the exact same polynomial algebra!

#### 3. Composing Functors into a Bifunctor (`Biff`)
While `Compose` elegantly handles nesting a Functor inside another Functor (`f ∘ g`), what happens when we want to compose Functors directly into the independent branches of a **Bifunctor**?

Because a standard Bifunctor `p` takes exactly two type arguments, we can mathematically substitute two independent Functors (`f` and `g`) into those dimensional parameters! In Haskell, this exact compositional bridge is completely formalized by the `Biff` operator in `Data.Bifunctor.Biff`:

```haskell
-- 'p' is a Bifunctor (like Either or Pair)
-- 'f' and 'g' are Functors (like List, Maybe)
newtype Biff p f g a b = Biff (p (f a) (g b))

instance (Bifunctor p, Functor f, Functor g) => Bifunctor (Biff p f g) where
    bimap f1 f2 (Biff pfg) = Biff (bimap (fmap f1) (fmap f2) pfg)
```

`Biff` mathematically proves that if you take a base Bifunctor ($p$) and compose it with two Functors ($f$ and $g$), the structure is mathematically guaranteed to generate a perfectly lawful, brand-new **Bifunctor**!

For example, `Biff Either [] Maybe a b` geometrically creates `Either [a] (Maybe b)`. Because `Either`, `List`, and `Maybe` are completely lawful atoms, `Biff` automatically writes `bimap` for you by natively mapping the left function over the list and the right function over the `Maybe` branch. This flawlessly bridges 1D Functors and 2D Bifunctors in our mathematical closed algebraic system!

### Section 2.6: Polynomial Functors

The relationship between Category Theory and Haskell's **Algebraic Data Types (ADTs)** is formalized through **Polynomial Functors**.

If a functor is built solely from:
-   **Constants**: `Const r` ($C$ or $1$)
-   **Identity**: `Identity` ($X$)
-   **Sums**: `Either` ($+$)
-   **Products**: Tuples ($\times$)

... it is a **Polynomial Functor**. Most standard Haskell ADTs (like `Maybe`, `Either`, and non-recursive records) are polynomial. They are the "algebra" of types, where complex structures are discovered by summing and multiplying simpler ones.

#### Why the Name "Polynomial"?
The terminology is beautifully literal. Think about a regular algebraic polynomial from high school math, like $F(X) = 1 + Int + X^2$. It is built using exactly the same operations:
*   **$X$**: The variable (The Identity Functor).
*   **$1, Int$**: Constants (The Constant Functor).
*   **Multiplication ($X^2 = X \times X$)**: Products (Tuples `(a, a)`).
*   **Addition ($+$)**: Sums (`Either` or alternative constructors).

When we build an Algebraic Data Type (ADT) in Haskell, we are quite literally writing a polynomial equation. For example, consider this functor:
```haskell
data Shape a = Empty | Point Int | Line a a
```
If we translate this to algebra using our building blocks:
*   `Empty` has zero parameters: It is $1$ (a constant, `Proxy`).
*   `Point Int` has an `Int` but no parameter `a`: It is the constant $Int$.
*   `Line a a` has two parameters (a pair): It is the product of identity with itself, $X \times X = X^2$.

So, the polynomial functor shape for `Shape a` is mathematically written as: 
**$F(X) = 1 + Int + X^2$**

### Section 2.7: The Parallel Functor Ecosystem (Solutions for Restricted Functors)

As we briefly highlighted in Section 2.1, the mathematical definition of a functor is far broader than Haskell's native `Functor` typeclass (which strictly maps `* -> *` unconstrained). When structures inevitably violate these two rules, we do not throw our hands up in defeat! 

The Haskell ecosystem simply defines *parallel* typeclasses to capture these different categorical mappings, allowing us to retain the exact same structural guarantees.

#### 1. The Too-Wide Functor: `Bifunctor`
If a structure has a kind of `* -> * -> *` (like `Either` or `(,)`), it is a perfectly valid functor mapping from the product category $Hask \times Hask \to Hask$. Because it requires two types, we use `Data.Bifunctor`:
```haskell
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
```
*(We will completely deconstruct these in [Chapter 4](#chapter-4-deep-dive-into-bifunctors)).*

#### 2. The Reverse Functor: `Contravariant`
A standard Functor maps "covariant" inputs (it *produces* values). But what if a structure only *consumes* values? This is mathematically a functor mapped from the opposite category: $Hask^{op} \to Hask$. 

If you have a `Predicate a` (a wrapper around `a -> Bool`), you can't map its output (`Bool`), but you can map its input!
```haskell
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a  -- Notice the reversed 'b' and 'a'!
```

#### 3. The Mixed Functor: `Profunctor`
If a Bifunctor maps two covariant types, a **Profunctor** is a mapping over one contravariant shape and one covariant shape. The standard function arrow `(->)` is a Profunctor.
```haskell
class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
```
`dimap` allows you to simultaneously map the *incoming* argument (before the function runs) and the *outgoing* result (after the function runs). They form the categorical backbone of the `lens` library.

#### 4. The Constrained Functor: `MonoFunctor` (The `mono-traversable` library)
Recall that `Data.Set` fails to be a `Functor` because rebuilding its internal tree requires an `Ord a` constraint on mapping. It is a "Restricted Functor" mapping only onto a subcategory. 

Similarly, structures like `ByteString` or `Text` aren't parametric at all (they have kind `*`), but logically act precisely like containers. To solve this, Michael Snoyman's `mono-traversable` library created the `MonoFunctor` typeclass:
```haskell
class MonoFunctor mono where
    omap :: (Element mono -> Element mono) -> mono -> mono
```
This allows us to maintain the interface and laws of a Functor over mathematically restricted or entirely monomorphic structures.

### Section 2.8: Discovering Molecules (Compounds)

Using these "atoms," let's see how we can discover the rest of the Haskell universe.

#### 1. The Sum Molecule: `Maybe`
If we take the **Sum** (`+` in algebra, `Either` in Haskell) of `Proxy` (the number $1$) and `Identity` ($X$), we get the structure for choice or failure:
`Maybe a ≅ Sum Proxy Identity a ≅ Either () a`
**Algebraically**: $1 + X$

#### 2. The Product Molecule: `Writer`
If we take the **Product** ($\times$ in algebra, a Tuple in Haskell) of a constant `Const r` and `Identity` ($X$), we get a structure that carries a "log" along with the value:
`Writer r a ≅ Product (Const r) Identity a ≅ (r, a)`
**Algebraically**: $r \times X$

*(Note: `Proxy * Identity ≅ ((), a) ≅ a ≅ Identity`. Proxy acts as the number $1$ in multiplication).*

#### 4. The Infinite Chain: `List`
By using both Sums and Products with **Recursion**, we can build a list. A list is either empty (`Proxy`) OR a head and a tail (`Product Identity List`).
`List a ≅ Sum Proxy (Product Identity List) a`
**Algebraically**: $L(X) = 1 + X \times L(X)$

> **Is $1 + X \times W = W$ always the case?**
> Looking at the list equation, you might ask: "is it always the case that `Sum Proxy (Product Identity Whatever) = Whatever`?"
> The answer is no! The formula $1 + X \times W$ describes the "shape" of a single layer of a List. When we say $L(X) = 1 + X \times L(X)$, we are saying that `List` is exactly the type that satisfies this equation (it is the *Fixed Point* of that functor). If `Whatever` was a Binary Tree, its shape equation would look entirely different, such as $T(X) = 1 + X \times T(X) \times T(X)$.

