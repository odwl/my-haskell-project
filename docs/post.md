# Minimal Functors, Applicatives, and Monads in Haskell

**Author:** Olivier De Wolf

## 1. Introduction

Walking through the exercise of constructing "minimal" instances is one of the best ways to deeply understand Functors, Applicatives, and Monads in Haskell. By stripping away domain-specific noise (like state management, I/O, or failure), we demystify a lot of features that initially look like magic. It reveals the underlying mechanics at play.

While the core concepts structured here are foundational to modern Haskell, this specific teaching narrative—starting with absolute minimalism to actually "prove" the forced hand of parametricity—is something usually only found scattered across different resources. We will synthesize foundational ideas found in Philip Wadler's *"Theorems for free!"* and Sandy Maguire's *"Thinking with Types"*.

In this exploration, our scope is specific: we are focusing entirely on Endofunctors operating within the category of Haskell types (from `Hask` to `Hask`). 

> **Note on `Hask`**: Technically, `Hask` is not a strict mathematical category due to non-terminating programs (represented by `_|_` or "bottom"). For the purposes of reasoning about types, we generally ignore this, a practice validated by the famous paper ["Fast and Loose Reasoning is Morally Correct"](https://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.pdf). 

The true protagonist of this journey is **Parametricity**. Due to parametric polymorphism (the inability to inspect types at runtime), the implementation of most functor, applicative, and monad instances for simple structures is mathematically forced to be unique. This provides immense "intellectual economy" for Haskell developers: operations like `bind`, `pure`, `fmap`, `apply` (`<*>`), and Kleisli composition (`>=>`) generally have exactly one possible correct implementation for simple structural types. The compiler practically writes the code for you. 

*(Note: There are rare counterexamples where multiple valid implementations might exist—for example, traversing a complex tree structure in different orders—but these are atypical for the minimal types we are exploring).*

---

## Chapter 1: The Foundations of Functors, Applicatives, and Monads

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

##### Some "Almost" Functors
Many structures in Haskell *are* valid functors in Category Theory but fail the Haskell language condition above.

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

##### The Great Synthesis: Everything is a Restricted Functor
In all three cases above, we can "fix" the problem by adding a constraint like `Monoid a =>`, `Typeable a =>`, or `Ord b =>`. 

**In Haskell, almost every "non-functor" is actually just a functor on a subcategory.** 
By adding a constraint, you are explicitly telling the compiler: "I am no longer operating on the category of all types (`Hask`); I am now operating only on a subcategory." The standard `Functor` typeclass is simply the special case where that subcategory is the entire category `Hask`.

If we look at valid candidates in Haskell:
*   `Maybe` is a valid functor candidate (Kind `* -> *`).
*   `Identity` is a valid functor candidate (Kind `* -> *`).

#### 3. Mathematical Laws
You must satisfy the Identity and Composition laws:
1.  **Identity Law**: `fmap id == id`
2.  **Composition Law**: `fmap (f . g) == (fmap f) . (fmap g)`

The Identity Law guarantees that mapping the identity function over a structure results in the original structure.

> [!WARNING]
> **Truly Non-Functors (The Lawbreakers)**: Not everything that looks like a functor is one. If a mapping violates the laws, it is not a functor in *any* category.
> 
> *Example (Mutation Lie)*:
> ```haskell
> data Counter a = Counter Int a
> fakeFmap f (Counter n x) = Counter (n + 1) (f x)
> ```
> Here, `fakeFmap id (Counter 0 "x")` yields `Counter 1 "x"`, which is **not** equal to the original. This breaks the **Identity Law** (`fmap id == id`).

**A Crucial Note on Enforcement**: Haskell, the language compiler, does *not* enforce these mathematical laws. It is code; it only checks type signatures. It is entirely the developer's responsibility to ensure their instances are lawful.

Fortunately, in professional Haskell development, you don't need to write these property tests by hand. Libraries like `checkers` provide pre-packaged "batches" for all standard typeclasses. Using `tasty-checkers`, you can mathematically verify your instances across thousands of generated inputs with a single line:

```haskell
import Test.Tasty
import Test.Tasty.Checkers

main :: IO ()
main = defaultMain $ testGroup "Functor Laws"
  [ -- Automatically tests all Functor laws (Identity and Composition)
    testBatch (functor (undefined :: Maybe Int))
  ]
```

This approach becomes even more valuable as we move to Applicatives and Monads, where the number of laws (Identity, Homomorphism, Interchange, etc.) grows significantly.

### Section 1.2: The Atomic Functors

Now that we understand the rules, let's explore the absolute simplest "atoms" we can build in Haskell. These structures are the building blocks of the entire algebraic universe.

#### 1. The Smallest Candidate: `Proxy` (`MinF`)
*(Zero computational data, Zero contextual data).*

The smallest possible Functor holds absolutely the minimum amount of data: **none**.
```haskell
data MinF a = Val -- In Haskell, known as Proxy
```
`MinF` maps any phantom type `a` to a constructor that contains zero term-level data. The type `a` exists only at compile time; at runtime, the box is completely empty.

**Functor Implementation**:
```haskell
instance Functor MinF where
    fmap _ Val = Val
```
**The "Why"**: Due to parametricity, there is exactly one possible implementation that compiles. We are given a function `(a -> b)`. We have a `MinF a` (value `Val`). We must return a `MinF b` (value `Val`). We have no `a` to feed into the function. Therefore, the function *must* be ignored.

#### 2. The Accumulator: `Const`
*(Zero computational data, Some contextual data `r`).*

If `MinF` holds no data, `Const` holds zero *computational* data `a`, but stores an orthogonal contextual value `r`.
```haskell
newtype Const r a = Const r
```
**Functor Implementation**:
```haskell
instance Functor (Const r) where
    fmap _ (Const r) = Const r
```
**The "Why"**: Just like `MinF`, because we have no `a` to apply the function to, parametricity forces us to ignore the function entirely. Note that at the Functor level, `r` requires no special structure (it doesn't need to be a `Monoid`).

#### 3. The Wrapper: `Identity` (`IdF`)
*(One computational data, Zero contextual data).*

Next is the minimal structure with exactly *one* value: a transparent wrapper.
```haskell
data IdF a = IdVal a -- In Haskell, known as Identity
```
**Functor Implementation**:
```haskell
instance Functor IdF where
    fmap f (IdVal x) = IdVal (f x)
```
**The "Why"**: The type signature demands we produce an `IdF b`. We possess an `x :: a` and a function `f :: a -> b`. The *only* mathematical way to obtain a `b` is to apply `f` to `x`.

### Section 1.3: The Algebra of Functors

So far we have looked at the building blocks:
1.  **`Const r`**: Represents a constant value independent of `a`.
2.  **`Identity`**: Represents the parameter itself ($X$).

Every other algebraic data type in Haskell can be built by **summing** (Alternative constructors) and **multiplying** (Multiple fields) these blocks together!

### Section 1.4: Discovering Molecules (Compounds)

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

#### 5. Functors entirely out of Proxy
*   **Proxy + Proxy = Const Bool**: Summing two Proxies creates two possible empty states. `Either () ()` is isomorphic to a Boolean. Mathematically: $1 + 1 = 2$.
*   **Proxy * Proxy = Proxy**: A product of two empty boxes remains an empty box. Mathematically: $1 \times 1 = 1$.

### Section 1.5: Final Summary of Functors

At this level, Functors are entirely about **Shape and Preservation**. Whether we are dealing with an empty box (`Proxy`), a wrapper (`Identity`), or an infinite chain (`List`), `fmap` ensures that the structure of the data remains physically identical while the values inside are transformed.

#### Type Bundle Taxonomy
Before moving to Applicatives, remember the three tools Haskell gives us to bundle these shapes:

1.  **`type` (Alias)**: No new type created, zero overhead. Use for readability.
2.  **`newtype` (Strict Wrapper)**: Distinct type, zero overhead. Use for type safety (e.g., `UserId`).
3.  **`data` (Full ADT)**: Flexible, supports multiple constructors. Use for complex shapes.

---

## Chapter 2: The Applicative Evolution

Now we step up in power. An `Applicative` is a Functor equipped with two new powers: `pure` (to lift values) and `<*>` (to lift application).

### Section 5.1: The Applicative Atoms

Let's see how our atomic structures "upgrade" to this new level.

#### 1. `MinF` (`Proxy`)
```haskell
instance Applicative MinF where
    pure _ = Val
    Val <*> Val = Val
```
**The "Why"**: Our hands are tied. `pure` gives us an `a`, which we must discard (as `MinF` holds no data). `<*>` combines two empty boxes into one.

#### 2. `Const r` (The Monoid Requirement)
This is the most critical upgrade in the minimal universe.
```haskell
instance Monoid r => Applicative (Const r) where
    pure _ = Const mempty
    Const r1 <*> Const r2 = Const (r1 `mappend` r2)
```
**The "Why"**: 
*   `pure` requires us to produce an `r` out of nothing. We must use the **Identity element** (`mempty`).
*   `<*>` gives us two `r` values and needs one result. We must use the **Binary operation** (`mappend`).
This precisely defines why `Const` requires its context to be a `Monoid` to achieve Applicative status.

#### 3. `Identity` (`IdF`)
```haskell
instance Applicative IdF where
    pure x = IdVal x
    IdVal f <*> IdVal x = IdVal (f x)
```
**The "Why"**: Trivial application. We unwrap, apply, and rewrap.

---

## Chapter 3: The Monadic Conclusion

The `Monad` adds the power of **Context-Dependent Sequencing** via `bind` (`>>=`) or `join`.

### Section 6.1: The Final Upgrades

#### 1. `MinF` (`Proxy`)
```haskell
instance Monad MinF where
    Val >>= _ = Val
```
Flattening an empty box inside an empty box still yields an empty box.

#### 2. `Identity` (`IdF`)
```haskell
instance Monad IdF where
    IdVal x >>= f = f x
```
Pure function application.

#### 3. `Const r` (The Monad Barrier)
**Crucially, `Const r` cannot be a Monad.** 
```haskell
(>>=) :: Const r a -> (a -> Const r b) -> Const r b
```
Because `Const` contains no `a`, we can never execute the function `(a -> Const r b)`. We completely lose whatever `r` value the function *would* have produced, violating the **Left Identity law** (`pure a >>= f == f a`). The evolution stops here.

---

## Conclusion: The Tale of Three Minimals

By starting from these absolute minimal examples, the "magic" evaporates, leaving the elegant logic of types and the algebraic discovery of everything from `Maybe` to `List`.

---

## Annex: Proofs and Derivations

### Proof of Monad Equivalence
If `bind`, `join`, and `kleisli` are equivalent, we should be able to derive them algebraically from one another (assuming we have `fmap` and `pure`).

**Deriving `join` from `bind`**:
```haskell
join mm = mm >>= id  
-- Where mm :: m (m a) and id :: m a -> m a
```

**Deriving `bind` from `join` and `fmap`**:
```haskell
m >>= f = join (fmap f m)
-- fmap f m produces m (m b)
-- join then flattens it to m b
```

**Deriving `kleisli` from `bind`**:
```haskell
(f >=> g) x = f x >>= g
```

### Proof of Unique Functor Identity
Given `data MinF a = Val`, how do we formally prove the only valid function of type `(a -> a) -> MinF a -> MinF a` preserving structure is the identity?

1.  Let `g :: MinF a -> MinF a` be a total, terminating function.
2.  The only inhabited value of `MinF a` at the term level is `Val`.
3.  Therefore, `g Val = Val`.
4.  By definition, the `id` function is `id x = x`. 
5.  Thus, `id Val = Val`.
6.  Since `g Val = id Val` for the sole value of the type, `g = id`.
Because `g` is the only total mapping, `fmap id = id` trivially holds, and no other lawful interpretation exists.

---

## Bibliography
*   **"Theorems for free!"** by Philip Wadler (1989).
*   **"Notions of computation and monads"** by Eugenio Moggi (1991).
*   **"Fast and Loose Reasoning is Morally Correct"** by Nils Anders Danielsson, John Hughes, Patrik Jansson, and Jeremy Gibbons (2006).
*   **"The Typeclassopedia"** by Brent Yorgey (The Monad Reader Issue 13, 2009).
*   *(Recommended Reading)* **"Thinking with Types"** by Sandy Maguire.
*   *(Recommended Reading)* **"Functors, Applicatives, And Monads In Pictures"** by Aditya Bhargava.
*   **"Category Theory for Programmers"** (Introductory Notes) by Bartosz Milewski ([PDF Link](https://ai.dmi.unibas.ch/research/reading_group/milewski-2023-01-30.pdf)).
