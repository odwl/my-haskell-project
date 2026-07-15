# Part 2: The Algebras (Laws) for Concrete Types

## Table of Contents
- [Chapter 1: Equivalence and Ordering](#chapter-1-equivalence-and-ordering)
  - [Section 1.1: `Eq` (The Laws of Mathematical Equivalence)](#section-11-eq-the-laws-of-mathematical-equivalence)
  - [Section 1.2: `Ord` (The Laws of Total Ordering)](#section-12-ord-the-laws-of-total-ordering)
- [Chapter 2: Associative Binary Operations ($+$ and $\times$)](#chapter-2-associative-binary-operations--and-)
  - [Section 2.1: `Semigroup` and `Monoid`](#section-21-semigroup-and-monoid)
    - [1. A Well-Kinded Type (`Type`)](#1-a-well-kinded-type-type)
    - [2. Two Core Operations](#2-two-core-operations)
    - [1. The Absolute Minimum (1 Inhabitant)](#1-the-absolute-minimum-1-inhabitant)
    - [2. Types with 2 Inhabitants (`Bool`)](#2-types-with-2-inhabitants-bool)
    - [3. Types with 3 Inhabitants (e.g., `Ordering`)](#3-types-with-3-inhabitants-eg-ordering)
    - [4. Types with Countably Infinite Inhabitants (e.g., `Integer`)](#4-types-with-countably-infinite-inhabitants-eg-integer)
    - [5. The Free Monoid (`[a]`)](#5-the-free-monoid-a)
    - [6. Why do `Sum`, `Product`, `Max`, and `Min` stand out?](#6-why-do-sum-product-max-and-min-stand-out)
    - [Category Theory Origin: The Single-Object Category](#category-theory-origin-the-single-object-category)
- [Annex: The Category Hask](#annex-the-category-hask-)
  - [Hask: The Category of Haskell Types](#hask-the-category-of-haskell-types)
  - [The Secret Inhabitant: Bottom (`_|_`)](#the-secret-inhabitant-bottom-__)
    - [Interacting with Bottom safely using `IO`](#interacting-with-bottom-safely-using-io)

Welcome to the second part of Universe 1. In Part 1, we defined our core **Structures**—the bare mathematical geometry of how many values a type can hold. We looked at the Initial Object (`Void`), the Terminal Object (`()`), the Coproduct of Terminal Objects (`Bool`), and both Countable (e.g., `[()]`, `Integer`) and Uncountable (e.g., `Stream Bool`, `Integer -> Bool`) infinite inhabitants.

But structures alone are sterile. To actually perform computation, we need **Algebras**. An algebra assigns specific *behaviors* to our structures. In Haskell, we implement these algebras using Typeclasses. But unlike simple interfaces in other programming languages, a true algebra must come with **Mathematical Laws** to ensure the behavior is predictably sound.

In this document, we will build out the fundamental algebras that operate directly on concrete types of kind `Type`.

## Chapter 1: Equivalence and Ordering

Before we can combine values or map over structures, the most fundamental operation a computer can perform is determining if two things are the same.

### Section 1.1: `Eq` (The Laws of Mathematical Equivalence)

Before looking at the operations of `Eq`, we must ask: what *kinds* of types can have an `Eq` instance? Because the operators compare fully instantiated runtime values, any type implementing `Eq` **must be a simple kind `Type`**. We cannot test if two uninstantiated type constructors (like `Maybe`) are equal; we can only test if two concrete values (like `Maybe Int`) are equal.

The `Eq` typeclass provides the `(==)` and `(/=)` operators.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    {-# MINIMAL (==) | (/=) #-}
```

Notice the `{-# MINIMAL (==) | (/=) #-}` pragma. This means we only *need* to implement one of the two operators. If we write `(==)`, Haskell provides a default implementation `x /= y = not (x == y)` (and vice versa).

To be a valid instance, it must rigorously satisfy the three mathematical laws of an **equivalence relation**:

1. **Reflexivity**: Everything is equal to itself.
   `x == x` must be `True`.
2. **Symmetry**: Order of comparison doesn't matter.
   `x == y` implies `y == x`.
3. **Transitivity**: Equality chains perfectly.
   If `x == y` and `y == z`, then `x == z`.

**Testing the Laws (`tasty-quickcheck` & `testBatch`)**

As we discussed in the Introduction, these laws are mathematically absolute. But we don't need to manually verify them! We can write property tests using `tasty-quickcheck` to systematically generate random values and mechanically assert all three laws hold. For many standard typeclasses, libraries even provide pre-built test batches.
```haskell
-- Automatically tests Reflexivity, Symmetry, and Transitivity!
testBatch (eq (undefined :: MyData))
```

**The Minimal Implementations:**
- **0 Inhabitants (`Void`)**: As explored in Part 1 (Functions Returning an Uninhabited Type), any function computing a value from `Void` is mathematically valid via the Principle of Explosion. Since the signature becomes `(==) :: Void -> Void -> Bool`, substituting `Void` requires at least one `Void` as input. Using `absurd` (or the empty case), this gives us the *only* implementation possible:
  ```haskell
  instance Eq Void where
      v1 == _ = absurd v1
  ```
  And because we can never instantiate the values at runtime to break them, the property laws of `Eq` are trivially (vacuously) satisfied:
  * **Reflexivity**: $\forall v$, $v == v$? We can never provide any $v$, so the statement is vacuously true.
  * **Symmetry**: $\forall (v_1, v_2)$, $v_1 == v_2 \Rightarrow v_2 == v_1$? We can never provide $v_1$ or $v_2$, so yes.
  * **Transitivity**: $\forall (v_1, v_2, v_3)$, $v_1 == v_2 \land v_2 == v_3 \Rightarrow v_1 == v_3$? We can never provide $v_1$, $v_2$, or $v_3$, so yes.

  **Exercise 1: Point-Free `(==)`**
  How would you manually implement `(==) :: Void -> Void -> Bool` in a fully point-free style using only `absurd` and `const`?
  
  <details>
  <summary><b>View Solution</b></summary>
  
  `absurd` has the type `Void -> a`, which can be specialized to `Void -> Bool`.
  `const` takes a value and ignores its second argument, with type `x -> y -> x`.
  By passing `absurd` to `const`, we get a function `const absurd` with the signature `y -> (Void -> Bool)`. When used in our instance, `y` aligns with `Void`, giving `Void -> Void -> Bool`. Thus:
  ```haskell
  instance Eq Void where
      (==) = const absurd
  ```
  </details>

  **Exercise 2: The Derived `(/=)`**
  Since `Eq` has a minimal pragma prescribing either `(==)` or `(/=)`, Haskell will automatically derive `(/=)` from our `(==)` implementation. What is the effective full implementation of `Eq Void` that the compiler generates?

  <details>
  <summary><b>View Solution</b></summary>
  
  Haskell uses the default implementation `x /= y = not (x == y)`. Combined with our implementation of `(==)`, the full effective code becomes:
  ```haskell
  instance Eq Void where
      v1 == _ = absurd v1
      v1 /= v2 = not (absurd v1)
  ```
  </details>

- **1 Inhabitant (`()`)**: There is only one possible value, so `() == ()` is always `True`. This trivially respects all the laws.

  **Exercise 3: The Trivial Inequality**
  Without relying on `(==)`, how would you implement the simplest possible `(/=) :: () -> () -> Bool` directly?
  
  <details>
  <summary><b>View Solution</b></summary>
  
  Since both inputs must be `()`, they are always exactly the same value. Thus, they can never be not equal. The implementation is universally `False`:
  ```haskell
  instance Eq () where
      () /= () = False
  -- or simply:
  --  _ /= _ = False
  ```
  </details>

  **Exercise 4: Breaking the Unit Laws**
  Is it possible to write a mathematically invalid `Eq` instance for `()`? If so, what is it and which law does it break?

  <details>
  <summary><b>View Solution</b></summary>

  Yes! Although there's only one way to define equality that obeys the mathematical laws, Haskell still lets you write whatever code you want:
  ```haskell
  instance Eq () where
      () == () = False
  ```
  This immediately breaks **Reflexivity**, which strictly mandates that for all values $x$, $x == x$ must evaluate to `True`. Because our instance returns `False`, it is an unlawful, mathematically invalid `Eq`!
  </details>

- **2 Inhabitants (`Bool`)**: **Reflexivity** strictly forces our hand to define `True == True` and `False == False`. This leaves us with exactly two lawful possibilities for how we handle the cross-comparisons (`True == False`). Both of these configurations naturally fulfill the remaining laws (Symmetry and Transitivity), giving us two valid cases:
  1. **The Useful Case (`_ == _ = False` for differing values)**: By defining the cross-comparisons to evaluate to `False`, we preserve `True` and `False` as two completely distinct semantic concepts.
  2. **The Non-Useful Case (`_ == _ = True` for differing values)**: Mathematically, if we evaluate this to `True`, we construct a perfectly lawful equality where `True` and `False` belong to the exact same *equivalence class*. While `Bool` would still structurally have two distinct memory tags internally, this algebraically groups them together into a single *Quotient Type*. It mathematically collapses the concept of `Bool` into behaving like a 1-inhabitant type whenever evaluated through `(==)`. This is a perfectly correct and mathematically lawful instance, but it is hardly useful in any programming context since we would entirely lose the ability to differentiate branches!

  **Exercise 5: The Logic Gate**
  Focusing on the "useful" implementation of `Eq` for `Bool`, the `(==)` and `(/=)` operators are both functions of type `Bool -> Bool -> Bool`. If you were building a physical circuit board, what standard logic gates do these two operators correspond to?

  <details>
  <summary><b>View Solution</b></summary>
  
  * `(==)` outputs `True` only if both inputs are the same (both `True` or both `False`). This corresponds perfectly to an **XNOR (Exclusive-NOR)** gate (also called the logical biconditional).
  * `(/=)` outputs `True` only if the inputs differ (`True/False` or `False/True`). This corresponds perfectly to an **XOR (Exclusive-OR)** gate!
  </details>

- **$\infty$ Inhabitants (`Fraction`)**: When a type has many inhabitants, the default compiler-derived `Eq` (which checks identical memory structure) may not reflect true mathematical parity. We often have to manually implement structural equality.

  **Exercise 6: A Meaningful Custom `Eq`**
  Suppose you are working with fractions defined as a numerator and denominator:
  ```haskell
  data Fraction = Fraction Integer Integer
  ```
  If we let the compiler automatically derive an `Eq` instance for us, it would only check if the exact fields match. Under that default instance, `Fraction 1 2 == Fraction 2 4` would evaluate to `False`. How would you write a custom `Eq` instance that mathematically reflects truly equivalent fractions?

  <details>
  <summary><b>View Solution</b></summary>

  Two fractions $a/b$ and $c/d$ are equal if their cross-multiplication matches ($a \times d = b \times c$).
  ```haskell
  instance Eq Fraction where
      Fraction a b == Fraction c d = (a * d) == (b * c)
  ```
  By defining equality based on the mathematical properties of the values rather than their literal data layout, we ensure a robust equivalence relation that fully obeys Reflexivity, Symmetry, and Transitivity!
  </details>

### Section 1.2: `Ord` (The Laws of Total Ordering)

If `Eq` tells us if things are the same, `Ord` tells us how to line them up in a sequence. `Ord` provides operations like `compare`, `<=`, and `>`. 

```haskell
data Ordering = LT | EQ | GT

class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<)     :: a -> a -> Bool
    (<=)    :: a -> a -> Bool
    (>)     :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    max     :: a -> a -> a
    min     :: a -> a -> a
    {-# MINIMAL compare | (<=) #-}
```

Notice the `Ordering` type at the top of the block. This is a standard built-in Haskell enumeration with exactly three inhabitants: `LT` (Less Than), `EQ` (Equal To), and `GT` (Greater Than). It exists purely to represent the concrete result of evaluating which of two values comes first.

Just like with `Eq`, the `{-# MINIMAL compare | (<=) #-}` pragma dictates what we need to provide. To satisfy the compiler and get a full `Ord` instance with all seven functions, we only need to implement *either* the `compare` function *or* the `(<=)` operator. If we define `compare`, all other functions like `<`, `>`, and `max` are automatically derived from it.

It is a fundamental rule that any type with an `Ord` instance *must* also have an `Eq` instance.

Mathematically, `Ord` defines a **Total Order**. It inherits the rules of `Eq` and adds:

1. **Antisymmetry**: If `x <= y` and `y <= x`, then they must actually be the same value (`x == y`).
2. **Transitivity**: If `x <= y` and `y <= z`, then `x <= z`.
3. **Strong Connexity**: For any two values, one must be smaller than or equal to the other (`x <= y` or `y <= x`). In other words, every single value in the type can be compared to every other value without exception.

**The Minimal Implementations:**
- **0 Inhabitants (`Void`)**: Vacuously true.
- **1 Inhabitant (`()`)**: `()` is always equal to (and therefore `<=` to) `()`.
- **2 Inhabitants (`Bool`)**: `False` is canonically ordered before `True` (`False <= True`).

- **3 Inhabitants (`RPS`)**: Three inhabitants is the minimum number required to demonstrate a cyclic relationship, meaning we can mathematically break a Total Order!

  **Exercise 7: Breaking the Total Order**
  Consider a hypothetical game of Rock-Paper-Scissors. Can we mathematically construct a valid sequence of all choices? Let's try writing an `Ord` instance:
  ```haskell
  data RPS = Rock | Paper | Scissors deriving (Eq)

  instance Ord RPS where
      compare Rock Paper = LT     -- Rock loses to Paper  (Rock < Paper)
      compare Paper Scissors = LT -- Paper loses to Scissors (Paper < Scissors)
      compare Scissors Rock = LT  -- Scissors loses to Rock (Scissors < Rock)
      compare x y | x == y    = EQ
                  | otherwise = GT
  ```
  While this compiles and type-checks, which of the mathematical laws of a Total Order (`Ord`) does it violate?

  <details>
  <summary><b>View Solution</b></summary>

  It violates **Transitivity**!
  Our instance defines `Rock <= Paper` and `Paper <= Scissors`. By the mathematical rule of Transitivity, it would follow that `Rock <= Scissors` must be true.
  However, our code specifically defines `compare Scissors Rock = LT` (meaning `Scissors < Rock`, and thus `Rock > Scissors`), making `Rock <= Scissors` evaluate to `False`! Therefore, Rock-Paper-Scissors is a mathematical *cycle*, rendering it physically impossible to fulfill a sequence of Total Order!
  </details>

  **Exercise 8: Deriving the Rest from `compare`**
  Assume you have provided a valid `compare :: a -> a -> Ordering` for your type. How would you mathematically define the other operators (`<`, `<=`, `>`, `>=`, `max`, `min`) solely in terms of `compare`?

  <details>
  <summary><b>View Solution</b></summary>

  ```haskell
  x <  y = compare x y == LT
  x <= y = compare x y /= GT
  x >  y = compare x y == GT
  x >= y = compare x y /= LT

  max x y = case compare x y of
                LT -> y
                _  -> x

  min x y = case compare x y of
                GT -> y
                _  -> x
  ```
  This beautifully shows how the entirety of total ordering logic neatly cascades out of a single comparison query!
  </details>

Because we have firmly established how to compare and order concrete values, we can finally move on to *combining* them.

***

## Chapter 2: Associative Binary Operations ($+$ and $\times$)

### Section 2.1: `Semigroup`, `Monoid`, `Group`, and `Field`

While `Eq` and `Ord` define static relationships between existing values, abstract algebra structures (`Semigroup`, `Monoid`, `Group`, and `Field`) provide our fundamental toolkit for dynamically *combining, aggregating, and inverting* concrete values.

In standard GHC (`base`), the hierarchy natively defines `Semigroup` (`associative aggregation via <>`) and `Monoid` (`adding an identity element mempty`). However, in dedicated abstract algebra libraries (`such as the algebra package or numeric-prelude on Hackage`), the hierarchy extends smoothly up to **`Group`** (`adding an inverse operation invert`) and **`Field`** (`pairing two operations together: an additive group and a multiplicative monoid with division/reciprocals`):

```haskell
-- The abstract algebra class hierarchy (from packages like `algebra`):
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

class Monoid a => Group a where
  invert :: a -> a  -- e.g., negate in addition, or self-inverse (x /= y) in XOR

class (Group a, Monoid a) => Ring a where ...
class Ring a => Field a where ... -- Pairs additive Group with multiplicative inverse (/)
```

> [!NOTE]
> **The Category-Theoretic Dictionary (`One-Object Categories`)**
> In category theory, these foundational algebras share a profound, unified definition based on categories possessing **exactly one object (`•`)**:
> * **`Semigroup`**: A **Semicategory (`composition without requiring identity`) with exactly one object**. The arrows (`morphisms $f: • \to •$`) represent the elements of our semigroup, and arrow composition ($f \circ g$) represents our associative binary operation (`<>`)!
> * **`Monoid`**: A **Category with exactly one object**. By adding the category-theoretic identity arrow ($\text{id}: • \to •$), we gain our identity element (`mempty`)!
> * **`Group`**: A **Groupoid with exactly one object**! (`Or equivalent: a Category with 1 object where every morphism is an isomorphism / invertible`). Because a Groupoid requires every arrow $f$ to have a two-sided inverse $f^{-1}$ ($f \circ f^{-1} = \text{id}$), having exactly 1 object guarantees that every element in our Monoid possesses an inverse (`invert`)!

To form a lawful `Monoid`, `Group`, or `Field` in Haskell, a type must satisfy two primary conditions:

#### 1. A Well-Kinded Type (`Type`)
Unlike Functors which must be type constructors of kind `Type -> Type` (like `[]` or `Maybe`), an algebraic structure (`Monoid`, `Group`, or `Field`) must have kind `Type`. It operates on fully saturated, concrete value types like `[Int]`, `String`, or `Bool`. You cannot have a `Monoid` instance for a bare constructor like `Maybe`, only for a specific type like `Maybe Int`.

> [!WARNING]
> **What about function types?**
> It is a very common trap to look at a function type like `Integer -> Bool` and intuitively guess its kind is `Type -> Type` because of the single arrow. But this is an illusion of syntax!
> 
> The arrow operator `(->)` is actually an infix type constructor. It takes *two* concrete types to build a final concrete type. Thus, its base kind is `Type -> Type -> Type`.
> * `(->)` alone has kind `Type -> Type -> Type`
> * `(->) Integer` (partially applied) has kind `Type -> Type`
> * `(->) Integer Bool` (fully applied, normally written as `Integer -> Bool`) has kind `Type`.
> 
> Because `Integer -> Bool` is fully saturated and has kind `Type`, it operates as a concrete value type and perfectly qualifies to be a Monoid! In fact, Haskell automatically provides a Monoid instance for any function `a -> b` provided that the return type `b` is a Monoid.

> [!NOTE]
> However, the *concept* of a monoid absolutely exists for `Type -> Type`!
> In Haskell, the monoid for types of kind `Type -> Type` is captured by the `Alternative` typeclass (which we will talk about later). If you map the signatures conceptually, it's an exact match: `mempty` becomes `empty :: f a` and `<>` becomes `<|> :: f a -> f a -> f a`.

#### 2. Two Core Operations
A type is a monoid if it has two operations: a 0-ary identity and a 2-ary combination.
1. `mempty :: a`: An identity "empty" value.
2. `mappend :: a -> a -> a` (or `<>`): A binary associative operation to combine two values.

**The Monoid Laws and Testing**
Just like Functors and Applicatives, instances of `Monoid` must rigidly obey mathematical laws:
1. **Left Identity**: `mempty <> x == x`
2. **Right Identity**: `x <> mempty == x`
3. **Associativity**: `(x <> y) <> z == x <> (y <> z)`

> [!NOTE]
> **Wait, what about Commutativity?** 
> Notice that commutativity (`x <> y == y <> x`) is strictly **NOT** one of the Monoid laws! If a Monoid *happens* to also be commutative (like numeric `Sum` or `Product`), it is given a special name: an **Abelian Monoid**. However, the vast majority of useful structural monoids in programming are strictly non-commutative. For example, `List` (`"A" <> "B" /= "B" <> "A"`), `First` (keeps the first `Just` value), and `Endo` (function composition $f \circ g \neq g \circ f$) rigidly obey associativity but deliberately break commutativity!

**Developer Responsibility**: 
The Haskell compiler will perfectly compile a `Monoid` instance even if it violently breaks these laws! It is solely the developer's responsibility to ensure algebraic correctness. 

However, notice that these laws rely on strict `==` equality (unlike the natural Left/Right identities of Bifunctors from Chapter 1, which strictly relied on structural isomorphism $\cong$). Because they rely on simple equality, we can trivially automate their validation using `tasty-quickcheck` and `testBatch`:
```haskell
-- Automatically tests Associativity and Left/Right Identity!
testBatch (monoid (undefined :: All))
```What are the top minimal implementations of a `Semigroup` and `Monoid`? We can explore their behavior across exact inhabitant counts, starting right from zero!

#### 1. Types with 0 Inhabitants (`Void`: `Semigroup` without `Monoid`)
Can `Void` (`0 inhabitants`) form a `Semigroup` or `Monoid`?
* **Why `Void` IS a lawful `Semigroup`**: If you are asked to implement `(<>) :: Void -> Void -> Void`, if you are handed an `x :: Void`, because `x` is impossible to construct, you can immediately eliminate the branch using `absurd`:
  ```haskell
  instance Semigroup Void where
    x <> _ = x  -- or absurd x
  ```
  Does this satisfy the **Associativity Law** (`(x <> y) <> z == x <> (y <> z)`)?
  **Yes, vacuously!** Because no triple of values `(x, y, z)` of type `Void` can ever exist to produce a counterexample, the associativity law holds with 100% mathematical perfection! (`In fact, this exact instance lives right in GHC's base library inside Data.Void!`)
* **Why `Void` CANNOT form a `Monoid`**: To form a `Monoid`, you must provide a concrete value `mempty :: Void`. Because `Void` has **0 inhabitants**, it is physically and mathematically impossible to produce a `mempty :: Void` value (`without divergence or undefined`). Therefore, `Void` is a lawful `Semigroup` that can never be a `Monoid`!

#### 2. The Absolute Minimum for a `Monoid`, `Group`, and `Ring` (`1 Inhabitant: ()`)
**The Unit `()`**: Since `Void` cannot have a `mempty`, the smallest possible `Monoid` requires 1 inhabitant (`() <> () = ()` where `mempty = ()`). 
Because `()` is the only value in existence, it trivially fulfills every single law up through **Group** and **Ring**:
* **It forms the Trivial Group (`Zero Group`)**: What is the inverse of `()`? `invert () = ()`! The inverse law `() <> invert () == mempty` (`() == ()`) holds trivially!
* **It forms the Trivial Ring (`Zero Ring`)**: If we define both addition (`+`) and multiplication (`*`) as returning `()`, then our additive identity `0` is `()` and our multiplicative identity `1` is `()` ($0 = 1$).
* **Why `()` is NOT a `Field` (`The $0 \neq 1$ Axiom`)**: While `()` forms a Group and Ring, abstract algebra strictly mandates that **a Field must have at least 2 elements (`the additive identity $0$ and multiplicative identity $1$ must be distinct: $0 \neq 1$`)**. If $0 = 1$, division by zero (`0 / 0`) would become valid, destroying the uniqueness of prime fields. Therefore, `()` is our minimal Group/Ring, while `Bool` (`2 inhabitants: GF(2)`) is where the **Field** hierarchy begins!

#### 3. Types with 2 Inhabitants (`Bool`)
A type with exactly 2 values (like `Bool` with `True` and `False`) has $2 \times 2 = 4$ possible input combinations for a binary function. For each input, it must choose one of 2 outputs, yielding $2^4 = 16$ mathematically possible binary operations.

Here is the exhaustive list of all 16 possible logical operations for a Boolean type:
1. **Contradiction** (⊥): Always returns `False` (ignores both inputs).
2. **NOR** (↓): Returns `True` only if both are `False`.
3. **Converse Nonimplication** (↚): Returns `True` only if $B$ is True and $A$ is False.
4. **Negation A** (¬A): Always returns `Not A` (ignores the second argument).
5. **Material Nonimplication** (↛): Returns `True` only if $A$ is True and $B$ is False.
6. **Negation B** (¬B): Always returns `Not B` (ignores the first argument).
7. **XOR** (⊕): Returns `True` if inputs are different.
8. **NAND** (↑): Returns `False` only if both are `True`.
9. **AND** (∧): Returns `True` only if both are `True`.
10. **Equivalence** (↔): Returns `True` if inputs are the same.
11. **Projection B** (B): Always returns $B$ (ignores the first argument).
12. **Material Implication** (→): Returns `False` only if $A$ is True and $B$ is False.
13. **Projection A** (A): Always returns $A$ (ignores the second argument).
14. **Converse Implication** (←): Returns `False` only if $B$ is True and $A$ is False.
15. **OR** (∨): Returns `True` if at least one is `True`.
16. **Tautology** (⊤): Always returns `True` (ignores both inputs).

> [!NOTE]
> **Why we can focus purely on Isomorphism (`The 8 De Morgan Classes`)**
> When we check for isomorphisms between these 16 operations using Boolean Negation ($f(x) = \neg x$) as our bijection, we are checking for **De Morgan Duality** ($\diamond(x, y) == \neg(\neg x \circ \neg y)$).
> Under this negation bijection, the **16 operations immediately collapse into exactly 8 Isomorphism Classes**:
> 1. **Constants**: `Contradiction` ($\bot$) is isomorphic to `Tautology` ($\top$). (`2 ops $\to$ 1 class`)
> 2. **Lattice Monoids**: `AND` is isomorphic to `OR` (`De Morgan's Law`). (`2 ops $\to$ 1 class`)
> 3. **Cyclic Groups**: `XOR` is isomorphic to `XNOR` (`via negation $f = \text{not}$`). (`2 ops $\to$ 1 class`)
> 4. **Universal Gates**: `NAND` is isomorphic to `NOR`. (`2 ops $\to$ 1 class`)
> 5. **Implications**: The 4 implication variations partition into 2 dual pairs. (`4 ops $\to$ 2 classes`)
> 6. **Projections**: `Projection A` and `Projection B` (`and Negation A/B`) are each self-dual. (`2 ops $\to$ 2 classes`)
> 
> **How many survive when we enforce abstract algebra axioms up to isomorphism?**
> * **Enforcing Associativity (`Semigroups`)**: Exactly 3 isomorphism classes break associativity (`Universal Gates, Implications, Negated Projections`), leaving us with **5 Isomorphism Classes of Semigroups** (`Constants, Lattice Monoids, Cyclic Groups, Projection A, Projection B`).
> * **Enforcing Identity Element (`Monoids`)**: Exactly 3 classes fail (`Constants, Projection A, Projection B`), leaving us with **2 Isomorphism Classes of Monoids** (`The Lattice Monoid Class AND/OR` and `The Cyclic Group Class XOR/XNOR`).
> * **Enforcing Inverses (`Groups`)**: The Lattice Monoid class fails (`no inverse for absorbing zeros`), leaving us with **1 unique Group up to isomorphism** (`The Cyclic Group Class XOR/XNOR isomorphic to $\mathbb{Z}_2$` / $C_2$, matching the foundational theorem that there is exactly 1 group of prime order $p = 2$!)

Out of these 16 mathematically possible binary operations on a 2-inhabitant type (`or 8 isomorphism classes`), we can filter them down through an elegant, descending algebraic hierarchy: **16 Operations $\to$ 8 Semigroups $\to$ 4 Monoids $\to$ 2 Groups $\to$ 1 Field (`and up to isomorphism: 8 Classes $\to$ 5 Semigroups $\to$ 2 Monoids $\to$ 1 Group`)!**

#### A. The 8 Lawful Semigroups (`Associative Operations`)
To be a valid `Semigroup`, an operation must strictly satisfy **Associativity**: $(x \circ y) \circ z == x \circ (y \circ z)$.
Exactly **8 operations break associativity** (`NOR, NAND, Material Implication, Converse Implication, Material Nonimplication, Converse Nonimplication, Negation A, and Negation B`), leaving us with **8 lawful Semigroups**:
1. **Projection A (`First / Left`)**: Always returns $A$. (`Semigroup without identity`)
2. **Projection B (`Second / Right`)**: Always returns $B$. (`Semigroup without identity`)
3. **Contradiction ($\bot$)**: Always returns `False`. (`Semigroup without identity`)
4. **Tautology ($\top$)**: Always returns `True`. (`Semigroup without identity`)
5. **AND ($\land$ / `All`)**: Associative (`Semigroup`)
6. **OR ($\lor$ / `Any`)**: Associative (`Semigroup`)
7. **XOR ($\oplus$)**: Associative (`Semigroup`)
8. **XNOR / Equivalence ($\leftrightarrow$)**: Associative (`Semigroup`)

#### B. The 4 Boolean Monoids (`Adding an Identity Element`)
To upgrade a `Semigroup` into a `Monoid`, the operation must possess a two-sided **Identity Element** ($e \circ x == x$ and $x \circ e == x$).
The first 4 semigroups (`Projections and Constants`) fail to have an identity, leaving exactly **4 lawful Monoids**:
*   **Boolean `All` (AND)** (`&&`): Identity $e = \text{True}$
*   **Boolean `Any` (OR)** (`||`): Identity $e = \text{False}$
*   **Boolean Exclusive OR (XOR)** (`/=`): Identity $e = \text{False}$
*   **Boolean Equivalence (XNOR)** (`==`): Identity $e = \text{True}$

*(Notice that at parameter size 2, any two-sided identity automatically guarantees associativity — see **Annex A**).*

#### C. The 2 Boolean Groups (`Adding Inverses: XOR and XNOR`)
To step up in power from a `Monoid` to a **Group**, every single element $x$ in the type must possess an **Inverse element** ($\text{inv}(x)$) such that $x \circ \text{inv}(x) == e$.

Testing our 4 Monoids reveals that exactly **2 of them fail**, while **2 of them upgrade to Groups**:
1.  **Why `AND` (`All`) is NOT a Group**: The identity is `True`. While $\text{inv}(\text{True}) = \text{True}$, what is the inverse of `False`? We need an element $y$ such that `False && y == True`. But `False && y` is always `False`! Once you hit `False`, you can never get back to the identity (`False is a destructive absorbing zero`).
2.  **Why `OR` (`Any`) is NOT a Group**: The identity is `False`. By exact symmetry, `True` is an absorbing zero with no inverse (`True || y` is always `True`).
3.  **Why `XOR` ($\oplus$) IS a Group**: The identity is `False`. `False ` $\oplus$ ` False == False` (`inv(False) = False`), and `True ` $\oplus$ ` True == False` (`inv(True) = True`). Every element is its own self-inverse! This forms the cyclic group $\mathbb{Z}_2$ under addition modulo 2.
4.  **Why `XNOR` ($\leftrightarrow$) IS a Group**: The identity is `True`. `True ` $\leftrightarrow$ ` True == True` (`inv(True) = True`), and `False ` $\leftrightarrow$ ` False == True` (`inv(False) = False`). Every element is its own self-inverse! This forms the isomorphic group $\mathbb{Z}_2$ under sign multiplication $\{+1, -1\}$.

#### D. The Boolean Rings & Fields (`Up to Isomorphism: 1 Group Class $\times$ 1 Monoid Class = 1 Field Class!`)
To step up from a `Group` to a **Ring** (and **Field**), we must pair **two distinct binary operations** together (`an additive group + and a multiplicative monoid *`) satisfying the **Distributive Law** ($x * (y + z) == (x * y) + (x * z)$) and **Absorption** ($0 * x == 0$).

Since there are 16 binary operations (`or 8 isomorphism classes`), there are $16 \times 16 = \mathbf{256 \text{ physical pairs}}$ (`or $8 \times 8 = 64 \text{ class pairs}$`). Look how effortless the filtration becomes when working **up to Isomorphism (`De Morgan Duality`)**:
1. **The Additive Group ($+$)**: Must be an Abelian Group. From Subsection C, there is exactly **1 Group Class up to isomorphism** (`The Cyclic Group Class: XOR / XNOR`).
2. **The Multiplicative Monoid ($*$)**: Must be a Monoid that distributes over our Group Class without degenerating to $0 = 1$. From Subsection B, the only distinct class is **The Lattice Monoid Class (`AND / OR`)**!

This immediately narrows our search down to exactly **1 unique pair of classes up to isomorphism: `(Cyclic Group Class, Lattice Monoid Class)`**!

When we instantiate those two classes into physical operations on `Bool`:
* `XOR` ($0 = \text{False}$) pairs with its exact counterpart **`AND` ($\land$)** (`where $0$ absorbs False`) $\to$ **`(+ = XOR, * = AND)`**!
* `XNOR` ($0 = \text{True}$) pairs with its exact counterpart **`OR` ($\lor$)** (`where $0$ absorbs True`) $\to$ **`(+ = XNOR, * = OR)`**!

Out of 256 physical pairs, up to isomorphism there is exactly **1 unique Boolean Ring / Galois Field ($\mathbb{F}_2$)**, represented physically by those two De Morgan dual pairs!

> [!NOTE]
> **Why did `(+ = XOR, * = AND)` become our universal standard over `(+ = XNOR, * = OR)`?**
> While both pairs are mathematically isomorphic, `(+ = XOR, * = AND)` maps directly to **physical binary arithmetic** ($0 = \text{False}, 1 = \text{True}$):
> * **`XOR` is literally 1-bit Addition (Sum Bit)**: In binary, $0+0=0$, $0+1=1$, $1+0=1$, and $1+1=10_2$ (`Sum bit: 0`, plus a Carry out of $1$). Notice that `XOR` computes the exact Sum bit of 1-bit addition (`addition modulo 2`)!
> * **`AND` is literally 1-bit Multiplication (Carry Bit)**: In binary, $0 \times 0 = 0$, $0 \times 1 = 0$, $1 \times 0 = 0$, and $1 \times 1 = 1$. Notice that `AND` computes exact 1-bit multiplication (`and the Carry bit of an adder`)!
> Because electrical engineers universally map `0 Volts (Low)` to `0/False` and `5 Volts (High)` to `1/True`, `(+ = XOR, * = AND)` allows half-adder circuits and CPU ALUs to perform exact binary arithmetic directly using `XOR` and `AND` logic gates!

#### 4. Types with 3 Inhabitants (e.g., `Ordering`)
What happens when we jump to a type with exactly 3 values (like `LT`, `EQ`, `GT`)? We witness a massive combinatorial explosion, but it is still small enough to mathematically map out across our complete algebraic hierarchy: **19,683 Operations $\to$ 113 Semigroups $\to$ 33 Monoids $\to$ 3 Groups, and 387.4M Pairs $\to$ 9 Rings $\to$ 6 Fields!**

1. **Total Binary Operations**: A binary function takes two arguments, so there are $3 \times 3 = 9$ possible input combinations `(x, y)`. For each of those 9 inputs, the function must choose one of 3 outputs. This yields $3^9 = \mathbf{19,683}$ mathematically possible binary operations!
2. **Lawful Semigroups (`Associative Operations`)**: Out of those 19,683 total operations, how many satisfy the Law of Associativity `(x <> y) <> z == x <> (y <> z)`? By rigorous combinatorial enumeration (`OEIS A001423`), exactly **113 operations are associative** and form lawful `Semigroups` (`while 19,570 operations break associativity`)!
3. **Lawful Monoids (`Adding an Identity Element`)**: Out of those 113 Semigroups, how many also possess a valid two-sided Identity Element (`mempty`)? Exactly **33 of them survive** to form lawful `Monoids` (`11 monoids for each of our 3 identity choices: LT, EQ, or GT`)!
4. **Lawful Groups (`Adding Inverses`)**: Out of those 33 Monoids, how many step up to become `Groups` by providing an inverse for every element? Exactly **3 of them form lawful Groups**! Specifically, these are the 3 isomorphic variations of the cyclic group $\mathbb{Z}_3$ (`addition modulo 3`), corresponding to whether `LT`, `EQ`, or `GT` is selected as our additive identity $0$!
5. **Lawful Rings & Fields (`Filtering 387,420,489 Pairs`)**: To form a Ring or Field, we must pair two distinct operations $(+, *)$ out of our $19,683 \times 19,683 = \mathbf{387,420,489 \text{ total candidate pairs}}$!
   * **Group/Monoid Filter**: Our first operation $(+)$ must be one of our **3 Groups** (`the $\mathbb{Z}_3$ variations`). Our second $(*)$ must be one of our **33 Monoids**. This instantly drops ~387.4 million pairs down to at most $3 \times 33 = \mathbf{99 \text{ pairs}}$!
   * **Distributivity & Absorption Filter**: Because $n = 3$ is a prime number, abstract algebra proves that the only non-degenerate ring structure on $\mathbb{Z}_3$ is the **Galois Field $\text{GF}(3)$ / $\mathbb{F}_3$ (`arithmetic modulo 3`)**! Since we have **3 choices** for our additive identity $0$, and for each choice of $0$ we have **2 remaining choices** for our multiplicative identity $1$ ($1 \neq 0$), the laws of distributivity and absorption ($0 * x == 0$) uniquely lock in exactly $3 \times 2 = \mathbf{6 \text{ pairs of operations that form lawful Fields (`GF(3)`)!}}$ (`And including the 3 degenerate Zero Rings where $1 = 0$, exactly 9 pairs out of 387.4 million form Rings!`)
   * **The 6 Exact Fields on `Ordering` (`The 3! Bijections`)**:
     In abstract algebra, the Galois Field $\text{GF}(3)$ (`arithmetic modulo 3 on {0, 1, 2}`) requires an additive zero $0$, a multiplicative identity $1$, and a third element $2$ (`which acts as $-1$ since $2 + 1 \equiv 0$, and satisfies $2 \times 2 \equiv 1$, making $2$ its own self-inverse`). Because there are $3! = 6$ distinct ways to assign the mathematical roles $(0, 1, 2)$ to our 3 concrete values `(LT, EQ, GT)`, each bijection uniquely defines one of our 6 Fields:
     1. **`0 = EQ, 1 = GT, 2 = LT` (`The Natural Sign-Algebra Standard`)**: `EQ` acts as $0$, `GT` as $+1$, and `LT` as $-1$. (`Here, LT * LT = GT`, matching $(-1) \times (-1) = +1$).
     2. **`0 = EQ, 1 = LT, 2 = GT`**: Shares $0 = \text{EQ}$, but swaps the $+1$ and $-1$ roles of `LT` and `GT`.
     3. **`0 = LT, 1 = EQ, 2 = GT`**: `LT` acts as additive $0$, `EQ` as $1$, and `GT` as $2$.
     4. **`0 = LT, 1 = GT, 2 = EQ`**: Shares $0 = \text{LT}$, but swaps the multiplicative roles of `EQ` and `GT`.
     5. **`0 = GT, 1 = LT, 2 = EQ`**: `GT` acts as additive $0$, `LT` as $1$, and `EQ` as $2$.
     6. **`0 = GT, 1 = EQ, 2 = LT`**: Shares $0 = \text{GT}$, but swaps `EQ` and `LT`.

> [!TIP]
> **The Loss of the Mathematical Freebie**
> Notice a fascinating anomaly here! In the 2-inhabitant (`Bool`) case, every single one of the 4 operations that possessed an identity *automatically* passed associativity. You get associativity completely "for free". 
> 
> However, the moment you jump up to 3 inhabitants, this mathematical freebie violently vanishes. Out of the 243 operations that possessed a perfect identity element (`Unital Magmas`), a massive **210 operations** (243 - 33) had to be discarded *specifically* because they broke the Law of Association!

Therefore, for a type with 3 values (`like Ordering`), out of 19,683 single operations exactly **113 are Semigroups**, **33 are Monoids**, and **3 are Groups**—and out of 387,420,489 operation pairs, exactly **9 form Rings** and **6 form Fields (`GF(3)`)**!

#### 5. Types with Countably Infinite Inhabitants (e.g., `Integer`)
What if the type has an infinite number of values? In this case, there are an **infinite** number of valid Monoids. 
For example, for standard numeric types (`Integer`), you trivially have `Sum` ($0, \mathbf{+}$) and `Product` ($1, \mathbf{\times}$), but also `Max` ($-\infty, \max$) and `Min` ($\infty, \min$), along with infinite logical bitwise operations like `And` and `Xor`. 

#### 6. The Free Monoid (`[a]`)
An incredibly special case of a countably infinite type is the List (`[a]`). Lists form what mathematicians term the **Free Monoid** over a set `a`. A "Free" object in algebra is one that satisfies the minimal laws required, and absolutely nothing else.

By concatenating elements end-to-end (`++`), lists perfectly obey the Monoid laws:
* `[] ++ xs = xs` (Left Identity)
* `xs ++ [] = xs` (Right Identity)
* `(as ++ bs) ++ cs = as ++ (bs ++ cs)` (Associativity)

The List monoid does no "computation" or "squashing" like `Sum` or `Product` do; it purely memorizes the order and elements over time. Because it is the "purest" monoid with no additional baggage, you can map any List into *any other valid Monoid* using `foldMap`:
```haskell
-- The Free Monoid perfectly translates into any other Monoid:
sumList xs = getSum (foldMap Sum xs)
mulList xs = getProduct (foldMap Product xs)
```
In fact, `Foldable` is entirely defined by a data structure's ability to be collapsed down into this Free Monoid!

#### 6. Why do `Sum`, `Product`, `Max`, and `Min` stand out?
You might notice that while there are infinitely many ways to combine integers, we almost always reach for these four. What makes them "atomic"?

Just as Functors can be built from "atoms" (Identity, Constant, Either, Pair) using composition, these Monoids are the **natural algebraic projections** of underlying structures:

1.  **Additive/Multiplicative Monoids**: These are derived from the fact that `Integer` is a **Semiring**. A Semiring is a type with two monoidal operations that interact via the Distributive Law ($a \times (b + c) = a \times b + a \times c$).
2.  **Max/Min Monoids**: These are derived from the fact that `Integer` is a **Bounded Lattice**. Any type with a total ordering (`Ord`) can form a Monoid using the "least upper bound" (`max`) or "greatest lower bound" (`min`).

In this sense, these monoids aren't arbitrary; they are the **unique** ways to satisfy the Monoid laws while preserving the deeper algebraic relationships (like distribution or ordering) already present in the type. 

**Is Parametricity Helping Here?**
Unlike Functors (`Type -> Type`), which are parameterized over *any* type, Monoids operate on concrete types (`Type`). This means parametricity *does not* force a single, unique implementation. For example, the type `Double` could form a monoid under addition (`0` and `+`) or under multiplication (`1` and `*`). Haskell uses `newtype` wrappers like `Sum` and `Product` to explicitly choose the monoidal behavior.

#### Category Theory Origin: The Single-Object Categories
In Category Theory, these concrete algebraic structures share a profound, unified origin: they are rigorously defined as categories possessing **exactly one object (`•`)**!

If a category only has a single object (`•`), what do the morphisms (`arrows $f: • \to •$`) represent? Because there are no other objects to point to, every single morphism must be an endomorphism (`an arrow pointing from the object back to itself`). This yields a breathtaking, unified dictionary:

* **`Semigroup` (`Semicategory with 1 Object`)**: A **Semicategory** (`composition without requiring identity`) with exactly 1 object (`•`). The arrows (`morphisms $f: • \to •$`) represent our elements, and arrow composition ($f \circ g$) represents our associative binary operation `(<>)`!
* **`Monoid` (`Category with 1 Object`)**: A **Category** (`composition with identity`) with exactly 1 object (`•`). By adding the category-theoretic identity arrow ($\text{id}: • \to •$), we gain our identity element (`mempty`)!
* **`Group` (`Groupoid with 1 Object`)**: A **Groupoid** (`a Category where every arrow is an isomorphism / invertible`) with exactly 1 object (`•`)! Because a Groupoid mandates that every arrow $f$ possesses a two-sided inverse $f^{-1}$ ($f \circ f^{-1} = \text{id}$), having exactly 1 object guarantees that every element in our Monoid possesses an inverse (`invert`)!

Therefore, every time you combine, aggregate, or invert concrete values in Haskell, you are musically and mathematically composing arrows on a single, invisible categorical object!



