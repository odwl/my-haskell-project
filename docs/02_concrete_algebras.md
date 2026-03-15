# Part 2: The Algebras (Laws) for Concrete Types

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

- **2 Inhabitants (`Bool`)**: **Reflexivity** strictly forces `True == True` and `False == False`. But what about `True == False`? Mathematically, if we evaluated it to `True`, we'd be constructing a perfectly lawful equality where `True` and `False` are considered the exact same mathematical value—like synonymous words! To preserve them as distinct concepts, we manually define the cross-comparisons as `False`. The remaining laws (Symmetry and Transitivity) are then trivially fulfilled.

  **Exercise 5: A Meaningful Custom `Eq`**
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

  **Exercise 6: Breaking the Total Order**
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

  **Exercise 7: Deriving the Rest from `compare`**
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

### Section 2.1: `Semigroup` and `Monoid`

While `Eq` and `Ord` define relationships between static values, `Semigroup` and `Monoid` give us a fundamental way to dynamically *aggregate* concrete values together. To be a valid `Monoid` in Haskell, a type must satisfy two main conditions:

#### 1. A Well-Kinded Type (`Type`)
Unlike Functors which must be type constructors of kind `Type -> Type` (like `[]` or `Maybe`), a Monoid must have kind `Type`. It operates on fully saturated, concrete value types like `[Int]`, `String`, or `Sum Double`. You cannot have a `Monoid` instance for a bare constructor like `Maybe`, only for a specific type like `Maybe Int`.

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
```

What are the top minimal implementations of a Monoid? Of course, because we mathematically require an identity value, a monoid cannot be `Void` (a type with 0 inhabitants).

#### 1. The Absolute Minimum (1 Inhabitant)
**The Unit `()`**: There is only one value, so `mempty = ()` and `() <> () = ()`. This is the absolute minimum implementation of a Monoid, and the identity and associativity laws are trivially fulfilled because `()` is the only possible value.

#### 2. Types with 2 Inhabitants (`Bool`)
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

In fact, any 2-inhabitant operation that possesses a valid two-sided identity is mathematically *guaranteed* to be associative! (See the mathematical proof of this anomaly in **Annex A**).

To find our Monoids, we can mathematically filter these down by rigorously testing the identity laws!

**1. Which ones fail the Left Identity requirement? ($e \diamond x = x$)**
An operation must have some constant $e$ (`True` or `False`) that leaves the right side $x$ unchanged. 
Exactly **9 operations utterly fail** to have a left identity. These include the ones that ignore the right argument (Contradiction, Tautology, Projection A, Negation A), as well as NOR, NAND, Negation B, Material Nonimplication (↛), and Converse Implication (←).
Discarding those 9 leaves us with exactly 7 operations possessing a valid left identity.

**2. Which ones fail the Right Identity requirement? ($x \diamond e = x$)**
Of the 7 surviving operations, 3 of them fail to have a corresponding right identity element:
*   **Projection B** (B): Has a left identity but evaluation always yields $e \neq x$ on the right.
*   **Material Implication** (→): `T → x = x` (Left Identity is `T`), but `x → T = True` (Fails Right Identity).
*   **Converse Nonimplication** (↚): `F ↚ x = x` (Left Identity is `F`), but `x ↚ F = False` (Fails Right Identity).

Discarding those 3 leaves us with exactly 4 operations that possess a complete, **two-sided** identity element. At parameter size 2, proving that these surviving 4 operations also satisfy the final Monoid Law (Associativity) becomes trivial.

These remaining 4 operations perfectly form our 4 Boolean Monoids:
*   **Boolean `All` (AND)** (`&&`)
*   **Boolean `Any` (OR)** (`||`)
*   **Boolean Equivalence (XNOR)** (`==`)
*   **Boolean Exclusive OR (XOR)** (`/=`)

#### 3. Types with 3 Inhabitants (e.g., `Ordering`)
What happens when we jump to a type with exactly 3 values (like `LT`, `EQ`, `GT`)? We witness a massive combinatorial explosion, but it is still small enough to mathematically map out!

1. **Total Binary Operations**: A binary function takes two arguments, so there are $3 \times 3 = 9$ possible input combinations `(x, y)`. For each of those 9 inputs, the function must choose one of 3 outputs. This yields $3^9 = \mathbf{19,683}$ mathematically possible binary operations!
2. **Operations with an Identity**: To be a Monoid, we must possess an identity element. We have 3 choices for our identity (let's pick `EQ`). By setting `EQ` as the identity, we instantly lock in the required answers for 5 of our 9 input pairs (e.g., `(LT, EQ) -> LT`). This leaves only 4 remaining input pairs where we still have the freedom to choose any of the 3 outputs. Therefore, there are exactly $3^4 = 81$ operations where `EQ` is the identity. Since any of the 3 elements could have been chosen, there are exactly $3 \times 81 = \mathbf{243}$ total operations that possess a valid Identity Element (these are known mathematically as *Unital Magmas*).
3. **Operations that are Associative**: Out of those 243 Unital Magmas, we must filter out any that break the Law of Associativity `(x <> y) <> z == x <> (y <> z)`. If we explicitly calculate this for all 27 possible combinations of `(x, y, z)`, the math reveals that exactly **33** of them survive.

> [!TIP]
> **The Loss of the Mathematical Freebie**
> Notice a fascinating anomaly here! In the 2-inhabitant (`Bool`) case, every single one of the 4 operations that possessed an identity *automatically* passed associativity. You get associativity completely "for free". 
> 
> However, the moment you jump up to 3 inhabitants, this mathematical freebie violently vanishes. Out of the 243 operations that possessed a perfect identity element, a massive **210 operations** (243 - 33) had to be discarded *specifically* because they broke the Law of Association!

Therefore, for a type with 3 values, out of 19,683 possible operations, exactly **33 form perfectly valid Monoids**!

#### 4. Types with Countably Infinite Inhabitants (e.g., `Integer`)
What if the type has an infinite number of values? In this case, there are an **infinite** number of valid Monoids. 
For example, for standard numeric types (`Integer`), you trivially have `Sum` ($0, \mathbf{+}$) and `Product` ($1, \mathbf{\times}$), but also `Max` ($-\infty, \max$) and `Min` ($\infty, \min$), along with infinite logical bitwise operations like `And` and `Xor`. 

#### 5. The Free Monoid (`[a]`)
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

#### Category Theory Origin: The Single-Object Category
In Category Theory, a Monoid is rigorously defined as a **Category with exactly one object**.

If a category only has a single object, what do the morphisms (the arrows) represent? Because there are no other objects to point to, every single morphism must be an endomorphism (an arrow pointing from the object back to itself). 

This yields a profound translation:
1. **The Identity Value (`mempty`)**: Every category must have a mandated identity morphism for each object. In our single-object category, this single identity morphism geometrically represents `mempty`!
2. **The Binary Operation (`<>`)**: Every category must allow the composition of morphisms ($\circ$). Because all our morphisms start and end at the exact same single object, *any* two morphisms can be freely composed with each other. This categorical composition perfectly corresponds to `mappend`!
3. **The Laws**: The fundamental categorical laws of Morphism Identity and Morphism Associativity directly translate into our Monoid laws!

Therefore, every time you combine two concrete values using `<>`, you are musically and mathematically composing two structural morphisms on a single, invisible categorical object.



