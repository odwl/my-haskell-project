# The Minimal Haskell Series: Part 3 - The Monoids

## Chapter 6: Other Minimals

### Section 6.1: Minimal Monoid

While functors and applicatives define the shape of computations, *Monoids* give us a fundamental way to aggregate concrete values. To be a valid `Monoid` in Haskell, a type must satisfy two main conditions:

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



