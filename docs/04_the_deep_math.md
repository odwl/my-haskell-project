# The Minimal Haskell Series: Part 4 - The Deep Math

## Chapter 4: Deep Dive into Bifunctors

### Section 4.1: The True Nature of Bifunctors
In Category Theory, a **Bifunctor** is not actually a special new structure; it is quite literally just a standard Functor whose domain happens to be a **Product Category**. 

If you have two categories, $\mathcal{C}$ and $\mathcal{D}$, you can create a Product Category $\mathcal{C} \times \mathcal{D}$. The objects in this category are pairs of objects $(c, d)$, and the morphisms are pairs of morphisms $(f, g)$. A Bifunctor is simply a normal Functor $F$ that maps from that Product Category into a third category $\mathcal{E}$:

$$F: \mathcal{C} \times \mathcal{D} \to \mathcal{E}$$

In Haskell, everything happens in the single category `Hask`. So, a Haskell `Bifunctor` is just a standard functor mapping from the product category to the base category:

$$F: \mathbf{Hask} \times \mathbf{Hask} \to \mathbf{Hask}$$

Because it's just a normal Functor from a Product Category, the mapping operation (`bimap`) takes a pair of morphisms (which in Haskell means two functions: `(a -> c)` and `(b -> d)`) and applies them to the pair of objects inside the structure.
### Section 4.2: The Laws of Bifunctors

When you implement an `instance Bifunctor` in Haskell, you must satisfy laws analogous to the standard Functor laws, just extended over two dimensions. Since a Bifunctor is just a functor from a Product Category, the morphisms we are mapping are pairs of functions. 

The two laws are:

1. **Identity Law**:
   ```haskell
   bimap id id == id
   ```
   *Meaning*: If you apply the identity function to both the left and right sides simultaneously, the structure and its contents must remain completely unchanged.

2. **Composition Law**:
   ```haskell
   bimap (f . g) (h . i) == bimap f h . bimap g i
   ```
   *Meaning*: Composing two functions and then mapping them over a Bifunctor is identical to mapping the first pair of functions, and then mapping the second pair of functions over the result.

**The `first` and `second` Equivalences**
The `Data.Bifunctor` typeclass in Haskell also provides the helper functions `first` and `second` to map over only one side of the Bifunctor. The definition of a Bifunctor inextricably links `bimap`, `first`, and `second` through these properties:
*   `bimap f g == first f . second g`
*   `first f == bimap f id`
*   `second g == bimap id g`

This reinforces the concept described in [Section 1.4](#section-13-the-algebra-of-functors-bifunctors): if you fix the identity function to one side of a Bifunctor, it mathematically collapses into a standard Endofunctor.

## Chapter 5: Monoidal Categories

### Section 5.1: The Pentagon and Triangle Laws

A **Monoidal Category** is a higher-level structure that uses a specific Bifunctor as a "tensor product". 

It is defined by:
1. A base category (like `Hask`).
2. A specific **Bifunctor** acting as the tensor product (like `(,)` or `Either`).
3. A unit object (like `()` for products, or `Void` for sums).
4. Associativity and Unit natural isomorphisms.
5. **The Coherence Conditions**: This is where the **pentagon identity** (ensuring associativity associates consistently) and the **triangle identity** (ensuring the unit behaves consistently) come into play.

While `Either` and `(,)` are Bifunctors, they are *special* Bifunctors because they act as the tensor products that turn `Hask` into a Monoidal Category. Other Bifunctors (like `BiProxy` or `ConstContext r`) are perfectly valid Bifunctors without forming a monoidal category with strict pentagon/triangle laws.

*(This section is a placeholder for a future deep-dive into the formal definitions of tensor products, and how they interact structurally via the pentagon and triangle identities).*

---

### Parametricity in Category Theory

Yes, there is a very deep and well-established categorical notion of Haskell's parametricity. In fact, category theory provides the exact mathematical language needed to formalize what Philip Wadler famously called "Theorems for Free!"

Depending on how deep you want to go, parametricity can be understood categorically in a few distinct layers, ranging from natural transformations up to relational fibrations. Here is a breakdown of how category theory models parametric polymorphism.

#### 1. Natural Transformations (The Basic View)
At the simplest level, if a type variable only appears in covariant positions (like the output of a function or inside a standard data structure), parametricity corresponds exactly to natural transformations.

Suppose you have a polymorphic function in Haskell:
```haskell
f :: forall a. [a] -> Maybe a
```
Categorically, `[]` (List) and `Maybe` are functors from the category of Haskell types ($\mathbf{Hask}$) to itself. The polymorphic function `f` is a natural transformation $\eta : \text{List} \to \text{Maybe}$.

The defining property of a natural transformation is that for any function $h : A \to B$, the following square commutes:
$ \eta_B \circ \text{List}(h) = \text{Maybe}(h) \circ \eta_A $

In Haskell syntax, this is exactly the "free theorem" for `f`:
```haskell
f . fmap h == fmap h . f
```
Because the function is parametrically polymorphic, it must be a natural transformation, meaning it cannot inspect the elements it is moving around.

#### 2. Ends and Coends (Universal and Existential Types)
When type variables appear in both contravariant (input) and covariant (output) positions, we need a stronger categorical concept to represent the `forall` keyword. This is where **Ends** come in.

Consider the identity function type:
```haskell
id :: forall a. a -> a
```
Here, `a` is an argument to the function constructor `(->)`, which is a functor that is contravariant in its first argument and covariant in its second. So, `(->)` is a functor $H : \mathbf{Hask}^{op} \times \mathbf{Hask} \to \mathbf{Hask}$.

The `forall` quantifier is interpreted as a categorical end over this mixed-variance functor:
$$ \int_{A \in \mathbf{Hask}} H(A, A) $$
An end conceptually "takes the intersection" over all objects $A$, giving you the family of morphisms that act uniformly across all types. (Conversely, existential types like `exists a. ...` are modeled by coends, $\int^{A} H(A, A)$).

#### 3. Dinatural Transformations (Mixed Variance)
The "elements" (or points) of the end $\int_A H(A, A)$ are called **dinatural transformations**.

Standard natural transformations only work between functors of the same variance. Because `id` maps from a contravariant position to a covariant position, its uniformity is expressed as a dinatural transformation. The dinaturality hexagon (the commuting diagram for dinatural transformations) gives you the exact free theorem for functions with mixed-variance type signatures.

#### 4. Reflexive Graphs and Relational Fibrations (The Deep View)
While naturality and ends describe how polymorphic functions behave structurally, John C. Reynolds' original abstraction theorem (**Relational Parametricity**) states that polymorphic functions must preserve relations, not just functions.

To model this categorically, we have to move beyond just looking at the category of types and functions. We use a structure often called a **Reflexive Graph Category** or a **Relational Fibration**.

1.  We construct a base category $\mathbb{B}$ where objects are types and morphisms are functions.
2.  We construct a total category $\mathbb{E}$ where objects are relations between types, and morphisms are pairs of functions that preserve those relations.
3.  There is a functor $p : \mathbb{E} \to \mathbb{B} \times \mathbb{B}$ that projects a relation down to the two types it relates.

In this setting, a type operator (like List) isn't just a functor; it must be a functor that lifts to relations (e.g., if you have a relation $R$ between $A$ and $B$, you automatically get a relation $\text{List}(R)$ between $\text{List}(A)$ and $\text{List}(B)$). A parametrically polymorphic function is then an object in this higher category that intrinsically preserves all relations, fulfilling Reynolds' exact definition.

---

### Summary and Type Bundle Taxonomy
At this level, Functors are entirely about **Shape and Preservation**. Whether we are dealing with an empty box (`Proxy`), a wrapper (`Identity`), or an infinite chain (`List`), `fmap` ensures that the structure of the data remains physically identical while the values inside are transformed.

Before moving to Applicatives, remember the three tools Haskell gives us to bundle these shapes:

1.  **`type` (Alias)**: No new type created, zero overhead. Use for readability.
2.  **`newtype` (Strict Wrapper)**: Distinct type, zero overhead. Use for type safety (e.g., `UserId`).
3.  **`data` (Full ADT)**: Flexible, supports multiple constructors. Use for complex shapes.

---

## Annex A: Proof of 2-Inhabitant Associativity
*Proof that all 2-inhabitant logical operations possessing a valid two-sided identity element are automatically associative.*

Assume we have a 2-element type `{E, X}`. 
We declare that `E` is the identity element. Because `E` is the identity, the rules `E ⋄ a = a` and `a ⋄ E = a` instantly lock in 3 of the 4 slots in our logical multiplication table (Cayley Table):

| `x \ y` | E | X |
| :---: | :---: | :---: |
| **E** | `E` | `X` |
| **X** | `X` | **???** |

Because this table represents a closed binary operation on a 2-inhabitant type, there are only two possible values we can pick for the single empty box (`X ⋄ X`):
1. **Case 1**: `X ⋄ X = E` (This forms the XOR or Equivalence gate)
2. **Case 2**: `X ⋄ X = X` (This forms the OR or AND gate)

Now, we must test the Law of Associativity: `(a ⋄ b) ⋄ c == a ⋄ (b ⋄ c)`. 

*   **Trivial Cases**: If any of `a`, `b`, or `c` is the identity `E`, the associativity law trivially holds without evaluation. (For example, if `a = E`, then `(E ⋄ b) ⋄ c = b ⋄ c`, and `E ⋄ (b ⋄ c) = b ⋄ c`).
*   **The Single Non-Trivial Case**: The only case where none of the operands are the identity is when `a`, `b`, and `c` are all `X`. Thus, the entire proof hinges on whether `(X ⋄ X) ⋄ X == X ⋄ (X ⋄ X)`.

Let us evaluate this final equation for both possible remaining tables:

**Case 1 (where `X ⋄ X = E`)**: 
* Left side: `(X ⋄ X) ⋄ X = E ⋄ X = X`
* Right side: `X ⋄ (X ⋄ X) = X ⋄ E = X`
* Since `X = X`, Associativity unconditionally holds.

**Case 2 (where `X ⋄ X = X`)**:
* Left side: `(X ⋄ X) ⋄ X = X ⋄ X = X`
* Right side: `X ⋄ (X ⋄ X) = X ⋄ X = X`
* Since `X = X`, Associativity unconditionally holds.

**Q.E.D.** Once you successfully lock in an identity element on a 2-inhabitant type, there is simply no remaining mathematical room in the $2 \times 2$ matrix for associativity to break!

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
Given `data Proxy a = Proxy`, how do we formally prove the only valid function of type `(a -> a) -> Proxy a -> Proxy a` preserving structure is the identity?

1.  Let `g :: Proxy a -> Proxy a` be a total, terminating function.
2.  The only inhabited value of `Proxy a` at the term level is `Proxy`.
3.  Therefore, `g Proxy = Proxy`.
4.  By definition, the `id` function is `id x = x`. 
5.  Thus, `id Proxy = Proxy`.
6.  Since `g Proxy = id Proxy` for the sole value of the type, `g = id`.
    Because `g` is the only total mapping, `fmap id = id` trivially holds, and no other lawful interpretation exists.

### Proof of Identity Implies Composition
In Haskell, if `fmap id = id` (Identity Law) holds for a parametrically polymorphic `fmap`, then `fmap (f . g) = fmap f . fmap g` (Composition Law) is automatically satisfied. This is a direct consequence of the **Naturality** of `fmap`.

1.  **The Signature**: `fmap :: forall a b. (a -> b) -> F a -> F b`.
2.  **Naturality Condition**: For any natural transformation $\eta : F \to G$, and any function $k : a \to b$, the condition $\eta_b \circ F(k) = G(k) \circ \eta_a$ must hold.
3.  **Treating `fmap` as a transformation**: We can view $fmap(f)$ as a transformation from the functor `F` to itself.
4.  **Free Theorem**: The "Free Theorem" for the type of `fmap` (as derived in Wadler's paper) states:
    `fmap f . fmap g = fmap (f . g)`
    This equality holds because the type of `fmap` is so restrictive that it cannot differentiate between "applying a composition" and "composing two applications" without knowing the internal structure of the types—which parametricity forbids.

### Proof of Identity Uniqueness
If a binary operation $B$ has a left identity $I_L$ and a right identity $I_R$, they must be structurally isomorphic ($I_L \cong I_R$).

1.  By definition of a Left Identity, for *any* type $A$: $B(I_L, A) \cong A$.
2.  Let's choose $A = I_R$. Therefore: $B(I_L, I_R) \cong I_R$.
3.  By definition of a Right Identity, for *any* type $A$: $B(A, I_R) \cong A$.
4.  Let's choose $A = I_L$. Therefore: $B(I_L, I_R) \cong I_L$.
5.  Since both $I_L$ and $I_R$ are mathematically isomorphic to the exact same structural element $B(I_L, I_R)$, then logically $I_L \cong I_R$.

Thus, if both exist, they are structurally identical.

### Strict Equality vs. Structural Isomorphism
It is critical mathematically and computationally to distinguish between typeclass "laws" and structural "isomorphisms" or Unitors.

**Typeclass Laws (Behavioral strict equality)**:
These govern how typeclass methods (like `fmap`, `bimap`, `>>=`) must behave computationally. When we write a law like `fmap id == id`, we mean **strict equality**. The two sides must evaluate to the exact same value of the exact same type. If you map the identity over `[1, 2]`, you must get the exact `[1, 2]` back, not a copy or an equivalent wrapper. 

**Structural Isomorphisms (Type-level mapping)**:
These govern the "shape" of the types themselves. When we say $B(I, A) \cong A$ (e.g., `(Either Void A) ≅ A`), we are describing **structural isomorphism**. The compiler knows that `Either Void Bool` and `Bool` are two entirely different types (`Left True` vs `True`). However, because `Void` contains no information, we can write a perfect, lossless two-way mapping between the two structures. These mappings are the exact "Left/Right Unitors". They are not equalities; they are natural transformations between non-equal types.

## Bibliography
*   **"Theorems for free!"** by Philip Wadler (1989).
*   **"Notions of computation and monads"** by Eugenio Moggi (1991).
*   **"Fast and Loose Reasoning is Morally Correct"** by Nils Anders Danielsson, John Hughes, Patrik Jansson, and Jeremy Gibbons (2006).
*   **"The Typeclassopedia"** by Brent Yorgey (The Monad Reader Issue 13, 2009).
*   *(Recommended Reading)* **"Thinking with Types"** by Sandy Maguire.
*   *(Recommended Reading)* **"Functors, Applicatives, And Monads In Pictures"** by Aditya Bhargava.
*   **"Category Theory for Programmers"** (Introductory Notes) by Bartosz Milewski ([PDF Link](https://ai.dmi.unibas.ch/research/reading_group/milewski-2023-01-30.pdf)).
*   **"Algebra of Programming"** by Richard Bird and Oege de Moor (1997) — *A foundational text exploring how algebras and functor subcategories are derived systematically from building blocks like Bifunctors.*

---

