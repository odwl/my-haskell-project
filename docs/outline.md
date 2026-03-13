# Master Outline: The Minimal Haskell Series

This document serves as the master architectural blueprint for the four-part "Minimal Haskell" blog series.

## Part 1: The Atoms (docs/01_the_atoms.md)

### 1. Introduction
- **The Teaching Narrative**: Using absolute minimalism to prove the "forced hand" of parametricity.
- **Intended Audience**: Mathematicians and intermediate functional programmers seeking an axiomatic, bottom-up categorical understanding.
- **Scope**: Focusing on Endofunctors within the `Hask` category.
- **Parametricity**: The protagonist; how polymorphism forces unique implementations of `fmap`, `pure`, and `bind`.

### Chapter 1: The Foundations of Functors and Bifunctors
#### Section 1.1: What is a Functor?
- **Foundations**: Well-kindedness (`* -> *`), Unconstrained Morphism Mapping (`fmap`).
- **Parametricity**: Theorems for free!
- **Mathematical Laws**: Identity and Composition.
  - **Automated Law Testing**: Professional verification with `tasty-checkers`.
  - **Caveat: Testing vs. Proof**: The probabilistic nature of property-based testing.
- **Categorical Functors that are not Haskell Functors**:
  - **Bifunctors**: Wrong kind (`* -> * -> *`).
  - **Restricted Functors**: Requires constraints (e.g., `Data.Set` with `Ord`).
- **Law-Breaking Functors (Non-Valid)**:
  - **The Counter**: Mutations breaking identity.
  - **The Malicious Functor**: Hidden behavior conditionally breaking identity.

#### Section 1.2: Minimal Functors and Bifunctors
> *Focus: Atoms at the Functor level only.*

##### Minimal Functors
- **`Zero` (Uninhabited)**: The absolute bottom; zero constructors, mathematically impossible to instantiate.
- **`Proxy` (The Empty Box)**: One constructor; `fmap` MUST ignore the function.
- **`Const r` (Constant Context)**: Context-only. Includes notes on `Const Void` (isomorphic to `Zero`) and `Const ()` (isomorphic to `Proxy`).
- **`Identity` (One)**: Transparency; mapping forced by possession of `a`.
- **`(->) r` (The Exponential)**: The Reader function; infinite delayed computational data via domain `r`.

##### Minimal Bifunctors
- **`BiProxy`**: Zero data, ignores both type parameters.
- **`ConstContext r`**: Context data only, ignores both type parameters.
- **`ConstLeft` / `ConstRight`**: One-sided constants acting as half-Identity, half-Proxy.
- **Sum (`Either`)**: The fundamental co-product operations of two types.
- **Product (`(,)`)**: The fundamental product operation of two types.
- **`BiReader r` (Dual Exponential)**: The delayed computation of a tuple.

#### Section 1.3: Bifunctors as Binary Operations on Functors
- **Binary Operator**: Treating $H(x) = B(F(x), G(x))$ as combinatorial arithmetic of functors.
- **Minimal Examples**:
  - $0 + 1 = 1$: `Either (Zero a) (Proxy a)` isomorphic to `Proxy`.
  - $0 \times 1 = 0$: `(Zero a, Proxy a)` isomorphic to `Zero`.
  - $1 + 1 = 2$: `Either (Proxy a) (Proxy a)` isomorphic to `Const Bool`.
  - $1 \times 1 = 1$: `(Proxy a, Proxy a)` isomorphic to `Proxy`.
  - $1 \times X = X$: `(Proxy a, Identity a)` isomorphic to `Identity`.
- **The Ordinals**: Showing how `Const r` mathematically maps to $0$, $1$, and $2$ based on inhabited states now that $+$ and $\times$ are defined.

#### Section 1.4: Deriving the Atoms from Bifunctors
- **1. Extracting a Functor from a Bifunctor**: Explaining partial application and why picking an arbitrary type $T$ is not mathematically "natural".
- **2. Bifunctors with Identity ("Naturality")**: How possessing a left or right identity forces a unique canonical choice. Classifying our zoo of bifunctors (`BiProxy`, `->`, `Either`, `(,)`).
- **3. Sub-Category Closures**: Why applying a single bifunctor and its identity leads to a trivial, flat lineage.
- **4. Polynomial Functors**: The magic of combining multiple interacting bifunctors (+ and $\times$) to generate infinitely rich families (ADTs).
- **5. Is Identity Required?**: Validating non-unital bifunctor sets. A profound example: generating the strictly **Non-Empty** subcategory of data structures (`Data.List.NonEmpty`) by possessing Sum ($+$) and Product ($\times$) but explicitly missing the Product identity $1$.
- **6. Basic Examples**: Translating operations into compounds like $1 + X$ (`Maybe`), $E + X$ (Error), and $E \times X$ (Writer).
- **The Ultimate Closure**: Sum (+), Product (*), and Exponential (`->`) perfectly close to form a **Bicartesian Closed Category (BCC)**, creating the mathematical foundation of typed programming.

#### Section 1.5: Generating Functor Subcategories (The Algebra as a Special Case)
- **Closure of Bifunctors**: Generating a subcategory of functors from a starting set of bifunctors.
- **What "Algebraic" means**: A specialized subcategory generated strictly by adding (Sum) and multiplying (Product) atoms, excluding Exponentials.
- **The Algebra of Functors (1D)**: Summing (+) and Multiplying (*) single-variable building blocks.
- **Bifunctors**: Sums (`Either`) and Products (`(,)`).
- **Monoidal Laws**: Brief mention of pentagon and triangle laws.
- **Proxy Math**: $1 + 1 = 2$ (`Const Bool`) and $1 \times 1 = 1$.
- **The Algebra of Bifunctors (2D)**: Extending the exact same algebra to two-variable polynomials (`ConstLeft`, `ConstRight`).

#### Section 1.6: Polynomial Functors
- **Polynomial Functors**: The relationship between Category Theory and ADTs.
- **Why the Name "Polynomial"?**: Exploring the $1 + X + X^2$ shape equations mapping directly to types.

#### Section 1.7: The Parallel Functor Ecosystem
- **Bifunctors**: Product category ($* \to * \to *$).
- **Contravariant**: Opposite category ($Hask^{op} \to Hask$).
- **Profunctors**: Mixed variance mapping.
- **MonoFunctor (`mono-traversable`)**: Mapping monomorphic or constrained structures (`Data.Set`).

#### Section 1.8: Discovering Molecules (Compounds)
- **`Maybe`**: $1 + X$ (Sum of Proxy and Identity).
- **`Writer`**: $r \times X$ (Product of Const and Identity).
- **`List`**: $1 + X \times L(X)$ (Recursive chain).
- **Fixed Points**: Shape equations (Lists vs. Trees).

---

## Part 2: The Evolution (docs/02_the_evolution.md)

### Chapter 2: The Applicative Evolution
#### Section 2.1: Foundations
- **Powers**: `pure` (lifting values) and `<*>` (lifting application).
- **Applicative Laws**: Predictable sequencing.
- **Section 2.2: Automated Law Testing**: Using `testBatch` for Applicatives.

#### Section 2.3: Upgrading the Atoms
- **`Proxy`**: Trivial upgrade.
- **`Const r` (The Monoid Twist)**: Why `Applicative` necessitates `mempty` and `mappend`.
- **`Identity`**: Trivial application.

### Chapter 3: The Monadic Conclusion
#### Section 3.1: Foundations
- **The Monadic Triad**: Bind, Join, Kleisli.
- **Categorical Mu**: `join` as the foundational flattening operation.
- **Section 3.2: Automated Law Testing**: Using `testBatch` for Monads.

#### Section 3.3: The Final Evolution
- **`Proxy` & `Identity`**: Trivial upgrades.
- **`Const r` (The Monad Barrier)**: Why the evolution stops; violating the Left Identity law.

### Conclusion: The Tale of Three Minimals
- **Spectrum of Necessity**: How Zero, Context, and One define the logic of types.
- **The Alchemy of ADTs**: Discovering the universe from atoms.

---

## Part 3: The Aggregators (docs/03_the_aggregators.md)

### Chapter 6: Other Minimals
#### Section 6.1: Minimal Monoid
- **Definition**: Unpacking `mempty` and the binary operation (`mappend` or `<>`).
- **The Monoid Laws**: Associativity, Left Identity, and Right Identity.
  - **Developer Responsibility & Testing**: Using `testBatch` to automate equality testing (unlike Bifunctor natural identities which use isomorphism).
- **The Top Minimal Implementations**:
  - **1 Inhabitant (`()`)**: The absolute minimum. `mempty = ()` and `() <> () = ()`.
  - **2 Inhabitants (`Bool`)**: Breaking down the 16 mathematically possible binary operations and why exactly 4 of them (`All`, `Any`, `Equivalence`, `Xor`) form Monoids.
  - **3 Inhabitants (e.g., `Ordering`)**: The combinatorial explosion: 19,683 possible operations → 243 with an identity → Exactly 33 valid Monoids!
  - **Countably Infinite Inhabitants (e.g., `Integer`)**: There are infinitely many valid Monoids (e.g., `Sum`, `Product`, `Max`, `Min`, `List` concatenation).
- **Commutativity is Not Required**: Explaining that Monoids only require Associativity and Identity. Commutative monoids exist (Abelian Monoids like `Sum`), but many foundational programming monoids (like `List`, `First`, and `Endo`) are strictly non-commutative.
- **The Limits of Parametricity**: Unlike Functors (`* -> *`), Monoids (`*`) operate on concrete values, meaning parametricity doesn't force a single unique implementation (e.g., `Double` can be `Sum` or `Product`).
- **The Sum Monoid**: A practical numeric monoid (`mempty = 0`, `<> = +`).
- **Aggregation with `foldMap`**: How to map elements to a Monoid and fold them securely without requiring the full Monad machinery.

### Chapter 7: Minimal Foldable and the Foldable Laws
#### Section 7.1: What is a Foldable?
- **1. A Well-Kinded Type Constructor**: Why `Foldable` requires kind `* -> *` (excluding `Int` and `Void`).
- **2. Unconstrained Morphism Mapping (`foldMap`)**: The Minimal Complete Definition and the logic of Parametricity Constraints.
- **3. Mathematical Laws**: Ensuring fold consistency across modes of traversal.
- **Category Theory Origin**: The lossy projection onto a Monoid (`traverse` via `Const`).

#### Section 7.2: The Absolute Minimum Foldable
1. The Mathematically Unreachable Foldable (`Zero`) - The vacuous success
2. The Empty Foldable (`Proxy`) - Conjuring `mempty` out of thin air
3. The Single-Element Foldable (`Identity`) - The "Forced Hand" of the Foldable Laws
4. The Ghost Data (`Const r`) - Holding the wrong type mapping
5. The Static Pairing (`(e, a)`) - Discarding unmappable bounds
6. The Branching Possibility (`Either e`) - Fusing proofs across branches
7. The Homomorphism (The Essence of `toList`) as a structure-preserving map.

### Chapter 8: Minimal Traversable and the Algebra of Effects
#### Section 8.1: What is Traversable?
- **Effectful Sequencing**: Navigating the shape from left to right while performing an `Applicative` effect.
- **The Definition**: `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)`
- **The Holy Trinity**: Functor (Shape Preservation), Foldable (Aggregation), Traversable (Effectful Sequencing).

#### Section 8.2: The Absolute Minimum Traversable Atoms
- **`Proxy` (The Empty Sequence)**: Returning `pure Proxy`.
- **`Identity` (The Single Effect)**: Sequencing the single effect and rebuilding bounds using `fmap Identity`.

#### Section 8.3: The Algebra of Traversables
- **Traversable Sums (`Either`)**: Pattern matching and delegating the effectful mapping.
- **Traversable Products (`(,)`)**: Sequencing left, sequencing right, bundling via `<*>`.
- **Traversable Composition (`Compose`)**: Composing nested traversals transparently.
- **The Synthesis**: How the polynomial algebra mathematical proves that every single ADT generated by Sums, Products, 1s, and 0s is mathematically guaranteed to be Traversable (powering `DeriveTraversable`).

---

## Part 4: The Deep Math (docs/04_the_deep_math.md)

### Chapter 4: Deep Dive into Bifunctors
#### Section 4.1: The True Nature of Bifunctors
- **Product Categories**: A Bifunctor is just a normal functor from a product category $\mathcal{C} \times \mathcal{D} \to \mathcal{E}$.
- **Haskell implementation**: Mapping a pair of morphisms (`bimap`) from $\mathbf{Hask} \times \mathbf{Hask} \to \mathbf{Hask}$.

#### Section 4.2: The Laws of Bifunctors
- **Identity**: `bimap id id == id`.
- **Composition**: `bimap (f . g) (h . i) == bimap f h . bimap g i`.
- **Equivalences**: The relationship between `bimap`, `first`, and `second`.

### Chapter 5: Monoidal Categories
#### Section 5.1: The Pentagon and Triangle Laws
- **The Tensor Product**: Why `Either` and `(,)` are special Bifunctors.
- **Coherence Conditions**: Associativity (Pentagon) and Unit (Triangle) laws.
- *(Future section for deep dive into formal tensor products).*

### Annex: Proofs and Derivations
- **Summary & Bundle Taxonomy**:
  - **Final Summary**: Shape and Preservation.
  - **Type Bundle Taxonomy**: When to use `type`, `newtype`, or `data`.
- **Monad Equivalence**: Bind/Join/Kleisli.
- **Uniqueness Proof**: Formal mapping proof for `Proxy`.
- **Identity Implies Composition**: Formal proof of the parametricity shortcut.
- **Parametricity**: A deep dive into Natural Transformations, Ends, and Relational Fibrations.

### Annex A: Proof of 2-Inhabitant Associativity
- **The Cayley Table Proof**: Mathematical proof showing that once a two-sided identity element is locked in for a 2-inhabitant type, the remaining $2 \times 2$ grid leaves no structural room for associativity to fail.

### Bibliography
- Reading list: Wadler, Moggi, Danielsson, Milewski, Swierstra, McBride.
