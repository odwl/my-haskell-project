# Outline: Minimal Functors, Applicatives, and Monads in Haskell

This document is a rich structural breakdown of the "Minimal Functors" post, serving as a roadmap for the theoretical and pedagogical journey.

## 1. Introduction
- **The Teaching Narrative**: Using absolute minimalism to prove the "forced hand" of parametricity.
- **Intended Audience**: Mathematicians and intermediate functional programmers seeking an axiomatic, bottom-up categorical understanding.
- **Scope**: Focusing on Endofunctors within the `Hask` category.
- **Parametricity**: The protagonist; how polymorphism forces unique implementations of `fmap`, `pure`, and `bind`.

## Chapter 1: The Foundations of Functors
### Section 1.1: What is a Functor?
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

### Section 1.2: Minimal Functors and Bifunctors
> *Focus: Atoms at the Functor level only.*

#### Minimal Functors
- **`Zero` (Uninhabited)**: The absolute bottom; zero constructors, mathematically impossible to instantiate.
- **`Proxy` (The Empty Box)**: One constructor; `fmap` MUST ignore the function.
- **`Const r` (Constant Context)**: Context-only. Includes notes on `Const Void` (isomorphic to `Zero`) and `Const ()` (isomorphic to `Proxy`).
- **`Identity` (One)**: Transparency; mapping forced by possession of `a`.
- **`(->) r` (The Exponential)**: The Reader function; infinite delayed computational data via domain `r`.

#### Minimal Bifunctors
- **`BiProxy`**: Zero data, ignores both type parameters.
- **`ConstContext r`**: Context data only, ignores both type parameters.
- **`ConstLeft` / `ConstRight`**: One-sided constants acting as half-Identity, half-Proxy.
- **Sum (`Either`)**: The fundamental co-product operations of two types.
- **Product (`(,)`)**: The fundamental product operation of two types.
- **`BiReader r` (Dual Exponential)**: The delayed computation of a tuple.

### Section 1.3: Deriving the Atoms from Bifunctors
- **1. What is an Identity?**: Defining an identity via Tensor Products (mathematical neutrality: $B(A, I) \cong A$).
- **2. Extracting Functors**: Creating constant Functors (`Zero`, `Proxy`) naturally from Bifunctor identities (`Void`, `()`).
- **3. Sub-Category Closures**: Why applying a single bifunctor and its identity leads to a trivial, flat lineage.
- **4. Polynomial Functors**: The magic of combining multiple interacting bifunctors (+ and $\times$) to generate infinitely rich families (ADTs).
- **5. Is Identity Required?**: Validating non-unital bifunctor sets vs. the necessity of 0 and 1 closures for terminating computational data structures.
- **The Ultimate Closure**: Sum (+), Product (*), and Exponential (`->`) perfectly close to form a **Bicartesian Closed Category (BCC)**, creating the mathematical foundation of typed programming.

### Section 1.4: Generating Functor Subcategories (The Algebra as a Special Case)
- **Closure of Bifunctors**: Generating a subcategory of functors from a starting set of bifunctors.
- **What "Algebraic" means**: A specialized subcategory generated strictly by adding (Sum) and multiplying (Product) atoms, excluding Exponentials.
- **The Algebra of Functors (1D)**: Summing (+) and Multiplying (*) single-variable building blocks.
- **Bifunctors**: Sums (`Either`) and Products (`(,)`).
- **Monoidal Laws**: Brief mention of pentagon and triangle laws.
- **Proxy Math**: $1 + 1 = 2$ (`Const Bool`) and $1 \times 1 = 1$.
- **The Algebra of Bifunctors (2D)**: Extending the exact same algebra to two-variable polynomials (`ConstLeft`, `ConstRight`).

### Section 1.5: Polynomial Functors
- **Polynomial Functors**: The relationship between Category Theory and ADTs.
- **Why the Name "Polynomial"?**: Exploring the $1 + X + X^2$ shape equations mapping directly to types.

### Section 1.6: The Parallel Functor Ecosystem
- **Bifunctors**: Product category ($* \to * \to *$).
- **Contravariant**: Opposite category ($Hask^{op} \to Hask$).
- **Profunctors**: Mixed variance mapping.
- **MonoFunctor (`mono-traversable`)**: Mapping monomorphic or constrained structures (`Data.Set`).

### Section 1.7: Discovering Molecules (Compounds)
- **`Maybe`**: $1 + X$ (Sum of Proxy and Identity).
- **`Writer`**: $r \times X$ (Product of Const and Identity).
- **`List`**: $1 + X \times L(X)$ (Recursive chain).
- **Fixed Points**: Shape equations (Lists vs. Trees).

## Chapter 2: The Applicative Evolution
### Section 2.1: Foundations
- **Powers**: `pure` (lifting values) and `<*>` (lifting application).
- **Applicative Laws**: Predictable sequencing.
- **Section 2.2: Automated Law Testing**: Using `testBatch` for Applicatives.

### Section 2.3: Upgrading the Atoms
- **`Proxy`**: Trivial upgrade.
- **`Const r` (The Monoid Twist)**: Why `Applicative` necessitates `mempty` and `mappend`.
- **`Identity`**: Trivial application.

## Chapter 3: The Monadic Conclusion
### Section 3.1: Foundations
- **The Monadic Triad**: Bind, Join, Kleisli.
- **Categorical Mu**: `join` as the foundational flattening operation.
- **Section 3.2: Automated Law Testing**: Using `testBatch` for Monads.

### Section 3.3: The Final Evolution
- **`Proxy` & `Identity`**: Trivial upgrades.
- **`Const r` (The Monad Barrier)**: Why the evolution stops; violating the Left Identity law.

## Chapter 4: Deep Dive into Bifunctors
### Section 4.1: The True Nature of Bifunctors
- **Product Categories**: A Bifunctor is just a normal functor from a product category $\mathcal{C} \times \mathcal{D} \to \mathcal{E}$.
- **Haskell implementation**: Mapping a pair of morphisms (`bimap`) from $\mathbf{Hask} \times \mathbf{Hask} \to \mathbf{Hask}$.

### Section 4.2: The Laws of Bifunctors
- **Identity**: `bimap id id == id`.
- **Composition**: `bimap (f . g) (h . i) == bimap f h . bimap g i`.
- **Equivalences**: The relationship between `bimap`, `first`, and `second`.

## Chapter 5: Monoidal Categories
### Section 5.1: The Pentagon and Triangle Laws
- **The Tensor Product**: Why `Either` and `(,)` are special Bifunctors.
- **Coherence Conditions**: Associativity (Pentagon) and Unit (Triangle) laws.
- *(Future section for deep dive into formal tensor products).*

## Conclusion: The Tale of Three Minimals
- **Spectrum of Necessity**: How Zero, Context, and One define the logic of types.
- **The Alchemy of ADTs**: Discovering the universe from atoms.

## Annex: Proofs and Derivations
- **Summary & Bundle Taxonomy**:
  - **Final Summary**: Shape and Preservation.
  - **Type Bundle Taxonomy**: When to use `type`, `newtype`, or `data`.
- **Monad Equivalence**: Bind/Join/Kleisli.
- **Uniqueness Proof**: Formal mapping proof for `Proxy`.
- **Identity Implies Composition**: Formal proof of the parametricity shortcut.
- **Parametricity**: A deep dive into Natural Transformations, Ends, and Relational Fibrations.

## Bibliography
- Reading list: Wadler, Moggi, Danielsson, Milewski.
