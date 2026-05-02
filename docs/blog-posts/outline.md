# Master Outline: The Minimal Haskell Series

This document serves as the master architectural blueprint for the "Minimal Haskell" blog series. The pedagogy relies on a fundamental mathematical dichotomy: mastering the **Structures** (Nouns / Cardinality) before mastering the **Algebras** (Verbs / Typeclasses / Laws), applied consistently across the Kind Signatures.

## Universe 1: Concrete Types (Kind `Type`)

### Part 1: The Concrete Structures (No Laws) (`docs/01_concrete_structures.md`)
- **Focus**: The geometry of types, counting inhabitants, and categorical analogs.
- **Chapter 1: Types of Kind `Type`**
  - Section 1.1: `Void` (0 Inhabitants / Initial Object) 
  - Section 1.2: `()` (1 Inhabitant / Terminal Object)
  - Section 1.3: `Bool` (2 Inhabitants / Coproduct of Terminal Objects)
  - Section 1.4: Other Finite Inhabitants (Products and Coproducts)
  - Section 1.5: Infinite Inhabitants (Countable and Uncountable)

### Part 2: The Concrete Algebras (Laws) (`docs/02_concrete_algebras.md`)
- **Focus**: Attaching behavior to concrete structures using lawful typeclasses.
- **Chapter 1: Equivalence and Ordering**
  - `Eq`: The laws of mathematical equivalence.
  - `Ord`: The laws of total ordering.
- **Chapter 2: Associative Binary Operations ($+$ and $\times$)**
  - `Semigroup`: The laws of associativity.
  - `Monoid`: The laws of identity (`mempty` and `<>`).
  - **The Top Minimal Implementations**: Unpacking exactly why `Bool` has 4 valid Monoids, and why 3-inhabitant types have exactly 33!

***

## Universe 2: Higher-Kinded Types (Kind `Type -> Type`)

### Part 3: HKT Shape Structures & Algebras (`docs/03_the_holy_trinity.md`)
- **Focus**: Parameterized types of kind `Type -> Type`, shape preservation, context combination, and sequencing.

#### Chapter 1: HKT Structures (No Laws)
- Section 1.1: `EmptyHkt` (0 Inhabitants)
- Section 1.2: `Proxy` (1 Inhabitant)
- Section 1.3: `Const Bool a` (2 Inhabitants)

#### Chapter 2: HKT Algebras (Laws)
- **Functor (Shape Preservation)**: parametricity and the laws of Identity and Composition.
  - Minimal Functors: `Zero`, `Proxy`, `Const r`, `Identity`, `(->) r`.
- **Foldable (Lossy Aggregation)**: The adjunction mapping (`foldMap`).
  - Minimal Foldables: `Zero`, `Proxy`, `Identity`, `Const`, `Either`.
- **Traversable (Effectful Folding)**: Commuting structure and effects.
- **Applicative (Context Aggregation)**: Lifting values and application.
  - The `Const` Twist: Why `Applicative` relies on `Monoid`.
- **Monad (Effectful Sequencing)**: Bind, Join, Kleisli, and the `Const` barrier.

***

## Universe 3: Binary HKTs (Kind `Type -> Type -> Type`)

### Part 4: HKT2 Morphic Structures & Workflows (`docs/04_morphisms_pipelines.md`)
- **Focus**: Two-parameter types of kind `Type -> Type -> Type`, relational morphisms, and computation pipelines.

#### Chapter 1: HKT2 Structures (No Laws)
- Section 1.1: `Empty2` (0 Inhabitants)
- Section 1.2: `Const2` (1 Inhabitant)
- Section 1.3: `Bool2` (2 Inhabitants)

#### Chapter 2: HKT2 Algebras (Laws)
- **Bifunctor (Binary Morphisms)**: Covariant mapping in both branches (`bimap`).
  - Minimal Bifunctors: `BiProxy`, `ConstContext`, `ConstLeft`, `ConstRight`, `Either`, `(,)`, `BiReader`.
  - Bifunctors as Binary Operations on Functors (e.g., $0 + 1 = 1$, $1 + 1 = 2$).
  - Deriving Atoms from Bifunctors (Left and Right Identity Unitors).
  - Polynomial Functors ($F(X) = 1 + Int + X^2$).
  - The Parallel Functor Ecosystem (`Bifunctor`, `Contravariant`, `Profunctor`, `MonoFunctor`).
- **Category & Arrow (Pipeline Workflows)**
  - `Category`: Standard composition (`.`), associative and identity laws.
  - `Arrow`: Function lifting (`arr`), parallel splits (`first`, `***`, `&&&`).

***

## Advanced Higher-Order Combinatorics

### Part 5: Functor Monoids & N-Ary Glues (`docs/05_n_ary_glues.md`)
- **Focus**: The mathematical foundations of combining parameterized types.
- **Chapter 1: The Functor Monoid (The True Engine)**
  - Section 1.1: The Minimal Generators (`Zero`/`Sum`, `Proxy`/`Product`, `Fix`, `(->)`, `Compose`).
- **Chapter 2: From Monoids to N-ary Glues**
  - Section 2.1: Expanding beyond Binary (`Either`, `(,)`) to N-Ary structures (`UnionF`, `HListF`).
  - Section 2.2: The Ultimate Generator: System F (Church Encodings).
- **Chapter 3: The Value-Level Symmetry**
  - Section 3.1: The Value-Level Product (`Applicative`).
  - Section 3.2: The Value-Level Sum (`Alternative`).

### Part 6: The Deep Math (`docs/06_deep_math_and_proofs.md`)
- **Chapter 4: Deep Dive into Bifunctors**
  - **Product Categories**: A Bifunctor is just a normal functor from a product category $\mathcal{C} \times \mathcal{D} \to \mathcal{E}$.
  - **Haskell implementation**: Mapping a pair of morphisms (`bimap`) from $\mathbf{Hask} \times \mathbf{Hask} \to \mathbf{Hask}$.
- **Section 4.2: The Laws of Bifunctors**
  - **Identity**: `bimap id id == id`.
  - **Composition**: `bimap (f . g) (h . i) == bimap f h . bimap g i`.
  - **Equivalences**: The relationship between `bimap`, `first`, and `second`.
- **Chapter 5: Monoidal Categories**
  - **The Tensor Product**: Why `Either` and `(,)` are special Bifunctors.
  - **Coherence Conditions**: Associativity (Pentagon) and Unit (Triangle) laws.
- **Annex: Proofs and Derivations**
  - **Summary & Bundle Taxonomy** (When to use `type`, `newtype`, or `data`).
  - **Monad Equivalence**: Bind/Join/Kleisli.
  - **Uniqueness Proof**: Formal mapping proof for `Proxy`.
  - **Identity Implies Composition**: Formal proof of the parametricity shortcut.
  - **Parametricity**: A deep dive into Natural Transformations, Ends, and Relational Fibrations.
- **Annex A: Proof of 2-Inhabitant Associativity**
  - **The Cayley Table Proof**: Mathematical proof showing that once a two-sided identity element is locked in for a 2-inhabitant type, the remaining $2 \times 2$ grid leaves no structural room for associativity to fail.

### Part 7: Bibliography (`docs/07_bibliography.md`)
- **Focus**: Centralized list of foundational papers, influential articles, and recommended reading for the concepts discussed throughout this series.
