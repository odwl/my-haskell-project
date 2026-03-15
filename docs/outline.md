# Master Outline: The Minimal Haskell Series

This document serves as the master architectural blueprint for the "Minimal Haskell" blog series. The pedagogy relies on a fundamental mathematical dichotomy: mastering the **Structures** (Nouns / Cardinality) before mastering the **Algebras** (Verbs / Typeclasses / Laws).

## Universe 1: Concrete Types (Kind `Type`)

### Part 1: The Structures (No Laws) (`docs/minimal_types.md`)
- **Focus**: The geometry of types, counting inhabitants, and categorical analogs.
- **Chapter 1: Types of Kind `Type`**
  - Section 1.1: `Void` (0 Inhabitants / Initial Object) 
  - Section 1.2: `()` (1 Inhabitant / Terminal Object)
  - Section 1.3: `Bool` (2 Inhabitants / Coproduct of Terminal Objects)
  - Section 1.4: Infinite Inhabitants (Streams and Functions)

### Part 2: The Algebras (Laws) (`docs/03_the_monoid.md` - TO BE RENAMED)
- **Focus**: Attaching behavior to concrete structures using lawful typeclasses.
- **Chapter 1: Equivalence and Ordering**
  - `Eq`: The laws of mathematical equivalence.
  - `Ord`: The laws of total ordering.
- **Chapter 2: Associative Binary Operations ($+$ and $\times$)**
  - `Semigroup`: The laws of associativity.
  - `Monoid`: The laws of identity (`mempty` and `<>`).
  - **The Top Minimal Implementations**: Unpacking exactly why `Bool` has 4 valid Monoids, and why 3-inhabitant types have exactly 33!

---

## Universe 2: Higher-Kinded Types (Kind `Type -> Type`)

### Part 3: The Parameterized Structures (No Laws) (`docs/minimal_types.md` - continued)
- **Focus**: Parameterized shapes and "empty boxes".
- **Chapter 2: Parameterized Types of Kind `Type -> Type`**
  - Section 2.1: `VoidFoldable` (0 Inhabitants)
  - Section 2.2: `Proxy` (1 Inhabitant)
  - Section 2.3: `Const Bool a` (2 Inhabitants)

### Part 4: The Algebras of Shape (`docs/01_the_atoms.md`, `02_the_evolution.md`, `04_the_foldable.md`)
- **Focus**: The "holy trinity" of shapes (Functor, Foldable, Traversable) and computational contexts (Applicative, Monad).

#### Chapter 1: Functor & Bifunctor (Shape Preservation)
- parametricity and the laws of Identity and Composition.
- Minimal Functors: `Zero`, `Proxy`, `Const r`, `Identity`, `(->)`.

#### Chapter 2: Foldable (Lossy Aggregation)
- The adjunction mapping (`foldMap`).
- Minimal Foldables: `Zero`, `Proxy`, `Identity`, `Const`, `Either`.

#### Chapter 3: Applicative (Context Aggregation)
- Lifting values and application.
- The `Const` Twist: Why `Applicative` relies on `Monoid`.

#### Chapter 4: Monad (Effectful Sequencing)
- Bind, Join, Kleisli, and the `Const` barrier.

#### Chapter 5: Traversable (Effectful Folding)
- Commuting structure and effects.

---

## Universe 3: Functor Combinators & N-Ary Glues

### Part 5: The Functor Monoids (`docs/annex_n_ary_glues.md`)
- **Focus**: The mathematical foundations of combining parameterized types.
- **Chapter 1: The N-Ary Glues**
  - Section 1.1: Expanding beyond Binary (`Either`, `(,)`) to N-Ary structures.
  - Section 1.2: The trivial glues: `NaryZero` and `NaryProxy`.
  - Section 1.3: Real N-Ary glues: `Union` (n-ary Sum) and `HList` (n-ary Product).
- **Chapter 2: The Functor Monoid (Combinators)**
  - Section 2.1: Treating Functors as atoms and defining type-level monoids (`FunctorMonoid`).
  - Section 2.2: The Combinator Generators: `Zero`/`Sum`, `Proxy`/`Product`, `Fix`, `(->)`, and `Compose`/`Identity`.
  - Section 2.3: Connection to value-level counterparts (`Applicative` and `Alternative`).

---

## Part 6: The Deep Math (`docs/05_the_deep_math.md`)

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
