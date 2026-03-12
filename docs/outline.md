# Outline: Minimal Functors, Applicatives, and Monads in Haskell

This document is a rich structural breakdown of the "Minimal Functors" post, serving as a roadmap for the theoretical and pedagogical journey.

## 1. Introduction
- **The Teaching Narrative**: Using absolute minimalism to prove the "forced hand" of parametricity.
- **Scope**: Focusing on Endofunctors within the `Hask` category.
- **Parametricity**: The protagonist; how polymorphism forces unique implementations of `fmap`, `pure`, and `bind`.

## Chapter 1: The Foundations of Functors
### Section 1.1: What is a Functor?
- **Foundations**: Well-kindedness (`* -> *`), Unconstrained Morphism Mapping (`fmap`).
- **Parametricity**: Theorems for free!
- **Some "Almost" Functors**: Subcategory functors (Forgetful, Type Inspector, Balanced Trees).
- **Mathematical Laws**: Identity and Composition.
- **Property-Based Testing**: QuickCheck and the **Pro Tip: Using `checkers`** for `testBatch`.

### Section 1.2: The Atomic Functors
> *Focus: Atoms at the Functor level only.*
- **`Proxy` (Zero)**: Empty box; `fmap` MUST ignore the function.
- **`Const r` (Accumulation)**: Context-only; no computational data.
- **`Identity` (One)**: Transparency; mapping forced by possession of `a`.

### Section 1.3: The Algebra of Functors
- **Atoms to Molecules**: Using Sums (+) and Products (*) as algebraic operators.

### Section 1.4: Discovering Molecules (Compounds)
- **`Maybe`**: $1 + X$ (Sum of Proxy and Identity).
- **`Writer`**: $r \times X$ (Product of Const and Identity).
- **`List`**: $1 + X \times L(X)$ (Recursive chain).
- **Fixed Points**: Shape equations (Lists vs. Trees).
- **Proxy Math**: $1 + 1 = 2$ (`Const Bool`) and $1 \times 1 = 1$.

### Section 1.5: Summary & Bundle Taxonomy
- **Summary**: Shape and Preservation.
- **Taxonomy**: When to use `type`, `newtype`, or `data`.

## Chapter 2: The Applicative Evolution
### Section 2.1: Foundations
- **Powers**: `pure` (lifting values) and `<*>` (lifting application).
- **Applicative Laws**: Predictable sequencing.

### Section 2.2: Upgrading the Atoms
- **`Proxy`**: Trivial upgrade.
- **`Const r` (The Monoid Twist)**: Why `Applicative` necessitates `mempty` and `mappend`.
- **`Identity`**: Trivial application.

## Chapter 3: The Monadic Conclusion
### Section 3.1: Foundations
- **The Monadic Triad**: Bind, Join, Kleisli.
- **Categorical Mu**: `join` as the foundational flattening operation.

### Section 3.2: The Final Evolution
- **`Proxy` & `Identity`**: Trivial upgrades.
- **`Const r` (The Monad Barrier)**: Why the evolution stops; violating the Left Identity law.

## Conclusion: The Tale of Three Minimals
- **Spectrum of Necessity**: How Zero, Context, and One define the logic of types.
- **The Alchemy of ADTs**: Discovering the universe from atoms.

## Annex: Proofs and Derivations
- **Monad Equivalence**: Bind/Join/Kleisli.
- **Uniqueness Proof**: Formal mapping proof for `MinF`.
- **Parametricity**: A deep dive into Natural Transformations, Ends, and Relational Fibrations.

## Bibliography
- Reading list: Wadler, Moggi, Danielsson, Milewski.
