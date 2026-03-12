# Outline: Minimal Functors, Applicatives, and Monads in Haskell

This document is a rich structural breakdown of the "Minimal Functors" post, serving as a roadmap for the theoretical and pedagogical journey.

## 1. Introduction
- **The Teaching Narrative**: Using absolute minimalism to prove the "forced hand" of parametricity.
- **Scope**: Focusing on Endofunctors within the `Hask` category.
- **The "Hask" Category**: Justifying reasoning despite non-termination via "Fast and Loose Reasoning is Morally Correct."
- **Parametricity**: The protagonist of the journey; how polymorphism forces unique implementations of `fmap`, `pure`, and `bind`.

## Chapter 1: The Foundations of Functors, Applicatives, and Monads
### Section 1.1: What is a Functor?
- **Categorical Origin**: Saunders Mac Lane and structure-preserving mappings.
- **Haskell's 4 Conditions**:
    1. Kind signature `* -> *` (Endofunctors on Hask).
    2. Morphism mapping (`fmap`).
    3. No constraints (the parametricity requirement).
    4. Mathematical Laws (Identity and Composition).
- **Taxonomy of Categorical Functors (Haskell Non-Functors)**:
    - **The Forgetful Functor**: (e.g., `forget = id` on Monoids).
    - **The Type Inspector**: (e.g., `isInt` via `Typeable`).
    - **The Balanced Tree**: (e.g., `Data.Set` requiring `Ord`).
- **The Great Synthesis**: How constraints transform "Invalid" functors into "Restricted" functors on subcategories.
- **Type Bundle Taxonomy**: `type` vs `newtype` vs `data` (Purposes and constraints).
- **Signature of Maybe**: `data Maybe a = Nothing | Just a` (Kind `* -> *`).
### Section 1.2: The Constraint of Parametricity
- **"Theorems for free!"**: How type signatures dictate physical behavior. Implementation must preserve structure because the type `a` is invisible to the logic.
### Section 1.3: `fmap` (and `<$>`) and the Functor Laws
- **Mechanical Lifting**: `fmap` as function application lifted into context.
- **The Laws**: Identity (`fmap id == id`) and Composition.
- **Truly Non-Functors**: Law-breaking examples (e.g., `Counter` Mutation).
- **Enforcement**: QuickCheck and `tasty` for property-based testing of laws.
### Section 1.4: The Applicative Functor
- **Powers**: `pure` (lifting values) and `<*>` (lifting application).
### Section 1.5: The Monad
- **The Monadic Triad**: Three equivalent paths (Bind, Join, Kleisli).
- **Categorical Mu**: `join` as the foundational "flattening" operation.

## Chapter 2: The Minimal Functor/Monad (Proxy)
> *Theme: Zero computational data, Zero contextual data*
### Section 2.1: The Smallest Valid Candidate
- `data MinF a = Val`. Empty at runtime; `a` is purely phantom.
### Section 2.2: A Singular Functor Implementation
- `fmap _ Val = Val`. Implementation forced because no `a` is available for the function.
### Section 2.3 & 2.4: Upgrades
- **Applicative**: `pure` throws away the value to return `Val`.
- **Monad**: `join` flattens an empty box trivially.
- **Equivalence**: Identical to Haskell's `Proxy`.
### Section 2.5: Compiler Deduction
- Using `DeriveFunctor`—the compiler's deduction matches our mathematical proof.

## Chapter 3: The Minimal Applicative Functor (Const)
> *Theme: Zero computational data, Some contextual data `r`*
### Section 3.1: The Definition of `Const`
- `newtype Const r a = Const r`. Stores `r` but ignores `a`.
### Section 3.2: Functor Implementation
- Function is ignored; `r` value is passed along.
### Section 3.3: The Applicative Twist
- **Monoid Requirement**: Why `Applicative` necessitates `Monoid r`.
    - `pure` requires `mempty` to initialize.
    - `<*>` requires `mappend` to combine contexts.
### Section 3.4: The Monad Barrier
- `bind` unrecoverably drops `r` values from the function, violating the Left Identity law.

## Chapter 4: The Minimal Synchronous Monad (Identity)
> *Theme: One computational data, Zero contextual data*
### Section 4.1: The Identity Candidate
- `data IdF a = IdVal a`. Transparent wrapper.
### Section 4.2: Functor Implementation
- Parametricity forces unboxing, mapping, and re-boxing.
### Section 4.3 & 4.4: Upgrades
- `pure` wraps; `bind` is raw function application. Identical to Haskell's `Identity`.

## Chapter 5: The Algebra of Functors (Sums and Products)
- **Algebraic Logic**: Every ADT is a combination of foundational blocks.
- **Baselines**: `Proxy` $\cong 1$. `Identity` $\cong X$.
- **Practical Use**: `Proxy` for guiding type inference (e.g., `Storable`).

## Chapter 6: Functors out of Proxy and Identity
- **The Sum ($1 + X$)**: Summing `Proxy` and `Identity` yields `Maybe`. The Monad of choice/failure.
- **The Product ($1$ vs $r$)**: 
    - `1 * X = X` (Identity).
    - `r * X = Writer` (Logging). Constant context combined via product.

## Chapter 7: Functors entirely out of Proxy
- **Proxy Math**:
    - $1 + 1 = 2 \cong \text{Const Bool}$.
    - $1 * 1 = 1 \cong \text{Proxy}$.

## Chapter 8: Recursion and Fixed Points
- **Shape Equations**: Structures defined as the "Fixed Point" of an equation.
- **List**: $L(X) = 1 + X \cdot L(X)$.
- **Binary Tree**: $T(X) = 1 + X \cdot T(X)^2$.

## Chapter 9: Conclusion: The Tale of Three Minimals
- Re-cap: How `MinF`, `Const r`, and `IdF` define the spectrum of structural necessity in Haskell.

## Annex: Proofs and Derivations
- **Monad Equivalence**: Deriving `Join` <-> `Bind` <-> `Kleisli`.
- **Uniqueness Proof**: Formal proof that `MinF` has exactly one valid functor mapping.

## Bibliography
- Authoritative reading list Including Wadler, Moggi, Danielsson, and Milewski.
