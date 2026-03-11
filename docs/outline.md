# Outline: Minimal Functors, Applicatives, and Monads in Haskell

This document serves as a collaborative living outline for the documentation and implementation of minimal categorical structures in Haskell.

## 1. Introduction
- **The Journey**: Constructing minimal instances (like `Proxy` and `Identity`) to strip away domain-specific noise and reveal the pure mechanics of Haskell's core typeclasses.
- **Parametricity**: Exploring how the inability to inspect types at runtime forces implementations to be mathematically unique—the "theorems for free" philosophy.
- **Category Hask**: Acknowledging that while Haskell isn't a perfect mathematical category due to "bottom" (`_|_`), we can reason about it effectively by ignoring these edges.

## Chapter 1: Foundations
- **Section 1.1**: Defining Functors as structure-preserving mappings, distinguishing between categorical theory and Haskell's practical `Endofunctor` implementation.
- **Section 1.2**: Understanding how polymorphic signatures create "Intellectual Economy" by restricting possible code behaviors.
- **Section 1.3**: Breaking down `fmap` as function application lifted into a context, and the importance of the Identity and Composition laws.
- **Section 1.4**: Introducing `Applicative` powers: `pure` for entry into a context and `<*>` for applying functions already wrapped in contexts.
- **Section 1.5**: Proving the equivalence of `bind`, `join`, and `kleisli` composition as three paths to the same Monadic destination.

## Chapter 2: The Minimal Functor/Monad (Proxy)
- **Identity**: Defining `data MinF a = Val`, representing the absolute minimum structure with zero term-level data.
- **Implementation**: Showing why the compiler literally forces `fmap _ Val = Val` because there is no `a` to work with.
- **Deriving**: Practical use of `DeriveFunctor` to let GHC mechanically generate these inescapable implementations.

## Chapter 3: The Minimal Applicative (Const)
- **Identity**: Using `newtype Const r a = Const r` to hold contextual data `r` while ignoring the computational type `a`.
- **Monoid Constraint**: Why `mempty` and `mappend` are the fundamental requirements to satisfy the `Applicative` laws for constant types.
- **Monad Failure**: Demonstrating how the inability to access `a` makes it impossible to satisfy the Monad Left Identity law.

## Chapter 4: The Minimal Synchronous Monad (Identity)
- **Identity**: The `data IdF a = IdVal a` structure, which serves as the most transparent, "naked" wrapper for a value.
- **Transparency**: Implementing the full suite (Functor, Applicative, Monad) to show how an unobstructed value flows through context.

## Chapter 5: The Algebra of Functors
- **Foundational Blocks**: Treating `Proxy` as the number $1$ and `Identity` as the variable $X$ in a type-level polynomial.
- **Combinations**: How the concepts of "OR" (Sums) and "AND" (Products) allow us to build complex ADTs from minimal primitives.

## Chapter 6: Functors out of Proxy and Identity
- **The Sum (Maybe)**: Deriving `Maybe` as the sum of Zero and One ($1 + X$), representing the minimal monad of failure.
- **The Product (Identity)**: Proving that the product of Zero and One ($1 \times X$) collapses back into `Identity`, showing that `Proxy` acts as the multiplicative identity.
- **Extensions**: Generalizing the product to `Writer` by replacing the unit with a Monoidal constant `r`.

## Chapter 7: Functors entirely out of Proxy
- **Fundamental Rule**: Reinforcing that `Proxy` represents a single computational state with no attached data.
- **Proxy + Proxy**: Showing how summing two empty states ($1 + 1 = 2$) creates the `Const Bool` functor.
- **Proxy * Proxy**: Proving that a tuple of units ($1 \times 1 = 1$) remains isomorphic to a single unit.

## Chapter 8: Recursion and Fixed Points
- **Recursive Sums/Products**: Building the `List` type algebraically as $L(X) = 1 + X \times L(X)$.
- **Shape Equations**: Understanding how different polynomial formulas define the physical layout of data structures like Trees and Streams.
- **Alternative Shapes**: Brief comparison of shape formulas for different recursive types.

## Annex: Proofs
- **Equivalence**: Step-by-step derivation showing how `bind` and `join` are algebraically interchangeable.
- **Uniqueness**: A formal explanation of why no other lawful Functor instance can exist for `MinF`.

---
> [!TIP]
> **Collaborator Note**: Feel free to add comments, questions, or proposed changes directly to this outline!
