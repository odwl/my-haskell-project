# Outline: Minimal Functors, Applicatives, and Monads in Haskell

This document serves as a collaborative living outline for the documentation and implementation of minimal categorical structures in Haskell.

## 1. Introduction
- **The Journey**: Constructing minimal instances to strip away noise.
- **Parametricity**: The core constraint that forces unique implementations.
- **Category Hask**: Notes on loose reasoning and bottom (`_|_`).

## Chapter 1: Foundations
- **Section 1.1**: What is a Functor? (Categorical vs. Haskell Endofunctors).
- **Section 1.2**: Parametricity and "Theorems for Free".
- **Section 1.3**: `fmap` signatures and Functor Laws.
- **Section 1.4**: Applicative Functors (`pure`, `<*>`).
- **Section 1.5**: Monads (equivalence of `bind`, `join`, and `kleisli`).

## Chapter 2: The Minimal Functor/Monad (Proxy)
- **Identity**: `data MinF a = Val`.
- **Implementation**: Why `fmap`, `pure`, and `bind` are mathematically forced.
- **Deriving**: Letting the compiler tie its own hands.

## Chapter 3: The Minimal Applicative (Const)
- **Identity**: `newtype Const r a = Const r`.
- **Monoid Constraint**: Why `mempty` and `mappend` are recursive requirements for `pure` and `<*>`.
- **Monad Failure**: Why `Const` cannot be a lawful Monad.

## Chapter 4: The Minimal Synchronous Monad (Identity)
- **Identity**: `data IdF a = IdVal a`.
- **Transparency**: The simplest wrapper that implements all three levels.

## Chapter 5: The Algebra of Functors
- **Foundational Blocks**: `Const r` ($c$) and `Identity` ($X$).
- **Combinations**: How Sums and Products build more complex types.

## Chapter 6: Functors out of Proxy and Identity
- **The Sum (Maybe)**: $1 + X$.
- **The Product (Identity)**: $1 \times X = X$.
- **Extensions**: Writer Monad (`r * X`).

## Chapter 7: Functors entirely out of Proxy
- **Fundamental Rule**: Proxy as the number 1.
- **Proxy + Proxy**: `Const Bool` ($1 + 1 = 2$).
- **Proxy * Proxy**: `Proxy` ($1 \times 1 = 1$).

## Chapter 8: Recursion and Fixed Points
- **Recursive Sums/Products**: Building the `List`.
- **Shape Equations**: $L(X) = 1 + X \times L(X)$.
- **Alternative Shapes**: Binary Trees.

## Annex: Proofs
- Algebraic proof of Monad path equivalence.
- Proof of unique structural identity for `MinF`.

---
> [!TIP]
> **Collaborator Note**: Feel free to add comments, questions, or proposed changes directly to this outline!
