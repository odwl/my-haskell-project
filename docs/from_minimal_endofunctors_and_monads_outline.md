# Outline: Minimal Functors, Applicatives, and Monads in Haskell

## 1. Introduction
*   **The Journey**: Starting with a note that walking through this exercise of constructing the "minimal" instances is one of the best ways to deeply understand Functors, Applicatives, and Monads in Haskell. It strips away the domain-specific noise, demystifying a lot of features that initially look like magic.
*   **The Narrative**: Noting that while these core concepts are foundational to modern Haskell, this specific teaching narrative—starting with absolute minimalism to "prove" the forced hand of parametricity—is unique, synthesizing ideas from papers like Wadler's "Theorems for Free!" and books like Maguire's "Thinking with Types".
*   **The Scope**: Establishing that we are focusing specifically on Endofunctors operating within the category of Haskell types (from `Hask` to `Hask`).
    *   **Note on `Hask`**: Mention that `Hask` is technically not a strict category due to non-terminating programs (bottom, `_|_`). Include a link to the famous paper ["Fast and Loose Reasoning is Morally Correct"](https://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.pdf).
*   **The Power of Parametricity**: Noting that due to parametricity, the implementation of most functor, applicative, and monad instances is forced to be mathematically unique. This provides immense "intellectual economy" for Haskell developers: operations like `bind`, `pure`, `fmap`, `apply` (`<*>`), and Kleisli composition (`>=>`) generally have only one possible correct implementation for simple structural types.
    *   *Counterexamples*: Add a very brief note acknowledging rare counterexamples where multiple valid implementations might exist (e.g., variations in ordering/traversal), but confirm they are atypical for minimal types.

## 2. The Foundations of Functors, Applicatives, and Monads

### 2.1. What is a Functor?
*   **The Original Definition**: Starting with the original definition from Category Theory (referencing Saunders Mac Lane).
*   **Programming Ubiquity**: Noting that functors are widely used in programming languages (even non-functional ones) without developers always knowing it.
*   **Haskell's Category Theory Context**: Highlighting that a Haskell `Functor` is an *Endofunctor* mapping from `Hask` to `Hask`.
    *   **Examples of Non-Haskell Categorical Functors**:
        1.  *Non-Endofunctor*: Mapping from Sets to `Hask`.
        2.  *Non-parametric Functor*: Inspects types (forbidden in Haskell).
        3.  *Restricted Functor*: Applies only to a subset of types.
*   **Taxonomy**: Differences between `type`, `newtype`, and `data`.

### 2.2. The Constraint of Parametricity
*   **The Concept**: How `fmap` generically handling *all* types forces it to preserve structure.
*   **Theorems for Free**: Linking to Wadler's paper.

### 2.3. `fmap` (and `<$>`) and the Functor Laws
*   **The Signature**: `(a -> b) -> f a -> f b`. Lifting function application into a context.
*   **The Laws**: Identity and Composition. 
*   **Enforcement**: Use of `QuickCheck` for automated verification.

### 2.4. The Applicative Functor
*   **The Context**: Functor equipped with `pure` and `apply` (`<*>`).
*   **The Laws**: Briefly listing Applicative laws.

### 2.5. The Monad
*   **Three Equivalent Paths**: `bind`, `join` (`mu`), and `kleisli` composition (`>=>`).

---

## 3. The Minimal Functor/Monad (Proxy)
*(Zero computational data, Zero contextual data)*

### 3.1. The Smallest Valid Candidate
*   **The Definition**: `data MinF a = Val`.
*   **Haskell Equivalents**: `Proxy` or `Const ()`.

### 3.2. A Singular Functor Implementation
*   **The Code**: `fmap _ Val = Val`.
*   **The "Why"**: The type signature forces our hand.

### 3.3. Upgrading to Applicative
*   **The Code**: `pure _ = Val` and `Val <*> Val = Val`.

### 3.4. Upgrading to Monad
*   **Via `join`**: `join Val = Val`.
*   **Via `bind`**: `Val >>= _ = Val`.

### 3.5. Let the Compiler Do the Work (`deriving`)
*   Using `DeriveFunctor`.

---

## 4. The Minimal Applicative Functor (Const)
*(Zero computational data, Some contextual data `r`). Requires `Monoid r`.*

### 4.1. The Definition of `Const`
*   `newtype Const r a = Const r`.

### 4.2. A Singular Functor Implementation
*   `fmap _ (Const r) = Const r`.

### 4.3. The Applicative Twist (Necessary and Sufficient)
*   Why `Monoid r` is strictly necessary for `pure` (identity) and `<*>` (combination).

### 4.4. Why Not a Monad?
*   Failing the Left Identity law.

---

## 5. The Minimal Synchronous Monad (Identity)
*(One computational data, Zero contextual data)*

### 5.1. The Definition
*   `data IdF a = IdVal a`.

### 5.2. A Singular Functor Implementation
*   `fmap f (IdVal x) = IdVal (f x)`.

### 5.3. Upgrading to Applicative
*   `pure x = IdVal x` and `IdVal f <*> IdVal x = IdVal (f x)`.

### 5.4. Upgrading to Monad
*   `join (IdVal (IdVal x)) = IdVal x`.
*   `IdVal x >>= f = f x`.

---

## 6. The Algebra of Functors

*   **Sums (Maybe)**: $1 + X$.
*   **Products (Writer)**: $r \times X$.
*   **Fundamental Rule**: `Proxy` represents the number 1.
*   **Recursion (List)**: $L(X) = 1 + X \times L(X)$.

---

## 7. Conclusion: The Tale of Three Minimals
*   **Comparison**: Zero (`MinF`), Accumulation (`Const`), and One (`Identity`).

## 8. Annex & Bibliography
*   Proofs of Monad equivalence and unique Functor identity.
