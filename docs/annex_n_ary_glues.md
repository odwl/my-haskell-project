# Annex: Deconstructing the Universe into N-ary Glues

This document captures a profound conversation regarding the true, irreducible minimal foundations of Algebraic Data Types (ADTs) in Haskell, moving beyond 2-ary Bifunctors to discover the ultimate minimal generating set of the type universe.

## 1. The Standard 5-Piece Axiomatic Universe
Normally, to construct the universe of Polynomial Functors (ADTs), we rely on five fundamental axioms:
*   **3 Atoms**: `Zero` ($0$), `Proxy` ($1$), and `Identity` ($X$)
*   **2 Glues (2-ary)**: `Either` ($+$) and `(,)` ($\times$)

While 2-ary glues can generate any larger arity by induction (e.g., nesting tuples to make larger tuples), they **cannot build downwards**. You cannot mathematically manipulate 2-ary `Either` and `(,)` to organically produce the $0$ and $1$ atoms. Therefore, in a strictly 2-ary system, the atoms must be injected manually as axioms.

## 2. Deriving Atoms via Laws
If we define our 2-ary glues and simply demand that they possess an **Identity Law**, the atoms are naturally deduced:
*   $A + 0 = A \implies 0$ must be defined as the uninstantiable type (`Void` / `Zero`).
*   $A \times 1 = A \implies 1$ must be defined as the single-state empty type (`()` / `Proxy`).

However, this still requires us to explicitly write out the $0$ and $1$ types to satisfy the equations. Can we define a minimal set where we don't even need to write the atoms at all?

## 3. The Power of N-ary Glues (`[*] -> *`)
To organically generate the atoms, we must upgrade our glues from strictly 2-ary to **N-ary**. An N-ary glue accepts a list of Types and returns a single Type.

What happens if we try to imagine the absolute simplest mathematical N-ary glues possible?

### The Trivial Glues (Generates exactly 1 Atom)
The absolute simplest operations simply ignore their inputs:
*   **`NaryZero` (The Black Hole)**: Swallows $N$ types, holds 0 constructors.
    *   `NaryZero '[]` generates the **$0$ atom**.
    *   *Closure*: Flat. It only ever generates $0$.
*   **`NaryProxy` (The Empty Box)**: Swallows $N$ types, holds 1 empty constructor.
    *   `NaryProxy '[]` generates the **$1$ atom**.
    *   *Closure*: Flat. It only ever generates $1$.

### The Projection Fake-Glues (Generates nothing)
What if a glue tries to extract a specific type from the list, like `FirstOnly`?
*   **`FirstOnly '[]`**: **CRASH!** It mathematically demands the list has at least one element.
    *   Because it crashes on $N=0$, it cannot bootstrap a universe. It is a partial function that destroys data and requires the universe to already exist.

## 4. The Meaningful Glues: The True Generator Set
If a glue cannot ignore its inputs (useless flat closure) and cannot selectively extract them (crashes on $0$), its only mathematically sound option is to **perfectly preserve all inputs**.

There are exactly two valid ways to preserve a list of types:

### A. The N-ary Sum: `Union`
You preserve exactly one type by forcing a choice.
```haskell
data Union (ts :: [*]) where
    This :: t -> Union (t ': ts)
    That :: Union ts -> Union (t ': ts)
```
*   **$N = 2$**: `Union '[A, B]` (Evaluates exactly to `Either A B`).
*   **$N = 1$**: `Union '[A]` (Evaluates exactly to `Identity A`).
*   **$N = 0$**: `Union '[]` (Logically impossible to instantiate. Evaluates exactly to `Zero` / the $0$ Atom!).

### B. The N-ary Product: `HList`
You preserve all types by holding them simultaneously.
```haskell
data HList (ts :: [*]) where
    HNil  :: HList '[]                     
    HCons :: t -> HList ts -> HList (t ': ts)  
```
*   **$N = 2$**: `HList '[A, B]` (Evaluates exactly to `(A, B)`).
*   **$N = 1$**: `HList '[A]` (Evaluates exactly to `Identity A`).
*   **$N = 0$**: `HList '[]` (Trivially holding 0 items. Evaluates exactly to `Proxy` / the $1$ Atom!).

## 5. The Ultimate Proof
By utilizing N-ary glues instead of 2-ary glues, the base cases ($N=0$ and $N=1$) organically resolve directly into our foundational atoms! We have mathematically eliminated the need for axiomatic atoms. 

If you take exactly two abstract concepts:
1.  **`Union`**
2.  **`HList`**

And endlessly nest them (the formal closure), you strictly and perfectly map the entire mathematical space of **Polynomial Functors** in Haskell. You cannot generate functions (`->`), and you cannot generate outside the ADT framework. 

The entirety of Algebraic Data Types is mathematically just the infinite closure of `Union` and `HList`!
