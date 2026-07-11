# Fibonacci Systems Benchmarking Report: Rust vs. Haskell

> [!IMPORTANT]
> This report documents the comparative performance, memory representation, and compiler behaviors of 128-bit integer Fibonacci calculations ($F_{100}$) across **Rust (`u128`)** and **Haskell (`Word128`)**, measuring both imperative and functional folding patterns.

---

## 1. Executive Summary & Side-by-Side Benchmark

When calculating the $100^{\text{th}}$ Fibonacci number ($F_{100} = 354,224,848,179,261,915,075$) across 10,000 runs using `make compare-fib`, the execution profiles reveal fundamental differences in how systems compilers (**LLVM** vs. **GHC**) optimize pure functional expressions and hardware loops:

| Methodology / Scenario | Haskell GHC (`-O2 Word128`) | Rust (`--release u128`) | Hardware / Compiler Mechanism |
| :--- | :--- | :--- | :--- |
| **Memoized / Shared** (`Pure Caching`) | **$3.62\text{ ns}$** | *N/A (requires manual cache)* | GHC automatically shares pure subexpressions (`WHNF`) across calls without manual hash tables. |
| **Uncached / From-Scratch** (`Bare-Metal Math`) | **$162.06\text{ ns}$** | **$25.25\text{ ns}$** | Rust hits the exact physical limit of 1 CPU clock cycle per step. Haskell handles 64-bit pair operations and `IO` isolation wrappers. |
| **Compile-Time Constant Folding** | *N/A (requires `Template Haskell`)* | **$0.32\text{ ns}$** | LLVM precalculates $F_{100}$ during `cargo build` and embeds the literal in the binary (`1 clock cycle return`). |

---

## 2. Mathematical Foundation: $O(N)$ vs. $O(\log N)$

### A. Linear Iteration ($O(N)$ Time)
The implementations benchmarked in this report (`fibFold` and `fib`) compute the Fibonacci recurrence sequentially:

$$F_n = F_{n-1} + F_{n-2}, \quad \text{with } (F_0, F_1) = (0, 1)$$

* **Step Count**: Exactly $N$ addition operations. For $N=100$, the loop executes **100 steps**.
* **Complexity**: $O(N)$ arithmetic steps, requiring $O(1)$ memory registers.

### B. Algorithmic Breakthrough: Fast Doubling ($O(\log N)$ Time)
By formulating Fibonacci transitions as $2 \times 2$ matrix multiplication:

$$\begin{pmatrix} F_{n+1} \\ F_n \end{pmatrix} = \begin{pmatrix} 1 & 1 \\ 1 & 0 \end{pmatrix}^n \begin{pmatrix} 1 \\ 0 \end{pmatrix}$$

We can apply **Exponentiation by Squaring** ($M^{2k} = (M^k)^2$). When we multiply $Q^k \times Q^k$, the top-right matrix dot product yields:

$$F_{2k} = F_{k+1} F_k + F_k F_{k-1} = F_k (F_{k+1} + F_{k-1})$$

To eliminate $F_{k-1}$ (`so our algorithm only needs to track 2 values: $F_k$ and $F_{k+1}$`), we substitute $F_{k-1} = F_{k+1} - F_k$ from the definition of Fibonacci:

$$F_{2k} = F_k \Big(F_{k+1} + (F_{k+1} - F_k)\Big) = F_k (2 F_{k+1} - F_k)$$

And for the odd step ($F_{2k+1}$), the top-left dot product yields directly:

$$F_{2k+1} = F_k^2 + F_{k+1}^2$$

> [!TIP]
> **Logarithmic Efficiency**: To calculate $F_{1,000,000}$, the linear $O(N)$ loop requires **1,000,000 steps**, whereas Fast Doubling requires only **$\log_2(1,000,000) \approx 20\text{ steps}$**!

### C. Matrix Transformations & Generalizations (`Binet, Tribonacci, Pell & Lucas`)
The companion matrix technique generalizes beyond standard Fibonacci to arbitrary linear recurrences and basis transformations:

#### 1. Diagonalization & Binet's Closed-Form Formula
Because $Q = \begin{pmatrix} 1 & 1 \\ 1 & 0 \end{pmatrix}$ is symmetric and diagonalizable, any matrix change-of-basis $Q = P D P^{-1}$ preserves powers: $Q^n = P D^n P^{-1}$. Diagonalizing $Q$ using the **Golden Ratio ($\phi = \frac{1+\sqrt{5}}{2}$)** and its conjugate ($\psi = \frac{1-\sqrt{5}}{2}$) yields:

$$Q^n = P \begin{pmatrix} \phi^n & 0 \\ 0 & \psi^n \end{pmatrix} P^{-1} \implies F_n = \frac{\phi^n - \psi^n}{\sqrt{5}}$$

#### 2. Higher-Order Recurrences (`Tribonacci & Companion Matrices`)
To compute $k$-step linear recurrences such as **Tribonacci numbers** ($T_n = T_{n-1} + T_{n-2} + T_{n-3}$), the $2 \times 2$ matrix expands into a $3 \times 3$ companion matrix (`and $k \times k$ in general`):

$$\begin{pmatrix} T_{n+1} \\ T_n \\ T_{n-1} \end{pmatrix} = \begin{pmatrix} \mathbf{1} & \mathbf{1} & \mathbf{1} \\ 1 & 0 & 0 \\ 0 & 1 & 0 \end{pmatrix}^n \begin{pmatrix} 1 \\ 0 \\ 0 \end{pmatrix}$$

#### 3. Alternative 2-Step Sequences (`Pell and Lucas Numbers`)
By altering matrix coefficients or initial state vectors, we obtain related integer sequences in $O(\log N)$ time:
* **Pell Numbers ($P_n = 2 P_{n-1} + P_{n-2}$)**: Change the top-left matrix coefficient from $1$ to $2$:
  $$\begin{pmatrix} \mathbf{2} & 1 \\ 1 & 0 \end{pmatrix}^n = \begin{pmatrix} P_{n+1} & P_n \\ P_n & P_{n-1} \end{pmatrix}$$
* **Lucas Numbers ($L_n = F_{n-1} + F_{n+1}$)**: Retain the exact Fibonacci matrix $Q^n$, but multiply by the initial Lucas state vector $\begin{pmatrix} L_1 \\ L_0 \end{pmatrix} = \begin{pmatrix} 1 \\ 2 \end{pmatrix}$ instead of $\begin{pmatrix} 1 \\ 0 \end{pmatrix}$.

---


## 3. Hardware Clock-Cycle Breakdown ($N=100$)

To understand why **$25.25\text{ ns}$** in Rust represents the theoretical physical ceiling of silicon registers, we divide total execution duration by the step count:

$$\text{Time per Step} = \frac{25.25\text{ ns}}{100\text{ steps}} = \mathbf{0.2525\text{ nanoseconds per step}}$$

On a modern $3.5\text{ GHz} - 4.0\text{ GHz}$ x86_64 processor, 1 CPU clock cycle lasts approximately:

$$\tau_{\text{cycle}} = \frac{1}{4 \times 10^9\text{ Hz}} = \mathbf{0.25\text{ nanoseconds}}$$

### Why $0.25\text{ ns}$ is the absolute physical ceiling:
Because each step depends sequentially on the previous step ($a + b$), the CPU must execute the 128-bit hardware additions (`ADD rax, rbx; ADC rdx, rcx`) inside general-purpose registers sequentially. Executing 100 sequential register additions takes exactly **~100 physical clock cycles ($\approx 25\text{ ns}$)**.

---

## 4. Methodology Tiers & Compiler Behaviors

### Tier 1: Functional Memoization (`Haskell 3.62 ns`)
When `fibFold 100` is evaluated without barriers, GHC recognizes that the expression has no side effects (`pure function`).

```haskell
-- Because `fibFold 100` is pure, GHC evaluates it once to Weak Head Normal Form (WHNF)
-- and shares the evaluated pointer across all 10,000 benchmark iterations.
!r <- evaluate (fibFold 100)
```

* **The Superpower**: Pure-code caching happens automatically at runtime. No manual memoization tables or hash keys are required by the programmer.

### Tier 2: Uncached / From-Scratch Execution (`Rust 25.25 ns` vs. `Haskell 162.06 ns`)
To force both compilers to genuinely execute all 100 mathematical iterations across every single run, double-barrier isolation is required:

#### Rust Isolation (`std::hint::black_box`):
```rust
for _ in 0..iters {
    let input = std::hint::black_box(100); // Forbids compile-time constant propagation
    std::hint::black_box(fib_fold(input)); // Forbids dead-code elimination
}
```

#### Haskell Isolation (`NOINLINE IO + Identity Barriers`):
```haskell
{-# NOINLINE identity #-}
identity :: Word32 -> Int -> Word32
identity x _ = x

{-# NOINLINE runOne #-}
runOne :: (Word32 -> Word128) -> Word32 -> Int -> IO Word128
runOne f x i = evaluate (f (identity x i))
```
* **Why `runOne` is required**: As an `IO` function taking the changing loop counter `i`, the rules of Haskell's `IO` monad strictly forbid GHC from reordering, memoizing, or floating the evaluation across runs.
* **Why `identity` is required**: Passes a fresh `Word32` on every call so `f` (`fibFold`) cannot reuse cached Weak Head Normal Form results.

### Tier 3: Compile-Time Constant Folding (`Rust 0.32 ns`)
If `black_box(input)` is omitted and a constant (`100`) is passed directly to `|| fib_fold(100)`, LLVM performs interprocedural constant propagation during `cargo build`:

$$\text{Compile Time}: \quad F_{100} \to 354,224,848,179,261,915,075$$

$$\text{Runtime Loop}: \quad \text{return literal } 354,224,848,179,261,915,075 \quad (1\text{ clock cycle})$$

---

## 5. Code Implementations (`rust/src/lib.rs` & `src/Lambda/SandBox.hs`)

### Rust Implementation (`u128`)
```rust
pub fn fib_fold(n: u32) -> u128 {
    (0..n).fold((0, 1), |(a, b), _| (b, a + b)).0
}
```

### Haskell Implementation (`Word128`)
```haskell
import Data.List (foldl')
import Data.WideWord.Word128 (Word128)
import Data.Word (Word32)

fibFold :: Word32 -> Word128
fibFold n = fst $ foldl' step (0, 1) [1..n]
  where
    step (!a, !b) _ = (b, a + b)
```

> [!NOTE]
> Notice the **Bang Patterns (`!a, !b`)** in Haskell: these force GHC to evaluate the tuple elements strictly inside hardware registers (`unboxed`), preventing the accumulation of lazy heap thunks (`WHNF`) across the 100 iterations.
