# Haskell Functional Programming Exercises

This project is a comprehensive collection of Haskell exercises focused on core functional programming abstractions, including **Functors**, **Applicatives**, **Monads**, **State**, and **Parsers**.

## 📂 Project Structure

- **src/Lambda/**
    - `Functor.hs`: Implementation of `MaybeList`, `MyMaybe`, and `MyReader`. Includes a **non-deterministic Hover Dam** state machine using the `MaybeList` monad.
        - *Inspiration: This section was inspired by the exercises at [Quantum Logic 2021 - Monads](https://lmf.di.uminho.pt/quantum-logic-2021/LQ-Monads.pdf).*
    - `State.hs`: Custom implementation of the State Monad.
        - *Inspiration: This section was inspired by the [Monads Lab](https://shaagerup.github.io/dm552/files/MonadsLab.pdf) exercise.*
    - `Parser.hs`: A robust parser implemented using **MegaParsec**.
        - *Inspiration: This section was inspired by [Exercise 5](https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ex/ex5.pdf) from the Functional Programming course at Uni Freiburg.*
    - `MiniWhile.hs`: An interpreter for a minimal "While" language.
        - *Inspiration: This section was inspired by [Exercise 6](https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ex/ex6.pdf) from the Functional Programming course at Uni Freiburg.*
- **src/Exercism/**
    - A collection of solved exercises from the [Exercism Haskell Track](https://exercism.org/tracks/haskell).
    - [Anagram](https://exercism.org/tracks/haskell/exercises/anagram)
    - [Bob](https://exercism.org/tracks/haskell/exercises/bob)
    - [Pangram](https://exercism.org/tracks/haskell/exercises/pangram)
    - [Reverse String](https://exercism.org/tracks/haskell/exercises/reverse-string)
- **test/Lambda/**
    - `FunctorTest.hs`: Law-based tests for `MaybeList` and other functors using `Checkers`, alongside extensive QuickCheck properties verifying robust Hover Dam state transitions (using `MaybeT`).
    - `ParserTest.hs`: Comprehensive test suite for the MegaParsec parser.
    - `StateTest.hs`: Tests for the custom State Monad.
- **test/Exercism/**
    - Automated test suites for all Exercism exercises (Anagram, Bob, Pangram, Reverse String).

## � Categorical Foundations

The Functor, Applicative, and Monadic structures implemented in this project closely follow the mathematical laws and derivations taught in **"Category Theory for Programmers"** by Bartosz Milewski. You can find the PDF version of the book here:
[Category Theory for Programmers (Milewski 2023)](https://ai.dmi.unibas.ch/research/reading_group/milewski-2023-01-30.pdf).

## �🚀 Key Feature: `MaybeList`

The `MaybeList` type (`[Maybe a]`) has been carefully implemented to strictly adhere to all Functor, Applicative, and Monad laws. We achieved this by leveraging the **`MaybeT`** monad transformer and the **`DerivingVia`** extension.

- **Architecture**: `MaybeList` is a newtype over `[Maybe a]`, deriving its logic directly from `MaybeT []`.
- **Performance**: The law-based test suite for `MaybeList` executes in under **0.3 seconds** by optimizing QuickCheck generation sizes.

## 🌊 The Hover Dam Simulation

One of the more complex exercises in this project is the **Hover Dam** state machine. It uses the `MaybeList` monad to model **non-deterministic** outcomes.

- **Non-Determinism**: At the dam's capacity, `carEnters` branches the universe into two possible futures: one where the car enters safely (`Just (n+1)`) and one where the dam collapses (`Nothing`).
- **3-Act Property Testing**: We verify the simulation's robustness with a unique 3-act QuickCheck property:
    1. **Survival**: A random safe walk starting from `damOpens` always maintains a single healthy timeline.
    2. **Bifurcation**: Pushing the dam to its limit triggers a precise bifurcation into `[Just 4, Nothing]`.
    3. **Collapse**: One more entry at the limit results in "death all around", where all timelines collapse: `[Nothing, Nothing]`.
- **Diagnostic Reporting**: The property tests use `counterexample` to provide detailed diagnostic traces upon any invariant violation.

## 🛠 How to Run

1. **Build the Project**
   ```bash
   cabal build
   ```
2. **Run All Tests**
   ```bash
   cabal run lambda-test
   ```
3. **Interactive REPL**
   ```bash
   cabal repl
   ```

## 🧪 Testing Framework

We use a robust testing stack:
- **Tasty**: The main test runner for organizing test groups.
- **QuickCheck**: For property-based testing across randomized inputs.
- **Checkers**: To specifically verify Functor, Applicative, and Monad laws automatically.