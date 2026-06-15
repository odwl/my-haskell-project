# Task: Enhancements for Haskell Project

## Status
- [ ] Implement `Data.Map.Strict` for Interpreter State
- [ ] Refactor `myDiv` to avoid re-computation
- [ ] Introduce Sum Type for Interpreter Values
- [ ] Add Makefile for Quality Checks

## Details

### 1. Use Strict Maps for Interpreter State
**Goal**: Prevent space leaks by switching to strict map evaluation in the interpreter.
- **File**: `src/Lambda/Interpreter.hs`
- **Action**: Change import to `import qualified Data.Map.Strict as Map`.

### 2. Refactor `myDiv`
**Goal**: Clean up the logic in `myDiv` to be more idiomatic and efficient.
- **File**: `src/Lambda/Functor.hs`
- **Action**: Use `let` binding or `Control.Monad (guard)` to avoid computing `div a b` twice.

### 3. Type-Safe Interpreter Values
**Goal**: Improve type safety by distinguishing between Integer and Boolean values in the interpreter.
- **File**: `src/Lambda/Interpreter.hs`
- **Action**:
    - Define a new `Value` data type: `data Value = VInt Int | VBool Bool`.
    - Update `evalAExp`, `evalExp`, and `evalStmt` to handle these wrapped values.
    - Implement error handling for type mismatches (e.g., adding a boolean to an integer) using `Either` or `ExceptT`.

### 4. Add Makefile for Quality Checks
**Goal**: Simplify the development workflow by adding a standard entry point for linting, formatting, and testing.
- **File**: `Makefile` (new file in root)
- **Action**: Create a `Makefile` with targets for:
    - `check`: Runs formatting check (ormolu), linting (hlint), and tests.
    - `format`: Automatically formats code with ormolu.
    - `lint`: Runs hlint.
    - `test`: Runs all tests.
