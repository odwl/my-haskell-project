# Minimal Haskell Project

This is a minimal starter template for a Haskell project using **Cabal**. It demonstrates a simple directory structure with separated source code and tests.

## 📂 Project Structure

```text
my-haskell-project/ 
├── my-haskell-project.cabal   # Build configuration
├── src/
│   └── Lambda.hs              # Main library logic
└── test/
    └── LambdaTest.hs          # Test suite.

🛠 How to Run
Open your terminal in the project root.

1. Build the Project
Downloads dependencies and compiles the code.

Bash 
cabal build
2. Interactive REPL
Loads the library in GHCi for interactive use.

Bash
cabal repl
3. Run the Tests
Runs the test suite defined in test/LambdaTest.hs.

Bash
cabal test