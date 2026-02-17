# Minimal Haskell Project

This is a minimal starter template for a Haskell project using **Cabal**. It demonstrates a simple directory structure with separated source code and tests.

## 📂 Project Structure

```text
my-haskell-project/
├── my-haskell-project.cabal   # Build configuration
├── src/
│   └── HelloWorld.hs          # Main library logic
└── test/
    └── HelloWorld_Test.hs     # Test suite

🛠 How to Run
Open your terminal in the project root.

1. Build the Project
Downloads dependencies and compiles the code.

Bash
cabal build
2. Run the Application
Executes the main program defined in src/HelloWorld.hs.

Bash
cabal run my-program
3. Run the Tests
Runs the test suite defined in test/HelloWorld_Test.hs.

Bash
cabal test