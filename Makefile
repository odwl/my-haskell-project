.PHONY: all build test lint format check watch

# Build the project
build:
	cabal build

# Run all tests
test:
	cabal test

# Run hlint on source and test directories
lint:
	hlint src test

# Format all Haskell files in src and test directories
format:
	ormolu --mode inplace $$(find src test -name '*.hs')

# Run format, lint, and test sequentially
check: format lint test

# Interactive REPL
repl:
	cabal repl

# Continuous feedback loop (requires ghcid)
# Reads parsing options from .ghcid if present
watch:
	ghcid

# Run tests on file change (fast TDD loop)
watch-test:
	ghcid --command="cabal repl lambda-test" --test=":main"

# Clean build artifacts
clean:
	cabal clean
