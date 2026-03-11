.PHONY: all build test lint format check watch

# Build the project
build:
	cabal build

# Run all tests
test:
	cabal test

# Run hlint on source and test directories
lint hlint:
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
	rm -f docs/*.aux docs/*.log docs/*.out docs/*.toc docs/*.synctex.gz

# Build documentation
docs:
	@which pdflatex > /dev/null || (echo "pdflatex not found. Please install a LaTeX distribution." && exit 1)
	for f in docs/*.tex; do pdflatex -output-directory=docs $$f; done
