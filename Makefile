.PHONY: all build test lint format check watch docs watch-sandbox

# Load local environment variables
-include .env

# Haskell Toolchain Setup
GHCUP_BIN := $(HOME)/.ghcup/bin
export PATH := $(GHCUP_BIN):$(HOME)/.cabal/bin:$(PATH)
export LIBRARY_PATH := $(CURDIR)/.local-lib:$(LIBRARY_PATH)


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
	ghcid --test="main"

# Run tests on file change (fast TDD loop)
watch-test:
	ghcid --command="cabal repl lambda-test" --test=":main" --restart=src --reload=test

# Run tests on file change but isolate only "Lens Tests"
watch-lens:
	ghcid --command="cabal repl lambda-test" --test=':main -p "Lens Tests"' --restart=src --reload=test

# Run tests on file change but isolate only "SandBox Tests"
watch-sandbox:
	ghcid --command="cabal repl lambda-test" --test=':main -p SandBox' --restart=src --reload=test


# Clean build artifacts
clean:
	cabal clean
	rm -f docs/*.aux docs/*.log docs/*.out docs/*.toc docs/*.synctex.gz

# Build documentation
docs:
	@which pdflatex > /dev/null || (echo "pdflatex not found. Please install a LaTeX distribution." && exit 1)
	for f in docs/*.tex; do \
		pdflatex -interaction=nonstopmode -output-directory=docs $$f; \
		pdflatex -interaction=nonstopmode -output-directory=docs $$f; \
	done
	@if [ ! -z "$(SYNC_DEST)" ]; then $(MAKE) sync; FROM_DOCS=1; fi

# Sync PDF to remote destination
sync:
	@if [ -z "$(SYNC_DEST)" ]; then echo "Error: SYNC_DEST is not set. Usage: make sync SYNC_DEST=user@host:path"; exit 1; fi
	@echo "Syncing PDFs to $(SYNC_DEST)..."
	scp docs/*.pdf $(SYNC_DEST)



