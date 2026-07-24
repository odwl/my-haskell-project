.PHONY: all build test lint format check watch docs watch-sandbox watch-exercism watch-lambda watch-zipper watch-free run-lean watch-lean watch-limit run-haskell-fib run-storable run-haskell-fib-llvm compare-fib run-rust run-rust-release build-rust build-rust-release check-rust test-rust watch-rust watch-rust-test watch-countdown run-countdown

# Load local environment variables
-include .env

# Haskell Toolchain Setup
GHCUP_BIN := $(HOME)/.ghcup/bin
export PATH := $(HOME)/.cargo/bin:$(HOME)/.elan/bin:$(GHCUP_BIN):$(HOME)/.cabal/bin:$(PATH)
export LIBRARY_PATH := $(CURDIR)/.local-lib:$(LIBRARY_PATH)


# Build the project
build:
	cabal build

# Run all tests (Haskell and Rust)
test: test-rust
	cabal test


# Run Lean Hello World
run-lean:
	lean --run lean/Main.lean

# Run Haskell Fibonacci benchmark executable with GHC Native Code Generator (-O2)
run-haskell-fib:
	cabal run -O2 fib-main

# Run RequiredTypeArguments & Storable demonstration
run-storable:
	cabal run storable-main

# Run Haskell Fibonacci benchmark executable with GHC LLVM Backend (-O2 -fllvm)
run-haskell-fib-llvm:
	cabal run -O2 --ghc-options="-fllvm" fib-main

# Compare Fibonacci performance across 10,000 runs: Haskell Memoized vs Haskell Uncached vs Rust Uncached vs Haskell Fast Doubling
compare-fib:
	@echo "=========================================================="
	@echo "  1. Haskell GHC (-O2 Word128: Memoized / Shared)         "
	@echo "=========================================================="
	@cabal run -v0 -O2 fib-main -- --memoized
	@echo ""
	@echo "=========================================================="
	@echo "  2. Haskell GHC (-O2 Word128: Uncached / From-Scratch)   "
	@echo "=========================================================="
	@cabal run -v0 -O2 fib-main -- --uncached
	@echo ""
	@echo "=========================================================="
	@echo "  3. Rust (--release u128: Uncached / From-Scratch)       "
	@echo "=========================================================="
	@cargo run -q --release --manifest-path rust/Cargo.toml --bin fib
	@echo ""
	@echo "=========================================================="
	@echo "  4. Haskell GHC (-O2 Word128: Fast Doubling O(log N))    "
	@echo "=========================================================="
	@cabal run -v0 -O2 fib-main -- --fibLog







# Watch Lean Hello World for changes
watch-lean:
	@echo "Watching lean/Main.lean for changes..."
	@LAST_MOD=""; \
	while true; do \
		MOD=$$(stat -c %Y lean/Main.lean 2>/dev/null); \
		if [ "$$MOD" != "$$LAST_MOD" ]; then \
			clear; \
			echo "lean/Main.lean changed. Re-running..."; \
			$(MAKE) run-lean; \
			LAST_MOD=$$MOD; \
		fi; \
		sleep 1; \
	done

# Run Rust project (debug)
run-rust:
	cargo run --manifest-path rust/Cargo.toml

# Run Rust project in release mode (maximum optimization)
run-rust-release:
	cargo run --release --manifest-path rust/Cargo.toml

# Check Rust project for errors without building binary
check-rust:
	cargo check --manifest-path rust/Cargo.toml

# Run Rust project tests
test-rust:
	cargo test --manifest-path rust/Cargo.toml

# Build Rust project (debug)
build-rust:
	cargo build --manifest-path rust/Cargo.toml

# Build Rust project in release mode
build-rust-release:
	cargo build --release --manifest-path rust/Cargo.toml

# Watch Rust project for changes and re-run
watch-rust:
	@echo "Watching rust/src for changes..."
	@LAST_MOD=""; \
	while true; do \
		MOD=$$(find rust/src -type f -exec stat -c %Y {} + 2>/dev/null | sort -nr | head -n1); \
		if [ "$$MOD" != "$$LAST_MOD" ]; then \
			clear; \
			echo "rust/src changed. Re-running..."; \
			$(MAKE) run-rust; \
			LAST_MOD=$$MOD; \
		fi; \
		sleep 1; \
	done

# Watch Rust project for changes and re-run tests
watch-rust-test:
	@echo "Watching rust/src and rust/tests for changes..."
	@LAST_MOD=""; \
	while true; do \
		MOD=$$(find rust/src rust/tests -type f -exec stat -c %Y {} + 2>/dev/null | sort -nr | head -n1); \
		if [ "$$MOD" != "$$LAST_MOD" ]; then \
			clear; \
			echo "Rust files changed. Running tests..."; \
			$(MAKE) test-rust; \
			LAST_MOD=$$MOD; \
		fi; \
		sleep 1; \
	done


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

# Run all Lambda tests on file change
watch-lambda:
	ghcid --command="cabal repl lambda-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "Lens Tests"
watch-lens:
	TASTY_PATTERN="Lens Tests" ghcid --command="cabal repl lambda-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "SandBox Tests"
watch-sandbox:
	TASTY_PATTERN="SandBox" ghcid --command="cabal repl lambda-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "Free, Cofree and Coyoneda Tests"
watch-free:
	TASTY_PATTERN="Free" ghcid --command="cabal repl lambda-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "Limit and Colimit Tests"
watch-limit:
	TASTY_PATTERN="Limit and Colimit Tests" ghcid --command="cabal repl lambda-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "CountDown Tests"
watch-countdown:
	TASTY_PATTERN="CountDown" ghcid --command="cabal repl all-tests" --test=':main' --restart=src --reload=test

# Run CountDown main solver (compiled with -O2 for maximum speed)
run-countdown:
	cabal exec -- ghc -O2 -isrc -main-is CountDown.main src/CountDown.hs -o /tmp/countdown-opt && /tmp/countdown-opt


# Run tests on file change but isolate only "Exercism" tests
watch-exercism:
	ghcid --command="cabal repl exercism-test" --test=':main' --restart=src --reload=test

# Run tests on file change but isolate only "Zipper" tests
watch-zipper:
	TASTY_PATTERN="Zipper Tests" ghcid --command="cabal repl exercism-test" --test=':main' --restart=src --reload=test


# Clean build artifacts
clean:
	cabal clean
	cargo clean --manifest-path rust/Cargo.toml
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



