# Agent Handoff — my-haskell-project

> Generated: 2026-04-11. Branch: `dev`.

## Goal

Get `make build` passing, with LiquidHaskell active as a GHC plugin, so that the project's
refinement-type annotations in `src/Lambda/SandBox.hs` are actually checked at compile time.

---

## Current State of Play

### What works
- The project structure, source, and tests are all in place and committed on `dev`.
- `cabal.project.local` pins GHC 9.6.3 and includes an `allow-newer: liquidhaskell:bytestring` bypass.
- `liquidhaskell == 0.9.6.3` is declared as a dependency and **GHC is configured to load it as a plugin** (`-fplugin=LiquidHaskell`) in the library stanza.
- The workaround shim `~/.cabal/bin/ld.gold` (a tiny shell script that just calls `exec ld "$@"`) was created in a previous session to satisfy Cabal's `-fuse-ld=gold` flag.

### What is broken — the linker error

Every `make build` run fails with:

```
error: collect2: fatal error: cannot find 'ld'
compilation terminated.
```

Root cause: GHC / Cabal passes `-fuse-ld=gold` to gcc during C-linking phases (e.g., building `hsc2hs` helpers for `basement`, `clock`, `zlib`, `network`, etc.).  
`ld.gold` is **not installed** on this machine:

```
$ ls /usr/bin/ld.gold   → does not exist
$ dpkg -l binutils-gold → un (not installed)
```

The `~/.cabal/bin/ld.gold` shim script exists and reads:
```sh
#!/bin/sh
exec ld "$@"
```

However it is **not being picked up** because `~/.cabal/bin` is not on the PATH that the
GHC/cabal C compilation sub-invocations see — they use GCC directly, which looks for
`ld.gold` in its own hard-coded search path (typically `/usr/bin`, `/usr/local/bin`).

### What has been tried (did NOT fix it)

1. Placing `ld.gold` shim in `~/.cabal/bin` — does not help because GCC ignores that directory.
2. `allow-newer: liquidhaskell:bytestring` in `cabal.project.local` — resolves the dep-conflict
   but does not fix the linker issue.
3. `extra-lib-dirs: .local-lib` in `cabal.project.local` — was for a different workaround.

---

## How to Fix the Build

### Option A — Install `binutils-gold` (simplest, if sudo available)

```bash
sudo apt-get install -y binutils-gold
```

This installs the real `ld.gold` at `/usr/bin/ld.gold`. The build should then succeed.

### Option B — Place the shim in a GCC-searched location (if no sudo)

```bash
mkdir -p ~/.local/bin
cat > ~/.local/bin/ld.gold << 'EOF'
#!/bin/sh
exec ld "$@"
EOF
chmod +x ~/.local/bin/ld.gold
export PATH="$HOME/.local/bin:$PATH"
```

Then add to `~/.bashrc` / `~/.profile`:
```bash
export PATH="$HOME/.local/bin:$PATH"
```

And in `cabal.project.local` tell Cabal about the extra program path:
```
extra-prog-path: /home/odwl/.local/bin
```

### Option C — Tell cabal to use `lld` or `bfd` instead of `gold`

Add to `cabal.project.local`:
```
package *
  ghc-options: -optl-fuse-ld=bfd
```

Or globally in `~/.cabal/config` under `program-default-options`:
```
  ghc-options: -optl-fuse-ld=bfd
```

---

## Project Structure

```
my-haskell-project/
├── src/
│   └── Lambda/
│       └── SandBox.hs          ← NEW: halve, halve', halve'' with LiquidHaskell specs
├── test/
│   └── Lambda/
│       └── SandBoxTest.hs      ← NEW: QuickCheck tests including deliberate halve' vs halve'' comparison
├── Makefile
├── my-haskell-project.cabal
├── cabal.project.local         ← pins GHC 9.6.3 + allow-newer bypass
└── .ghcid                      ← runs `cabal repl all-tests`
```

## Key Files

### `cabal.project.local` (current)
```cabal
with-compiler: ghc-9.6.3

package *
  extra-lib-dirs: /usr/local/google/home/odwl/Documents/dev/my-haskell-project/.local-lib

allow-newer:
  liquidhaskell:bytestring
```

### `my-haskell-project.cabal` — library stanza (key parts)
```cabal
library
  ghc-options: -fplugin=LiquidHaskell --prune-unsorted
  build-depends:
    liquidhaskell == 0.9.6.3,
    ...
```

### LiquidHaskell annotations in `src/Lambda/SandBox.hs`
```haskell
{-@ type EvenList a = {v:[a] | (len v) mod 2 == 0} @-}

{-@ halve :: EvenList a -> ([a], [a]) @-}
halve :: [a] -> ([a], [a])
halve list = splitAt (length list `div` 2) list

{-@ halve' :: EvenList a -> ([a], [a]) @-}
halve' :: [a] -> ([a], [a])
halve' [] = ([], [])
halve' (x : y : xs) = let (l, r) = halve' xs in (x : l, y : r)
halve' [_] = error "List must have even length"
```

`halve''` is **not yet implemented** — it is imported in `SandBoxTest.hs` (`import Lambda.SandBox (halve, halve', halve'')`) so the build will also fail with a missing export unless you add it. It should be an "outer-inner" split (first half = elements at even indices, second half = elements at odd indices, matching `halve`'s output). You can add it as:

```haskell
{-@ halve'' :: EvenList a -> ([a], [a]) @-}
halve'' :: [a] -> ([a], [a])
halve'' xs = (evens xs, odds xs)
  where
    evens []       = []
    evens (a:_:rest) = a : evens rest
    evens [_]      = error "odd length"
    odds []        = []
    odds (_:b:rest) = b : odds rest
    odds [_]       = error "odd length"
```

---

## Toolchain Versions

| Tool | Version |
|------|---------|
| GHC  | 9.6.7 (detected via `ghc --version`), but cabal.project.local pins **9.6.3** |
| cabal-install | 3.14.2.0 |
| LiquidHaskell | 0.9.6.3 (declared dep, installed via cabal) |
| liquidhaskell binary | not on PATH (not needed — used as GHC plugin only) |

> **Note:** There is a mismatch: `ghc --version` says 9.6.7 but `cabal.project.local` says
> `with-compiler: ghc-9.6.3`. Confirm `ghc-9.6.3` is actually installed via ghcup:
> `ghcup list | grep ghc`. If only 9.6.7 is available, update `cabal.project.local` accordingly.

---

## TDD Loop (once build works)

```bash
# Fast feedback on SandBox only:
make watch-sandbox

# All tests:
make test

# Full check (format + lint + test):
make check
```

## Expected Test Behaviour

- `halve` tests: should all pass (splitAt is correct).
- `halve' vs halve''` test: **expected to fail** — `halve'` alternates elements (interleave split) while `halve''` should match `halve` (contiguous split). The test is intentionally checking that they differ on lists longer than 2.
- `halve'` tests: should pass (length equality and element preservation).
- `halve''` tests: should pass (matches `halve` exactly).
