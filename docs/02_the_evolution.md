# The Minimal Haskell Series: Part 2 - The Evolution

## Chapter 2: The Applicative Evolution

Now we step up in power. An `Applicative` is a Functor equipped with two new powers: `pure` (to lift values) and `<*>` (to lift application).

### Section 2.1: The Applicative Atoms

Let's see how our atomic structures "upgrade" to this new level.

#### 1. `Proxy`
```haskell
instance Applicative Proxy where
    pure _ = Proxy
    Proxy <*> Proxy = Proxy
```
**The "Why"**: Our hands are tied. `pure` gives us an `a`, which we must discard (as `Proxy` holds no data). `<*>` combines two empty boxes into one.

#### 2. `Const r` (The Monoid Requirement)
This is the most critical upgrade in the minimal universe.
```haskell
instance Monoid r => Applicative (Const r) where
    pure _ = Const mempty
    Const r1 <*> Const r2 = Const (r1 `mappend` r2)
```
**The "Why"**: 
*   `pure` requires us to produce an `r` out of nothing. We must use the **Identity element** (`mempty`).
*   `<*>` gives us two `r` values and needs one result. We must use the **Binary operation** (`mappend`).
This precisely defines why `Const` requires its context to be a `Monoid` to achieve Applicative status.

#### 3. `Identity`
```haskell
instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)
```
**The "Why"**: Trivial application. We unwrap, apply, and rewrap.

### Section 2.2: Automated Law Testing

Just as with Functors, we can verify our Applicative instances using `tasty-checkers`. This is where the library truly shines, as the number of Applicative laws (Identity, Homomorphism, Interchange, and Composition) is significantly higher:

```haskell
  -- Automatically tests all Applicative laws
  testBatch (applicative (undefined :: Maybe (Int, String, Int)))
```

---

## Chapter 3: The Monadic Conclusion

The `Monad` adds the power of **Context-Dependent Sequencing** via `bind` (`>>=`) or `join`.

### Section 3.1: The Final Upgrades

#### 1. `Proxy`
```haskell
instance Monad Proxy where
    Proxy >>= _ = Proxy
```
Flattening an empty box inside an empty box still yields an empty box.

#### 2. `Identity`
```haskell
instance Monad Identity where
    Identity x >>= f = f x
```
Pure function application.

#### 3. `Const r` (The Monad Barrier)
**Crucially, `Const r` cannot be a Monad.** 
```haskell
(>>=) :: Const r a -> (a -> Const r b) -> Const r b
```
Because `Const` contains no `a`, we can never execute the function `(a -> Const r b)`. We completely lose whatever `r` value the function *would* have produced, violating the **Left Identity law** (`pure a >>= f == f a`). The evolution stops here.

### Section 3.2: Automated Law Testing

Finally, we can verify our Monad instances (Left Identity, Right Identity, and Associativity) with a single check:

```haskell
  -- Automatically tests all Monad laws
  testBatch (monad (undefined :: Maybe (Int, String, Int)))
```

---
## Conclusion: The Tale of Three Minimals

By starting from these absolute minimal examples, the "magic" evaporates, leaving the elegant logic of types and the algebraic discovery of everything from `Maybe` to `List`.

---

---


---

