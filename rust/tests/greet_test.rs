use proptest::prelude::*;
use rust::{greet, fib, fib_fold};

#[test]
fn test_greet_world() {
    assert_eq!(greet("world"), "Hello, world!");
}

#[test]
fn test_greet_custom() {
    assert_eq!(greet("Rust"), "Hello, Rust!");
}

#[test]
fn test_fib_100() {
    assert_eq!(fib(100), 354_224_848_179_261_915_075);
}

#[test]
fn test_fib_fold_100() {
    assert_eq!(fib_fold(100), 354_224_848_179_261_915_075);
}

proptest! {
    #[test]
    fn prop_greet_invariants(name in "\\PC*") {
        let greeting = greet(&name);
        prop_assert!(greeting.starts_with("Hello, "));
        prop_assert!(greeting.ends_with("!"));
        prop_assert!(greeting.contains(&name));
        prop_assert_eq!(greeting.len(), name.len() + 8);
    }

    #[test]
    fn prop_fib_equals_fib_fold(n in 0..100u32) {
        prop_assert_eq!(fib(n), fib_fold(n));
    }
}
