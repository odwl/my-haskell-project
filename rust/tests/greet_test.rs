use proptest::prelude::*;
use rust::greet;

#[test]
fn test_greet_world() {
    assert_eq!(greet("world"), "Hello, world!");
}

#[test]
fn test_greet_custom() {
    assert_eq!(greet("Rust"), "Hello, Rust!");
}

proptest! {
    #[test]
    fn prop_greet_invariants(name in "\\PC*") {
        let greeting = greet(&name);
        
        // 1. Prefix and Suffix Invariants
        prop_assert!(greeting.starts_with("Hello, "));
        prop_assert!(greeting.ends_with("!"));
        
        // 2. Containment Invariant
        prop_assert!(greeting.contains(&name));
        
        // 3. Length Invariant ("Hello, " is 7 bytes, "!" is 1 byte => total 8 extra bytes)
        prop_assert_eq!(greeting.len(), name.len() + 8);
    }
}

