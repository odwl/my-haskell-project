use rust::greet;

#[test]
fn test_greet_world() {
    assert_eq!(greet("world"), "Hello, world!");
}

#[test]
fn test_greet_custom() {
    assert_eq!(greet("Rust"), "Hello, Rust!");
}
