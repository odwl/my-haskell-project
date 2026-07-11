use rust::{greet, fib_fold};

fn main() {
    println!("{}", greet("world"));
    let x = 10;
    println!("x: {x}");
    print_type("x", &x);
    println!("result: {}", interproduct(120, 100, 248));
    let x = 10;
    let y = 20;
    
    print_type("x", &x);
    takes_u32(x);
    print_type("y", &y);
    takes_i8(y);
    // takes_u32(y);
    time_it("fibFold(100)", || fib_fold(100));
    let z = 13;
    let x = {
        let y = 10;
        print_type("y", &y);
        z - y
    };
    print_type("x", &x);
    print_type("y", &y);
}

fn time_it<F, T: std::fmt::Display>(label: &str, mut f: F)
where
    F: FnMut() -> T,
{
    let iters = 10_000;
    let res = f();
    println!("{label}: {res}");

    let start = std::time::Instant::now();
    for _ in 0..iters {
        std::hint::black_box(f());
    }
    let elapsed = start.elapsed();
    let avg_ns = elapsed.as_nanos() as f64 / iters as f64;
    println!("Average over {iters} runs: {avg_ns:.2} ns per calculation");
}

#[track_caller]
fn print_type<T: std::fmt::Debug + ?Sized>(name: &str, val: &T) {
    let loc = std::panic::Location::caller();
    println!(
        "[{}:{}:{}] {name} = {:?} (Type: {})",
        loc.file(),
        loc.line(),
        loc.column(),
        val,
        std::any::type_name_of_val(val)
    );
}

fn interproduct(a: i32, b: i32, c: i32) -> i32 {
    a * b + b * c + c * a
}

fn takes_u32(x: u32) {
    println!("u32: {x}");
}

fn takes_i8(y: i8) {
    println!("i8: {y}");
}
