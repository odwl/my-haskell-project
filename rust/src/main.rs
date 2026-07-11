use rust::greet;

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
    let start = std::time::Instant::now();
    let res = fib(47);
    println!("fib(47): {res} (calculated in {:?})", start.elapsed());

    let start_fold = std::time::Instant::now();
    let res_fold = fib_fold(47);
    println!("fib_fold(47): {res_fold} (calculated in {:?})", start_fold.elapsed());
}

fn print_type<T: ?Sized>(name: &str, val: &T) {
    println!("Type of {name}: {}", std::any::type_name_of_val(val));
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

fn fib(n: u32) -> u64 {
    let (mut a, mut b): (u64, u64) = (0, 1);
    for _ in 0..n {
        (a, b) = (b, a + b);
    }
    a
}

fn fib_fold(n: u32) -> u64 {
    (0..n).fold((0, 1), |(a, b), _| (b, a + b)).0
}
