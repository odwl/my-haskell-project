use rust::{fib_fold, fib_log, fib_log_fold};

fn main() {
    println!("--- Rust Fibonacci Timing (--release across 10,000 runs) ---");
    time_func("fibFold(1,000,000)     [O(N)]", fib_fold, 1_000_000);
    time_func("fibLog(1,000,000)      [O(log N)] (Recursive)", fib_log, 1_000_000);
    time_func("fibLogFold(1,000,000)  [O(log N)] (Fold)", fib_log_fold, 1_000_000);
}

fn time_func<F>(label: &str, mut f: F, n: u32)
where
    F: FnMut(u32) -> u128,
{
    let iters = 10_000;
    // Warmup print
    let res = f(n);
    println!("{label}: {res}");

    let start = std::time::Instant::now();
    for _ in 0..iters {
        // black_box(n) forbids LLVM from constant-folding f(100) at compile time!
        let input = std::hint::black_box(n);
        std::hint::black_box(f(input));
    }
    let elapsed = start.elapsed();
    let avg_ns = elapsed.as_nanos() as f64 / iters as f64;
    println!("Average over {iters} runs: {avg_ns:.2} ns per calculation");
}
