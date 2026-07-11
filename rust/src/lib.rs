pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

pub fn fib(n: u32) -> u128 {
    let (mut a, mut b): (u128, u128) = (0, 1);
    for _ in 0..n {
        (a, b) = (b, a + b);
    }
    a
}

use std::num::Wrapping;

pub fn fib_fold(n: u32) -> u128 {
    (0..n).fold((Wrapping(0), Wrapping(1)), |(fk, fk1), _| (fk1, fk + fk1)).0 .0
}

// Best approach in Rust (pure recursion). 
// Avoids Haskell's call-stack overhead because LLVM unrolls the recursion directly into hardware registers.
pub fn fib_log(n: u32) -> u128 {
    fn go(k: u32) -> (Wrapping<u128>, Wrapping<u128>) {
        if k == 0 {
            return (Wrapping(0), Wrapping(1));
        }
        let (fk, fk1) = go(k / 2);
        let f2k = fk * ((fk1 + fk1) - fk);
        let f2k1 = fk * fk + fk1 * fk1;
        if k % 2 == 0 { (f2k, f2k1) } else { (f2k1, f2k + f2k1) }
    }
    go(n).0 .0
}

pub fn fib_log_fold(n: u32) -> u128 {
    if n == 0 {
        return 0;
    }
    let msb = u32::BITS - 1 - n.leading_zeros();
    (0..=msb) 
        .rfold((Wrapping(0), Wrapping(1)), |(fk, fk1), i| {
            let bit = (n & (1 << i)) != 0;
            let f2k = fk * ((fk1 + fk1) - fk);
            let f2k1 = fk * fk + fk1 * fk1;
            if bit { (f2k1, f2k + f2k1) } else { (f2k, f2k1) }
        })
        .0 .0
}
