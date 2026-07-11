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
    (0..n).fold((Wrapping(0u128), Wrapping(1u128)), |(a, b), _| (b, a + b)).0 .0
}

pub fn fib_log(n: u32) -> u128 {
    fn go(k: u32) -> (Wrapping<u128>, Wrapping<u128>) {
        if k == 0 {
            return (Wrapping(0), Wrapping(1));
        }
        let (a, b) = go(k / 2);
        let c = a * (Wrapping(2) * b - a); // a * (2*b - a)
        let d = a * a + b * b;             // a^2 + b^2
        if k % 2 == 0 {
            (c, d)
        } else {
            (d, c + d)
        }
    }
    go(n).0 .0
}

pub fn fib_log_fold(n: u32) -> u128 {
    if n == 0 {
        return 0;
    }
    let msb = u32::BITS - 1 - n.leading_zeros();
    (0..=msb)
        .rev()
        .fold((Wrapping(0u128), Wrapping(1u128)), |(a, b), i| {
            let bit = (n & (1 << i)) != 0;
            let f2k = a * (Wrapping(2) * b - a);
            let f2k1 = a * a + b * b;
            if bit {
                (f2k1, f2k + f2k1)
            } else {
                (f2k, f2k1)
            }
        })
        .0 .0
}
