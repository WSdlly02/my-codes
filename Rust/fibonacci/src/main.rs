use std::io;
use std::process;

fn main() {
    let mut n = String::new();
    let mut fibonacci: u32;
    let mut fibonacci_n_1: u32 = 1;
    let mut fibonacci_n_2: u32 = 0;

    println!("请输入要计算的斐波那契数列阶数:");
    io::stdin().read_line(&mut n).expect("Failed to read line");

    let mut n: u32 = match n.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            println!("错误:n不是一个自然数");
            process::exit(1);
        }
    };

    let n_immu = n;

    if n == 0 {
        fibonacci = 0;
        println!("第{n_immu}阶斐波那契数为{fibonacci}");
    } else if n == 1 {
        fibonacci = 1;
        println!("第{n_immu}阶斐波那契数为{fibonacci}");
    } else {
        loop {
            fibonacci = fibonacci_n_1 + fibonacci_n_2;
            fibonacci_n_2 = fibonacci_n_1;
            fibonacci_n_1 = fibonacci;
            if n == 2 {
                println!("第{n_immu}阶斐波那契数为{fibonacci}");
                break;
            };
            n -= 1;
        }
    }
}
