use std::io;
use std::process;

fn fib(i: i32, n: i32, m: i32) -> i32 {
    if i == 0 {
        return n;
    } else {
        return fib(i - 1, m, n + m);
    }
}
fn main() {
    let mut step_str = String::new();
    println!("请输入要计算的斐波那契数列阶数:");
    io::stdin()
        .read_line(&mut step_str)
        .expect("Failed to read line");

    let step: i32 = match step_str.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            println!("错误:n不是一个自然数");
            process::exit(1)
        }
    };
    println!("第{}阶斐波那契数为{}", step, fib(step, 0, 1));
}
