use std::io;
use std::process;

fn main() {
    let mut input_method = String::new();
    let mut input_temperature = String::new();
    println!("输入转换类型所对应的数字:");
    println!("1.摄氏度->华氏度");
    println!("2.华氏度->摄氏度");
    io::stdin()
        .read_line(&mut input_method)
        .expect("Failed to read line");
    println!("输入温度:");
    io::stdin()
        .read_line(&mut input_temperature)
        .expect("Failed to read line");
    let input_method: u8 = input_method.trim().parse().expect("Please type a number!");
    let input_temperature: f32 = input_temperature
        .trim()
        .parse()
        .expect("Please type a number!");
    let output_temperature: f32;
    if input_method == 1 {
        output_temperature = input_temperature * 1.8 + 32.0;
    } else if input_method == 2 {
        output_temperature = (input_temperature - 32.0) * 5.0 / 9.0;
    } else {
        process::exit(1);
    }
    println!("转换后的温度是{output_temperature}度");
}
