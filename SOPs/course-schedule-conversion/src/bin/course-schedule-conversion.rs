use std::fs;

use anyhow::{Context, Result};
use clap::Parser;

use course_schedule_conversion::calendar::generate_ics;
use course_schedule_conversion::config::Config;
use course_schedule_conversion::schedule::parse_class_schedule_html;

fn main() -> Result<()> {
    let config = Config::parse();
    let html = fs::read_to_string(&config.html_path)
        .with_context(|| format!("读取 HTML 文件失败: {}", config.html_path.display()))?;
    let lessons = parse_class_schedule_html(&html)?;
    let ics = generate_ics(&lessons, &config)?;

    let output_path = config.output_path();
    fs::write(&output_path, ics)
        .with_context(|| format!("写入 ICS 文件失败: {}", output_path.display()))?;

    println!("课程表已成功转换为 {}", output_path.display());
    Ok(())
}
