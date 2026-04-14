mod calendar;
mod config;
mod model;
mod schedule;

use std::{fs, process::Command};

use anyhow::{Context, Result, bail};
use clap::Parser;

use crate::calendar::generate_ics;
use crate::config::Config;
use crate::schedule::parse_class_schedule_html;

fn main() -> Result<()> {
    let config = Config::parse();
    let html = load_schedule_html(&config)?;
    let lessons = parse_class_schedule_html(&html)?;
    let ics = generate_ics(&lessons, &config)?;

    let output_path = config.output_path();
    fs::write(&output_path, ics)
        .with_context(|| format!("写入 ICS 文件失败: {}", output_path.display()))?;

    println!("课程表已成功转换为 {}", output_path.display());
    Ok(())
}

fn load_schedule_html(config: &Config) -> Result<String> {
    if let Some(html_file) = &config.html_file {
        return fs::read_to_string(html_file)
            .with_context(|| format!("读取 HTML 文件失败: {}", html_file.display()));
    }

    let output = Command::new(&config.course_election_bin)
        .arg("query")
        .arg("--class-schedule")
        .current_dir(resolve_command_workdir(&config.course_election_bin))
        .output()
        .with_context(|| {
            format!(
                "执行外部命令失败: {} query --class-schedule",
                config.course_election_bin.display()
            )
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_owned();
        if stderr.is_empty() {
            bail!("外部命令执行失败，退出码 {:?}", output.status.code());
        }
        bail!("外部命令执行失败: {stderr}");
    }

    String::from_utf8(output.stdout).context("外部命令输出不是有效 UTF-8")
}

fn resolve_command_workdir(command_path: &std::path::Path) -> &std::path::Path {
    command_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
}
