mod browser;
mod cache;
mod cli;
mod commands;
mod http;
mod output;
mod parser;
mod support;

pub fn run() -> anyhow::Result<()> {
    commands::run()
}
