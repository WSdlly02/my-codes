mod api;
mod cache;
mod cli;
mod daemon;
mod http;
mod intent;
mod login;
mod output;
mod parser;
mod protocol;
mod runtime;
mod support;

pub async fn run() -> anyhow::Result<()> {
    cli::run().await
}

pub async fn run_daemon() -> anyhow::Result<()> {
    daemon::run().await
}
