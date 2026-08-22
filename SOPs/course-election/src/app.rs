mod cache;
mod http;
mod login;
mod output;
mod parser;
mod repl;
mod support;

pub async fn run() -> anyhow::Result<()> {
    repl::run().await
}
