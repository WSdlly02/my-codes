use std::process::Stdio;

use anyhow::{Context, Result, bail};
use tokio::{io::AsyncWriteExt, process::Command};

pub async fn run_yq(mode: &str, input: &[u8], expression: &str, args: &[&str]) -> Result<Vec<u8>> {
    let mut command = Command::new("yq");
    command.arg(mode).arg("-o=json").arg(expression).args(args);
    command.stdin(Stdio::piped());
    command.stdout(Stdio::piped());
    command.stderr(Stdio::piped());

    let mut child = command
        .spawn()
        .with_context(|| format!("failed to start yq {mode}"))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(input)
            .await
            .with_context(|| format!("failed to write stdin to yq {mode}"))?;
    }

    let output = child
        .wait_with_output()
        .await
        .with_context(|| format!("failed to wait for yq {mode}"))?;

    if !output.status.success() {
        bail!(
            "yq {mode} failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }

    Ok(output.stdout)
}
