use super::{
    api,
    protocol::{RUNTIME_DIR, SOCKET_FILE},
    runtime,
};
use anyhow::{Context, Result};
use clap::Parser;
use std::{fs, os::unix::fs::PermissionsExt, path::PathBuf};
use tokio::{
    net::UnixListener,
    signal::unix::{SignalKind, signal},
    sync::mpsc,
};
use tokio_util::sync::CancellationToken;

#[derive(Parser)]
#[command(
    name = "course-electiond",
    about = "选课后台：前台运行，使用 SIGINT/SIGTERM 优雅退出"
)]
struct Args {
    #[arg(long, default_value = ".")]
    data_dir: PathBuf,
}

pub(crate) async fn run() -> Result<()> {
    let args = Args::parse();
    std::env::set_current_dir(&args.data_dir).context("打开数据目录失败")?;
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .with_writer(std::io::stderr)
        .init();

    fs::create_dir_all(RUNTIME_DIR)?;
    fs::set_permissions(RUNTIME_DIR, fs::Permissions::from_mode(0o700))?;
    let lock = fs::File::create(format!("{RUNTIME_DIR}/daemon.lock"))?;
    lock.try_lock().context("该数据目录已有 daemon 正在运行")?;
    // Holding the lock, any existing socket is a leftover from a crash.
    let _ = fs::remove_file(SOCKET_FILE);
    let listener = UnixListener::bind(SOCKET_FILE)?;

    let shutdown = CancellationToken::new();
    let mut terminate = signal(SignalKind::terminate())?;
    let on_signal = shutdown.clone();
    tokio::spawn(async move {
        tokio::select! {
            _ = tokio::signal::ctrl_c() => {}
            _ = terminate.recv() => {}
        }
        on_signal.cancel();
    });

    let (sender, receiver) = mpsc::channel(64);
    let stopped = shutdown.clone().cancelled_owned();
    let server = tokio::spawn(async move {
        axum::serve(listener, api::router(sender))
            .with_graceful_shutdown(stopped)
            .await
    });
    let result = runtime::run(receiver, shutdown.clone()).await;
    shutdown.cancel();
    server.await??;
    let _ = fs::remove_file(SOCKET_FILE);
    result
}
