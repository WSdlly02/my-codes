use super::{
    api,
    protocol::{RUNTIME_DIR, SOCKET_FILE},
    runtime::Runtime,
};
use anyhow::{Context, Result, ensure};
use clap::Parser;
use std::{fs, future::IntoFuture, os::unix::fs::PermissionsExt, path::PathBuf, time::Duration};
use tokio::{
    net::UnixListener,
    signal::unix::{SignalKind, signal},
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
    /// 所有 watch 共享的名额读取间隔
    #[arg(long, default_value = "5s", value_parser = humantime::parse_duration)]
    poll: Duration,
}

pub(crate) async fn run() -> Result<()> {
    let args = Args::parse();
    ensure!(args.poll >= Duration::from_secs(1), "--poll 至少 1s");
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

    let runtime = Runtime::start(args.poll)?;
    let server = tokio::spawn(
        axum::serve(listener, api::router(runtime.clone()))
            .with_graceful_shutdown(shutdown.clone().cancelled_owned())
            .into_future(),
    );
    shutdown.cancelled().await;
    // Ends intents first, so requests waiting on them can answer before the server stops.
    runtime.shutdown().await;
    server.await??;
    let _ = fs::remove_file(SOCKET_FILE);
    Ok(())
}
