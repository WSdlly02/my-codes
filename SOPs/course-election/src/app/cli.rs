use super::{
    cache,
    intent::Phase,
    output::{self, LessonQueryFilter},
    protocol::{Command, Maintenance, Mode, Response, SOCKET_FILE, Spec},
    support,
};
use anyhow::{Context, Result, bail, ensure};
use chrono::{DateTime, FixedOffset};
use clap::{Args, Parser, Subcommand};
use serde::Serialize;
use std::{path::PathBuf, time::Duration};

/// How often `job wait` and `logs --follow` poll the daemon.
const POLL: Duration = Duration::from_millis(250);

#[derive(Parser)]
#[command(
    name = "course-election",
    version,
    about = "离线查询与选课 daemon 客户端；不管理 daemon 生命周期"
)]
struct Cli {
    #[arg(long, global = true, default_value = ".")]
    data_dir: PathBuf,
    #[arg(long, global = true)]
    json: bool,
    #[command(subcommand)]
    command: Action,
}

#[derive(Subcommand)]
enum Action {
    Status,
    Jobs,
    Profile {
        id: String,
        #[arg(long)]
        force: bool,
    },
    Login {
        username: String,
        #[arg(long)]
        password_stdin: bool,
    },
    Logout {
        #[arg(long)]
        force: bool,
    },
    Job {
        #[command(subcommand)]
        action: JobAction,
    },
    Fire(Submit),
    Drop(Submit),
    Watch(Submit),
    Arm(Submit),
    Prepare,
    Refresh,
    Channels {
        #[arg(long)]
        refresh: bool,
    },
    Selected {
        #[arg(long, conflicts_with = "profile")]
        refresh: bool,
        #[arg(long, required_unless_present = "refresh")]
        profile: Option<String>,
    },
    Find(Find),
    ExportSchedule {
        #[arg(long)]
        semester: Option<String>,
        #[arg(long)]
        output: Option<PathBuf>,
    },
    Cache {
        #[command(subcommand)]
        action: CacheAction,
    },
    Logs {
        #[arg(long)]
        follow: bool,
    },
}

#[derive(Subcommand)]
enum CacheAction {
    Clear,
    Status,
}

#[derive(Subcommand)]
enum JobAction {
    /// 核对 unknown 意图的当前已选状态；不会重发写请求。
    Reconcile {
        id: u64,
    },
    Show {
        id: u64,
    },
    Wait {
        id: u64,
    },
    Pause {
        id: u64,
    },
    Resume {
        id: u64,
    },
    Cancel {
        id: u64,
    },
}

#[derive(Args)]
struct Submit {
    #[arg(long)]
    lesson: String,
    #[arg(long, value_parser = humantime::parse_duration)]
    interval: Option<Duration>,
    #[arg(long, value_parser = humantime::parse_duration)]
    timeout: Option<Duration>,
    #[arg(long)]
    attempts: Option<u64>,
    #[arg(long)]
    at: Option<String>,
    #[arg(long)]
    dry_run: bool,
    #[arg(long)]
    wait: bool,
}

#[derive(Args)]
struct Find {
    name: Option<String>,
    #[arg(long)]
    profile: String,
    #[arg(long, conflicts_with = "code")]
    id: Option<String>,
    #[arg(long)]
    code: Option<String>,
    #[arg(long)]
    selected: bool,
}

#[derive(Serialize)]
struct FoundLessons<'a> {
    profile: &'a str,
    mapping_at: DateTime<FixedOffset>,
    counts_at: Option<DateTime<FixedOffset>>,
    lessons: &'a [output::LessonDisplayEntry],
}

fn print(value: &impl Serialize, json: bool) -> Result<()> {
    let text = if json {
        serde_json::to_string(value)?
    } else {
        serde_json::to_string_pretty(value)?
    };
    println!("{text}");
    Ok(())
}

async fn rpc(command: Command) -> Result<Response> {
    let response = reqwest::Client::builder()
        .unix_socket(SOCKET_FILE)
        .build()?
        .post("http://daemon/v1/command")
        .json(&command)
        .send()
        .await
        .context(
            "与 daemon 通信失败：确认 course-electiond 已用相同 --data-dir 启动；\
             已接受的意图仍在后台，请查询状态而非重复提交",
        )?;
    if !response.status().is_success() {
        bail!("{}", response.text().await?);
    }
    Ok(response.json().await?)
}

/// Waits for Ctrl-C or the poll interval; true on Ctrl-C.
async fn pause_or_interrupt() -> bool {
    tokio::select! {
        _ = tokio::time::sleep(POLL) => false,
        _ = tokio::signal::ctrl_c() => true,
    }
}

async fn wait_job(id: u64, json: bool) -> Result<()> {
    loop {
        let Response::Job(job) = rpc(Command::Job { id }).await? else {
            bail!("响应类型错误")
        };
        if !job.phase.active() || job.phase == Phase::Paused {
            print(&job, json)?;
            ensure!(job.phase == Phase::Succeeded, "意图已停止：{:?}", job.phase);
            return Ok(());
        }
        if pause_or_interrupt().await {
            bail!("停止等待；后台意图未取消");
        }
    }
}

async fn follow_logs(follow: bool, json: bool) -> Result<()> {
    let mut sequence = 0;
    loop {
        let Response::Logs { events, .. } = rpc(Command::Logs).await? else {
            bail!("响应类型错误")
        };
        if events
            .first()
            .is_some_and(|e| sequence != 0 && e.sequence > sequence + 1)
        {
            eprintln!("日志缓冲有缺口；jobs 是权威状态");
        }
        for event in events {
            if event.sequence > sequence {
                sequence = event.sequence;
                print(&event, json)?;
            }
        }
        if !follow || pause_or_interrupt().await {
            return Ok(());
        }
    }
}

async fn submit(args: Submit, mode: Mode, json: bool) -> Result<()> {
    let watch = mode == Mode::Watch;
    let millis = |duration: Duration| u64::try_from(duration.as_millis()).context("时间溢出");
    let default_interval = Duration::from_millis(if watch { 5000 } else { 500 });
    let default_timeout = Duration::from_secs(if watch { 1800 } else { 0 });
    let default_attempts = match mode {
        Mode::Watch => 0,
        Mode::Arm => 2,
        Mode::Fire | Mode::Drop => 1,
    };
    let spec = Spec {
        lesson: args.lesson,
        interval_ms: millis(args.interval.unwrap_or(default_interval))?,
        timeout_ms: millis(args.timeout.unwrap_or(default_timeout))?,
        attempts: args.attempts.unwrap_or(default_attempts),
        at_ms: args
            .at
            .map(|s| chrono::DateTime::parse_from_rfc3339(&s).map(|d| d.timestamp_millis()))
            .transpose()?,
        dry_run: args.dry_run,
        mode,
    };
    let response = rpc(Command::Add { spec }).await?;
    if args.wait
        && let Response::Job(job) = response
    {
        return wait_job(job.id, json).await;
    }
    print(&response, json)
}

fn find(args: Find, json: bool) -> Result<()> {
    let mapping = cache::load_mapping_cache(&args.profile)?;
    let counts = cache::load_count_snapshot(&args.profile).ok();
    let selected_lesson_ids = if args.selected {
        cache::load_selected_snapshot(&args.profile)?.selected
    } else {
        Default::default()
    };
    let filter = LessonQueryFilter {
        name: args.name,
        lesson_id: args.id,
        code: args.code,
        selected_only: args.selected,
        selected_lesson_ids,
    };
    let entries = output::build_lesson_display_entries(&mapping, counts.as_ref(), &filter);
    if json {
        let found = FoundLessons {
            profile: &args.profile,
            mapping_at: mapping.fetched_at,
            counts_at: counts.as_ref().map(|c| c.fetched_at),
            lessons: &entries,
        };
        return print(&found, true);
    }
    println!(
        "课程缓存：{}；容量缓存：{:?}",
        mapping.fetched_at,
        counts.as_ref().map(|c| c.fetched_at)
    );
    for (i, e) in entries.iter().enumerate() {
        print!("{}", output::format_lesson_display_entry(i + 1, e));
    }
    Ok(())
}

pub(crate) async fn run() -> Result<()> {
    let cli = Cli::parse();
    std::env::set_current_dir(&cli.data_dir)?;
    let json = cli.json;
    let command = match cli.command {
        Action::Status => Command::Status,
        Action::Jobs => Command::Jobs,
        Action::Profile { id, force } => Maintenance::Profile { id, force }.into(),
        Action::Login {
            username,
            password_stdin,
        } => {
            let password = if password_stdin {
                let mut line = String::new();
                std::io::stdin().read_line(&mut line)?;
                line.trim_end_matches(['\r', '\n']).to_owned()
            } else {
                rpassword::prompt_password("密码：")?
            };
            Maintenance::Login { username, password }.into()
        }
        Action::Logout { force } => Maintenance::Logout { force }.into(),
        Action::Job { action } => match action {
            JobAction::Show { id } => Command::Job { id },
            JobAction::Reconcile { id } => Maintenance::Reconcile { id }.into(),
            JobAction::Pause { id } => Command::Pause { id },
            JobAction::Resume { id } => Command::Resume { id },
            JobAction::Cancel { id } => Command::Cancel { id },
            JobAction::Wait { id } => return wait_job(id, json).await,
        },
        Action::Fire(args) => return submit(args, Mode::Fire, json).await,
        Action::Drop(args) => return submit(args, Mode::Drop, json).await,
        Action::Watch(args) => return submit(args, Mode::Watch, json).await,
        Action::Arm(args) => return submit(args, Mode::Arm, json).await,
        Action::Prepare => Maintenance::Prepare.into(),
        Action::Refresh => Maintenance::Refresh.into(),
        Action::Channels { refresh: true } => Maintenance::Channels.into(),
        Action::Channels { refresh: false } => return print(&cache::load_channel_cache()?, json),
        Action::Selected { refresh: true, .. } => Maintenance::Selected.into(),
        Action::Selected { profile, .. } => {
            let profile = profile.context("需要 --profile")?;
            return print(&cache::load_selected_snapshot(&profile)?, json);
        }
        Action::Find(args) => return find(args, json),
        Action::ExportSchedule { semester, output } => {
            let semester = support::resolve_semester_id(semester.as_deref())?;
            let path = output.unwrap_or_else(|| format!("class-schedule-{semester}.html").into());
            let Response::Exported { html, .. } =
                rpc(Maintenance::Export { semester }.into()).await?
            else {
                bail!("响应类型错误")
            };
            std::fs::write(&path, html)?;
            return print(&serde_json::json!({ "output": path }), json);
        }
        Action::Cache {
            action: CacheAction::Clear,
        } => Maintenance::ClearCache.into(),
        Action::Cache {
            action: CacheAction::Status,
        } => {
            let status = serde_json::json!({
                "mappings": cache::list_mapping_cache_statuses()?,
                "counts": cache::list_count_cache_statuses()?,
            });
            return print(&status, json);
        }
        Action::Logs { follow } => return follow_logs(follow, json).await,
    };
    print(&rpc(command).await?, json)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clap_schema() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }

    #[test]
    fn rejects_lifecycle_commands() {
        assert!(Cli::try_parse_from(["cli", "daemon", "stop"]).is_err());
    }

    #[test]
    fn rejects_unknown_options() {
        assert!(Cli::try_parse_from(["cli", "status", "--force"]).is_err());
    }
}
