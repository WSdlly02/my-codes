use super::{
    cache,
    output::{self, LessonQueryFilter},
    protocol::{Command, Maintenance, Phase, Response, SOCKET_FILE, Spec, Trigger},
    render, support,
};
use anyhow::{Context, Result, bail, ensure};
use chrono::{DateTime, FixedOffset};
use clap::{Args, Parser, Subcommand};
use serde::Serialize;
use std::{path::PathBuf, time::Duration};

/// How often `logs --follow` polls the daemon.
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
    /// 每行输出一个 JSON 值，供脚本和程序解析
    #[arg(long, global = true)]
    json: bool,
    #[command(subcommand)]
    command: Action,
}

#[derive(Subcommand)]
enum Action {
    Status,
    Jobs,
    /// 打开该 profile 的选课页面；取消其他 profile 的意图。
    Profile {
        id: String,
    },
    Login {
        username: String,
        #[arg(long)]
        password_stdin: bool,
    },
    /// 取消全部意图并清除会话。
    Logout,
    Job {
        #[command(subcommand)]
        action: JobAction,
    },
    /// 到点（缺省为立即）直接提交选课，不看容量。
    Fire(Fire),
    /// 到点（缺省为立即）提交退课。
    Drop(Fire),
    /// 名额读取出现空位时提交选课。
    Watch(Watch),
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
    Show {
        id: u64,
    },
    /// 等待意图结束；Ctrl-C 只停止等待。
    Wait {
        id: u64,
    },
    /// 停止等待；已交给执行器的提交仍会完成。
    Cancel {
        id: u64,
    },
}

#[derive(Args)]
struct Fire {
    #[arg(long)]
    lesson: String,
    /// RFC 3339 时刻，如 2026-09-23T13:00:00+08:00
    #[arg(long)]
    at: Option<DateTime<FixedOffset>>,
    #[arg(long, default_value_t = 1)]
    attempts: u32,
    #[arg(long, default_value = "500ms", value_parser = humantime::parse_duration)]
    interval: Duration,
    #[arg(long)]
    wait: bool,
}

#[derive(Args)]
struct Watch {
    #[arg(long)]
    lesson: String,
    /// 0s 表示不设截止
    #[arg(long, default_value = "30m", value_parser = humantime::parse_duration)]
    timeout: Duration,
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

fn json_line(value: &impl Serialize) -> Result<()> {
    println!("{}", serde_json::to_string(value)?);
    Ok(())
}

/// Human-readable text by default; one JSON value per line with `--json`.
fn show<T: Serialize>(value: &T, json: bool, human: impl FnOnce(&T) -> String) -> Result<()> {
    if json {
        return json_line(value);
    }
    println!("{}", human(value));
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

/// Sends a command the daemon answers when the intent ends; exits non-zero unless it succeeded.
async fn until_end(command: Command, json: bool) -> Result<()> {
    let response = tokio::select! {
        response = rpc(command) => response?,
        _ = tokio::signal::ctrl_c() => bail!("停止等待；意图仍在后台运行"),
    };
    show(&response, json, render::response)?;
    let Response::Job(job) = response else {
        bail!("响应类型错误")
    };
    ensure!(
        job.progress.phase == Phase::Succeeded,
        "意图结束：{:?}",
        job.progress.phase
    );
    Ok(())
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
                show(&event, json, render::log)?;
            }
        }
        if !follow || pause_or_interrupt().await {
            return Ok(());
        }
    }
}

async fn add(lesson: String, trigger: Trigger, wait: bool, json: bool) -> Result<()> {
    let command = Command::Add {
        spec: Spec { lesson, trigger },
        wait,
    };
    if wait {
        return until_end(command, json).await;
    }
    show(&rpc(command).await?, json, render::response)
}

fn millis(duration: Duration) -> u64 {
    duration.as_millis().try_into().unwrap_or(u64::MAX)
}

async fn fire(args: Fire, select: bool, json: bool) -> Result<()> {
    let trigger = Trigger::Fire {
        select,
        at_ms: args.at.map(|at| at.timestamp_millis()),
        attempts: args.attempts,
        interval_ms: millis(args.interval),
    };
    add(args.lesson, trigger, args.wait, json).await
}

async fn watch(args: Watch, json: bool) -> Result<()> {
    let trigger = Trigger::Watch {
        timeout_ms: millis(args.timeout),
        dry_run: args.dry_run,
    };
    add(args.lesson, trigger, args.wait, json).await
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
        return json_line(&found);
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
        Action::Profile { id } => Maintenance::Profile { id }.into(),
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
        Action::Logout => Maintenance::Logout.into(),
        Action::Job { action } => match action {
            JobAction::Show { id } => Command::Job { id },
            JobAction::Cancel { id } => {
                return show(&rpc(Command::Cancel { id }).await?, json, |response| {
                    let row = render::response(response);
                    format!("已停止等待；已交给执行器的提交仍会完成\n{row}")
                });
            }
            JobAction::Wait { id } => return until_end(Command::Wait { id }, json).await,
        },
        Action::Fire(args) => return fire(args, true, json).await,
        Action::Drop(args) => return fire(args, false, json).await,
        Action::Watch(args) => return watch(args, json).await,
        Action::Prepare => Maintenance::Prepare.into(),
        Action::Refresh => Maintenance::Refresh.into(),
        Action::Channels { refresh: true } => Maintenance::Channels.into(),
        Action::Channels { refresh: false } => {
            let channels = cache::load_channel_cache()?;
            return show(&channels, json, |c| render::render_channels(&c.channels));
        }
        Action::Selected { refresh: true, .. } => Maintenance::Selected.into(),
        Action::Selected { profile, .. } => {
            let profile = profile.context("需要 --profile")?;
            return show(
                &cache::load_selected_snapshot(&profile)?,
                json,
                render::selected,
            );
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
            let done = format!("已导出到 {}", path.display());
            return show(&serde_json::json!({ "output": path }), json, |_| done);
        }
        Action::Cache {
            action: CacheAction::Clear,
        } => Maintenance::ClearCache.into(),
        Action::Cache {
            action: CacheAction::Status,
        } => {
            let mappings = cache::list_mapping_cache_statuses()?;
            let counts = cache::list_count_cache_statuses()?;
            let status = serde_json::json!({ "mappings": mappings, "counts": counts });
            return show(&status, json, |_| {
                let mappings = render::cache_statuses("课程映射", "门", &mappings);
                format!(
                    "{mappings}\n{}",
                    render::cache_statuses("名额", "条", &counts)
                )
            });
        }
        Action::Logs { follow } => return follow_logs(follow, json).await,
    };
    show(&rpc(command).await?, json, render::response)
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
