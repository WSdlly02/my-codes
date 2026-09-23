use super::{
    cache,
    output::{self, LessonQueryFilter},
    protocol::{Command, Maintenance, Phase, Response, SOCKET_FILE, Spec, Trigger},
    render, support,
};
use anyhow::{Context, Result, bail, ensure};
use chrono::{DateTime, FixedOffset, NaiveTime, TimeZone};
use chrono_tz::Asia::Shanghai;
use clap::{Args, Parser, Subcommand};
use serde::Serialize;
use std::{path::PathBuf, time::Duration};

/// How often `logs --follow` polls the daemon.
const POLL: Duration = Duration::from_millis(250);

/// 上海海事大学选课客户端：离线查询课程缓存，并向 course-electiond 下达指令。
///
/// 典型流程：login → profile list --sync → profile use <ID> → course sync →
/// course find <名称> → fire / watch。本工具不启动、不停止 daemon。
#[derive(Parser)]
#[command(name = "course-election", version)]
struct Cli {
    /// 数据目录，须与 course-electiond 的 --data-dir 相同
    #[arg(long, global = true, default_value = ".", help_heading = "全局选项")]
    data_dir: PathBuf,
    /// 每行输出一个 JSON 值，供脚本和程序解析
    #[arg(long, global = true, help_heading = "全局选项")]
    json: bool,
    #[command(subcommand)]
    command: Action,
}

#[derive(Subcommand)]
enum Action {
    /// 查看 daemon：当前 profile、选课页面、名额读取、运行中意图
    Status,
    /// 登录（CAS 与验证码识别由 daemon 完成）；完成后全部意图结束
    Login {
        /// 学号
        username: String,
        /// 从标准输入读取一行作为密码，而不是交互式隐藏输入
        #[arg(long)]
        password_stdin: bool,
    },
    /// 退出登录并清除 Cookie；全部意图结束
    Logout,
    /// 选课轮次（profile）：列出，或进入某个轮次
    #[command(subcommand)]
    Profile(ProfileAction),
    /// 课程目录与已选课程
    #[command(subcommand)]
    Course(CourseAction),
    /// 到点直接提交选课，不看名额
    Fire(Fire),
    /// 到点直接提交退课
    Drop(Fire),
    /// 捡漏：名额读取出现空位时提交选课
    Watch(Watch),
    /// 查看、等待或取消意图；不带参数时列出全部运行中的意图
    Job(Job),
    /// 查看 daemon 最近的日志（内存中最多 512 条），含每个意图的最终结果
    Logs {
        /// 持续输出新日志，Ctrl-C 结束
        #[arg(long)]
        follow: bool,
    },
    /// 导出课表为 HTML 文件
    Schedule {
        /// 学期 ID；缺省按当前日期推算（也可用环境变量 COURSE_ELECTION_SEMESTER_ID 指定）
        #[arg(long)]
        semester: Option<String>,
        /// 输出文件；缺省为 class-schedule-<学期ID>.html
        #[arg(long)]
        output: Option<PathBuf>,
    },
}

#[derive(Subcommand)]
enum ProfileAction {
    /// 列出选课轮次及其 profile ID（读本地缓存）
    List {
        /// 先从服务器同步轮次列表
        #[arg(long)]
        sync: bool,
    },
    /// 进入该轮次：打开选课页面并取得提交所需的 token；其他 profile 的意图随之结束
    Use {
        /// profile ID，见 profile list
        id: String,
    },
}

#[derive(Subcommand)]
enum CourseAction {
    /// 从服务器同步当前 profile 的课程目录和名额到本地缓存（会重新打开选课页面）
    Sync,
    /// 查询本地课程目录缓存；未指定 --profile 时需向 daemon 查询当前轮次，不访问教务网站
    Find(Find),
    /// 查看已选课程缓存；未指定 --profile 时需向 daemon 查询当前轮次，只有 --sync 会访问教务网站
    Selected {
        /// 先从服务器同步当前 profile 的已选课程
        #[arg(long, conflicts_with = "profile")]
        sync: bool,
        /// 显式指定后无需 daemon，可完全离线读取已有缓存；缺省需 daemon 已运行并进入某个 profile
        #[arg(long)]
        profile: Option<String>,
    },
}

#[derive(Args)]
struct Find {
    /// 课程名（模糊匹配）；不填则列出全部
    name: Option<String>,
    /// 按 lessonID 精确查找
    #[arg(long, conflicts_with = "code")]
    id: Option<String>,
    /// 按课程号或课序号查找
    #[arg(long)]
    code: Option<String>,
    /// 只显示已选课程（需先 course selected --sync）
    #[arg(long)]
    selected: bool,
    /// 显式指定后无需 daemon，可完全离线读取已有缓存；缺省需 daemon 已运行并进入某个 profile
    #[arg(long)]
    profile: Option<String>,
}

#[derive(Args)]
struct Fire {
    /// lessonID，或课程目录缓存中唯一匹配的完整课程名
    lesson: String,
    /// 开抢时刻：HH:MM[:SS]（今天，北京时间）或 RFC 3339；缺省立即。会提前 5 秒预热连接
    #[arg(long, value_parser = parse_at)]
    at: Option<i64>,
    /// 最多提交几次；只有服务器明确拒绝（未开放、已满等）才会重试
    #[arg(long, default_value_t = 1)]
    attempts: u32,
    /// 两次提交之间的间隔，如 300ms、1s
    #[arg(long, default_value = "500ms", value_parser = humantime::parse_duration)]
    interval: Duration,
    /// 等到意图结束再返回；退出码表示是否成功
    #[arg(long)]
    wait: bool,
}

#[derive(Args)]
struct Watch {
    /// lessonID，或课程目录缓存中唯一匹配的完整课程名
    lesson: String,
    /// 最长等待时间，如 30m、2h；0s 表示不设截止
    #[arg(long, default_value = "30m", value_parser = humantime::parse_duration)]
    timeout: Duration,
    /// 发现空位只记录、不提交
    #[arg(long)]
    dry_run: bool,
    /// 等到意图结束再返回；退出码表示是否成功
    #[arg(long)]
    wait: bool,
}

#[derive(Args)]
#[command(args_conflicts_with_subcommands = true)]
struct Job {
    /// 只看这个意图
    id: Option<u64>,
    #[command(subcommand)]
    action: Option<JobAction>,
}

#[derive(Subcommand)]
enum JobAction {
    /// 等待意图结束；Ctrl-C 只停止等待，意图继续运行
    Wait {
        /// 意图 ID，见 job
        id: u64,
    },
    /// 停止意图的等待；已交给执行器的提交仍会完成，结果照常记录。这不是退课
    Cancel {
        /// 意图 ID，见 job
        id: u64,
    },
}

#[derive(Serialize)]
struct FoundLessons<'a> {
    profile: &'a str,
    mapping_at: DateTime<FixedOffset>,
    counts_at: Option<DateTime<FixedOffset>>,
    lessons: &'a [output::LessonDisplayEntry],
}

/// A future `HH:MM[:SS]` today in Beijing time, or a future RFC 3339 instant; as Unix ms.
fn parse_at(text: &str) -> Result<i64, String> {
    let at = match DateTime::parse_from_rfc3339(text) {
        Ok(at) => at.timestamp_millis(),
        Err(_) => {
            let time = NaiveTime::parse_from_str(text, "%H:%M:%S")
                .or_else(|_| NaiveTime::parse_from_str(text, "%H:%M"))
                .map_err(
                    |_| "格式应为 HH:MM[:SS] 或 RFC 3339，如 13:00 或 2026-09-23T13:00:00+08:00",
                )?;
            let today = support::now_fixed().date_naive();
            Shanghai
                .from_local_datetime(&today.and_time(time))
                .single()
                .ok_or("无效的本地时刻")?
                .timestamp_millis()
        }
    };
    if at <= support::now_ms() {
        return Err("该时刻已过（HH:MM 指今天；跨天请写完整的 RFC 3339 时刻）".into());
    }
    Ok(at)
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

/// The explicit profile, or else the daemon's current one.
async fn profile_or_current(explicit: Option<String>) -> Result<String> {
    if let Some(profile) = explicit {
        return Ok(profile);
    }
    let Response::Status(status) = rpc(Command::Status)
        .await
        .context("无法向 daemon 询问当前 profile；请用 --profile 指定")?
    else {
        bail!("响应类型错误")
    };
    status
        .profile
        .context("daemon 尚未进入 profile；请先 profile use，或用 --profile 指定")
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
        "意图结束：{}",
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
            eprintln!("日志缓冲有缺口；意图状态以 job 为准");
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
        at_ms: args.at,
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

async fn find(args: Find, json: bool) -> Result<()> {
    let profile = profile_or_current(args.profile).await?;
    let mapping = cache::load_mapping_cache(&profile)
        .context("没有该 profile 的课程目录缓存；请先 course sync")?;
    let counts = cache::load_count_snapshot(&profile).ok();
    let selected_lesson_ids = if args.selected {
        cache::load_selected_snapshot(&profile)
            .context("没有已选课程缓存；请先 course selected --sync")?
            .selected
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
        return json_line(&FoundLessons {
            profile: &profile,
            mapping_at: mapping.fetched_at,
            counts_at: counts.as_ref().map(|c| c.fetched_at),
            lessons: &entries,
        });
    }
    println!(
        "profile {profile}：课程目录同步于 {}，名额同步于 {}；共 {} 条结果",
        render::when(&mapping.fetched_at),
        counts
            .as_ref()
            .map_or_else(|| "（无）".into(), |c| render::when(&c.fetched_at)),
        entries.len()
    );
    for (i, e) in entries.iter().enumerate() {
        print!("{}", output::format_lesson_display_entry(i + 1, e));
    }
    Ok(())
}

async fn selected(sync: bool, profile: Option<String>, json: bool) -> Result<()> {
    if sync {
        let response = rpc(Maintenance::SyncSelected.into()).await?;
        return show(&response, json, render::response);
    }
    let profile = profile_or_current(profile).await?;
    let snapshot = cache::load_selected_snapshot(&profile)
        .context("没有已选课程缓存；请先 course selected --sync")?;
    show(&snapshot, json, render::selected)
}

async fn schedule(semester: Option<String>, output: Option<PathBuf>, json: bool) -> Result<()> {
    let semester = support::resolve_semester_id(semester.as_deref())?;
    let path = output.unwrap_or_else(|| format!("class-schedule-{semester}.html").into());
    let Response::Schedule { html, .. } =
        rpc(Maintenance::ExportSchedule { semester }.into()).await?
    else {
        bail!("响应类型错误")
    };
    std::fs::write(&path, html)?;
    let done = format!("课表已导出到 {}", path.display());
    show(&serde_json::json!({ "output": path }), json, |_| done)
}

pub(crate) async fn run() -> Result<()> {
    let cli = Cli::parse();
    std::env::set_current_dir(&cli.data_dir)?;
    let json = cli.json;
    let command = match cli.command {
        Action::Status => Command::Status,
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
        Action::Profile(ProfileAction::List { sync: true }) => Maintenance::SyncProfiles.into(),
        Action::Profile(ProfileAction::List { sync: false }) => {
            let profiles =
                cache::load_channel_cache().context("没有轮次缓存；请先 profile list --sync")?;
            return show(&profiles, json, |c| render::profiles(&c.channels));
        }
        Action::Profile(ProfileAction::Use { id }) => Maintenance::UseProfile { id }.into(),
        Action::Course(CourseAction::Sync) => Maintenance::SyncCourses.into(),
        Action::Course(CourseAction::Find(args)) => return find(args, json).await,
        Action::Course(CourseAction::Selected { sync, profile }) => {
            return selected(sync, profile, json).await;
        }
        Action::Fire(args) => return fire(args, true, json).await,
        Action::Drop(args) => return fire(args, false, json).await,
        Action::Watch(args) => return watch(args, json).await,
        Action::Job(Job { action: None, id }) => match id {
            Some(id) => Command::Job { id },
            None => Command::Jobs,
        },
        Action::Job(Job {
            action: Some(JobAction::Wait { id }),
            ..
        }) => return until_end(Command::Wait { id }, json).await,
        Action::Job(Job {
            action: Some(JobAction::Cancel { id }),
            ..
        }) => {
            return show(&rpc(Command::Cancel { id }).await?, json, |response| {
                let row = render::response(response);
                format!("已停止等待；已交给执行器的提交仍会完成\n{row}")
            });
        }
        Action::Logs { follow } => return follow_logs(follow, json).await,
        Action::Schedule { semester, output } => return schedule(semester, output, json).await,
    };
    show(&rpc(command).await?, json, render::response)
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    #[test]
    fn clap_schema() {
        Cli::command().debug_assert();
    }

    /// Every subcommand and every option explains itself in --help.
    #[test]
    fn everything_is_documented() {
        fn check(command: &clap::Command, path: &str) {
            for sub in command.get_subcommands().filter(|s| s.get_name() != "help") {
                let path = format!("{path} {}", sub.get_name());
                assert!(sub.get_about().is_some(), "{path} 缺少说明");
                check(sub, &path);
            }
            for arg in command.get_arguments() {
                let builtin = matches!(arg.get_id().as_str(), "help" | "version");
                assert!(
                    builtin || arg.get_help().is_some(),
                    "{path} {} 缺少说明",
                    arg.get_id()
                );
            }
        }
        check(&Cli::command(), "course-election");
    }

    #[test]
    fn job_takes_an_optional_id_or_a_subcommand() {
        let parse = |args: &[&str]| Cli::try_parse_from(["cli", "job"].iter().chain(args));
        assert!(matches!(
            parse(&[]).unwrap().command,
            Action::Job(Job {
                id: None,
                action: None
            })
        ));
        assert!(matches!(
            parse(&["3"]).unwrap().command,
            Action::Job(Job { id: Some(3), .. })
        ));
        assert!(matches!(
            parse(&["cancel", "3"]).unwrap().command,
            Action::Job(Job {
                action: Some(JobAction::Cancel { id: 3 }),
                ..
            })
        ));
        assert!(parse(&["pause", "3"]).is_err());
    }

    #[test]
    fn at_accepts_future_times_only() {
        assert_eq!(parse_at("2099-01-01T13:00:00+08:00"), Ok(4_070_926_800_000));
        assert!(parse_at("2000-01-01T00:00:00+08:00").is_err());
        assert!(parse_at("13点").is_err());
        let soon = support::now_fixed() + chrono::Duration::minutes(2);
        if soon.date_naive() == support::now_fixed().date_naive() {
            let at = parse_at(&soon.format("%H:%M:%S").to_string()).unwrap();
            assert!((at - soon.timestamp_millis()).abs() < 1000);
        }
    }

    #[test]
    fn rejects_removed_and_lifecycle_commands() {
        for args in [
            &["daemon", "stop"][..],
            &["arm"],
            &["prepare"],
            &["jobs"],
            &["cache", "clear"],
            &["status", "--force"],
        ] {
            assert!(Cli::try_parse_from(["cli"].iter().chain(args)).is_err());
        }
    }
}
