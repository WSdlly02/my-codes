use anyhow::{Context, Result, anyhow, bail};
use chrono::{DateTime, FixedOffset, Utc};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use rustyline_async::{Readline, ReadlineEvent, SharedWriter};
use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;

use crate::app::cache::{
    clear_derived_caches, clear_login_state, list_count_cache_statuses,
    list_mapping_cache_statuses, load_channel_cache, load_count_snapshot, load_mapping_cache,
    load_saved_cookies,
};
use crate::app::http::{
    ElectionContext, Session, fetch_and_cache_channels, fetch_elected_lesson_ids,
    fetch_lesson_counts, prewarm, query_class_schedule_html, refresh_course_data,
    transient_request_error,
};
use crate::app::login::login;
use crate::app::output::{
    LessonQueryFilter, build_lesson_display_entries, format_lesson_display_entry,
};
use crate::app::parser::{
    resolve_lesson_id_by_name, selection_succeeded, summarize_selection_response,
};
use crate::app::support::{channels_cache_path, format_time, now_fixed, resolve_semester_id};

struct State {
    session: Session,
    profile_id: Option<String>,
    lesson_id: Option<String>,
}

const PROMPT: &str = "course-election> ";
const ARM_PROMPT: &str = "arm> ";
/// 名额没有变化时，每多少轮打印一次心跳，避免刷屏又能看出程序还活着。
const WATCH_HEARTBEAT_ROUNDS: usize = 12;

pub(crate) async fn run() -> Result<()> {
    let session = load_saved_cookies()
        .ok()
        .and_then(|saved| Session::new(saved.cookies).ok())
        .unwrap_or(Session::empty()?);
    let mut state = State {
        session,
        profile_id: None,
        lesson_id: None,
    };
    print_help();
    let (mut readline, mut writer) = Readline::new(PROMPT.to_string())?;
    readline.should_print_line_on(false, false);

    loop {
        let line = match readline.readline().await? {
            ReadlineEvent::Line(line) => {
                suspend_readline(&mut readline)?;
                println!("{PROMPT}{line}");
                line
            }
            ReadlineEvent::Interrupted => {
                suspend_readline(&mut readline)?;
                println!("^C");
                resume_readline(&mut readline, PROMPT)?;
                continue;
            }
            ReadlineEvent::Eof => break,
        };
        let line = line.trim();
        if line.is_empty() {
            resume_readline(&mut readline, PROMPT)?;
            continue;
        }
        readline.add_history_entry(line.to_string());
        let (command, argument) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let result = match command {
            "help" => {
                print_help();
                Ok(())
            }
            "login" => run_login(&mut state, argument).await,
            "status" => run_status(&state).await,
            "channels" => run_channels(&state).await,
            "profile" => set_profile(&mut state, argument),
            "refresh" => run_refresh(&state).await,
            "find" => run_find(&state, argument).await,
            "target" => set_target(&mut state, argument),
            "export-schedule" => run_export_schedule(&state, argument).await,
            "arm" => run_arm(&state, argument, &mut readline, &mut writer).await,
            "watch" => run_watch(&state, argument).await,
            "fire" => run_fire_command(&state, argument).await,
            "drop" => run_drop_command(&state, argument).await,
            "clear" => run_clear(&mut state, argument),
            "quit" | "exit" => break,
            _ => Err(anyhow!("未知命令，输入 help 查看用法")),
        };
        disable_raw_mode()?;
        if let Err(error) = result {
            eprintln!("错误：{error:#}");
        }
        resume_readline(&mut readline, PROMPT)?;
    }
    readline.flush()?;
    state.session.persist_cookies()?;
    Ok(())
}

fn suspend_readline(readline: &mut Readline) -> Result<()> {
    readline.update_prompt("")?;
    disable_raw_mode()?;
    Ok(())
}

fn resume_readline(readline: &mut Readline, prompt: &str) -> Result<()> {
    enable_raw_mode()?;
    readline.update_prompt(prompt)?;
    Ok(())
}

async fn run_export_schedule(state: &State, argument: &str) -> Result<()> {
    let mut parts = argument.split_whitespace();
    let semester_id = resolve_semester_id(parts.next())?;
    let output = parts
        .next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(format!("class-schedule-{semester_id}.html")));
    if parts.next().is_some() {
        bail!("用法：export-schedule [semester-id] [output.html]");
    }
    let html = query_class_schedule_html(&state.session, &semester_id).await?;
    std::fs::write(&output, html).with_context(|| format!("写入课表失败: {}", output.display()))?;
    println!("已导出学期 {semester_id} 课表到 {}", output.display());
    Ok(())
}

async fn run_login(state: &mut State, argument: &str) -> Result<()> {
    let username = argument.trim();
    if username.is_empty() {
        bail!("用法：login <用户名>");
    }
    let password = rpassword::prompt_password("密码: ")?;
    if password.is_empty() {
        bail!("密码不能为空");
    }
    state.session = login(username, &password).await?;
    println!("登录成功");
    Ok(())
}

async fn run_status(state: &State) -> Result<()> {
    let cookies = state.session.cookies();
    println!("cookies: {}", cookies.len());
    for cookie in cookies {
        let expires = cookie
            .expires
            .map(|value| format_time(value.with_timezone(&chrono_tz::Asia::Shanghai)))
            .unwrap_or_else(|| "session".into());
        println!("  {} expires={expires}", cookie.name);
    }
    println!("sessionValid: {}", state.session.is_session_valid().await);
    println!(
        "profile: {}",
        state.profile_id.as_deref().unwrap_or("<unset>")
    );
    println!(
        "target: {}",
        state.lesson_id.as_deref().unwrap_or("<unset>")
    );
    println!("channelsCache: {}", channels_cache_path().exists());
    for (profile, count, fetched) in list_mapping_cache_statuses()? {
        println!("mapping[{profile}]: {count} lessons, {fetched}");
    }
    for (profile, count, fetched) in list_count_cache_statuses()? {
        println!("counts[{profile}]: {count} entries, {fetched}");
    }
    Ok(())
}

async fn run_channels(state: &State) -> Result<()> {
    let channels = match fetch_and_cache_channels(&state.session).await {
        Ok(channels) => channels,
        Err(error) => {
            eprintln!("警告：在线刷新通道失败，使用本地缓存：{error:#}");
            load_channel_cache()?.channels
        }
    };
    for channel in channels {
        println!(
            "[{}] {} | profile={} | {} | {}",
            channel.round_no,
            channel.name,
            channel.profile_id,
            if channel.opened {
                "已开放"
            } else {
                "未开放"
            },
            channel.open_time
        );
    }
    Ok(())
}

fn set_profile(state: &mut State, argument: &str) -> Result<()> {
    let value = argument.trim();
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        bail!("用法：profile <数字 profileID>");
    }
    state.profile_id = Some(value.to_string());
    state.lesson_id = None;
    println!("profile={value}");
    Ok(())
}

async fn run_refresh(state: &State) -> Result<()> {
    let profile = require_profile(state)?;
    let data = refresh_course_data(&state.session, profile).await?;
    println!(
        "mapping={} counts={}{}",
        data.mapping.lessons.len(),
        data.counts.as_ref().map_or(0, |value| value.counts.len()),
        if data.counts_from_cache {
            "（容量来自缓存）"
        } else {
            ""
        }
    );
    Ok(())
}

async fn run_find(state: &State, argument: &str) -> Result<()> {
    let profile = require_profile(state)?;
    let mut value = argument.trim();
    let selected_only = value == "--selected" || value.starts_with("--selected ");
    if selected_only {
        value = value.strip_prefix("--selected").unwrap().trim();
    }
    let (name, lesson_id, code) = if let Some(value) = value.strip_prefix("--id ") {
        (None, Some(value.trim().to_string()), None)
    } else if let Some(value) = value.strip_prefix("--code ") {
        (None, None, Some(value.trim().to_string()))
    } else if value.is_empty() || value == "--all" {
        (None, None, None)
    } else {
        (Some(value.to_string()), None, None)
    };
    let mapping = load_mapping_cache(profile)
        .with_context(|| format!("无法读取 profile={profile} 的课程缓存，请先执行 refresh"))?;
    let counts = load_count_snapshot(profile).ok();
    let selected_lesson_ids = if selected_only {
        fetch_elected_lesson_ids(&state.session, profile).await?
    } else {
        HashMap::new()
    };
    let filter = LessonQueryFilter {
        name,
        lesson_id,
        code,
        selected_only,
        selected_lesson_ids,
    };
    let entries = build_lesson_display_entries(&mapping, counts.as_ref(), &filter);
    for (index, entry) in entries.iter().enumerate() {
        print!("{}", format_lesson_display_entry(index + 1, entry));
    }
    println!("共 {} 门课程", entries.len());
    Ok(())
}

fn set_target(state: &mut State, argument: &str) -> Result<()> {
    let value = argument.trim();
    if value.is_empty() {
        bail!("用法：target <lessonID|完整课程名>");
    }
    let profile = require_profile(state)?;
    let lesson_id = if value.bytes().all(|byte| byte.is_ascii_digit()) {
        value.to_string()
    } else {
        resolve_lesson_id_by_name(&load_mapping_cache(profile)?, value)?
    };
    state.lesson_id = Some(lesson_id.clone());
    println!("target={lesson_id}");
    Ok(())
}

async fn run_arm(
    state: &State,
    argument: &str,
    readline: &mut Readline,
    writer: &mut SharedWriter,
) -> Result<()> {
    require_profile(state)?;
    require_target(state)?;
    if argument.trim().is_empty() {
        println!("正在预热；按 Enter 或输入 fire 触发，输入 cancel 取消");
        resume_readline(readline, ARM_PROMPT)?;
        let event = {
            let input = readline.readline();
            tokio::pin!(input);
            loop {
                tokio::select! {
                    biased;
                    event = &mut input => break event?,
                    result = async {
                        report_prewarm(&state.session, writer).await?;
                        tokio::time::sleep(Duration::from_secs(10)).await;
                        Ok::<(), anyhow::Error>(())
                    } => result?,
                }
            }
        }; // Drop any in-flight prewarm before starting the action.
        match event {
            ReadlineEvent::Line(line) => {
                suspend_readline(readline)?;
                println!("{ARM_PROMPT}{line}");
                let line = line.trim();
                if line == "cancel" {
                    println!("已取消");
                    return Ok(());
                }
                readline.add_history_entry(line.to_string());
                let arguments = line.strip_prefix("fire").unwrap_or(line).trim();
                return run_fire_command(state, arguments).await;
            }
            ReadlineEvent::Interrupted => {
                suspend_readline(readline)?;
                println!("已取消");
                return Ok(());
            }
            ReadlineEvent::Eof => {
                disable_raw_mode()?;
                return Ok(());
            }
        }
    }

    let target = DateTime::parse_from_rfc3339(argument.trim())?;
    if target <= Utc::now() {
        bail!("arm 时间必须晚于当前时间");
    }
    let prewarm_at = target - chrono::Duration::seconds(5);
    if wait_until_or_cancel(prewarm_at, readline).await? {
        return Ok(());
    }
    let cancelled = {
        let wait = wait_until_or_cancel(target, readline);
        tokio::pin!(wait);
        tokio::select! {
            biased;
            result = &mut wait => result?,
            result = report_prewarm(&state.session, writer) => {
                result?;
                wait.await?
            }
        }
    };
    if cancelled {
        return Ok(());
    }
    let lateness = Utc::now().signed_duration_since(target).num_milliseconds();
    eprintln!("计时：定时触发偏差 {lateness:+}ms（本地时钟）");
    run_action(state, 1, Duration::from_millis(500), true).await
}

async fn report_prewarm(session: &Session, writer: &mut SharedWriter) -> Result<()> {
    let started = std::time::Instant::now();
    let result = prewarm(session).await;
    let elapsed = started.elapsed().as_millis();
    match result {
        Ok(()) => writeln!(writer, "预热完成：{elapsed}ms")?,
        Err(error) => writeln!(writer, "预热失败：{elapsed}ms，{error}；仍可触发选课")?,
    }
    Ok(())
}

/// 事件驱动捡漏：只读地轮询名额快照，出现空位才走一次写入链路。
///
/// 初始化选课上下文后轮询 `queryStdCount.action`；有空位才调用共享提交链路。
async fn run_watch(state: &State, argument: &str) -> Result<()> {
    let result = run_watch_inner(state, argument).await;
    persist_action_cookies(&state.session);
    result
}

async fn run_watch_inner(state: &State, argument: &str) -> Result<()> {
    let profile = require_profile(state)?;
    let lesson = require_target(state)?;

    let mut interval = Duration::from_secs(5);
    let mut timeout = Some(Duration::from_secs(1800));
    let mut dry_run = false;
    let mut numbers = 0usize;
    for token in argument.split_whitespace() {
        if token == "--dry-run" {
            dry_run = true;
            continue;
        }
        if token.starts_with("--") {
            bail!("未知参数：{token}");
        }
        let value = token
            .parse::<u64>()
            .with_context(|| format!("参数必须是整数秒：{token}"))?;
        match numbers {
            0 => interval = Duration::from_secs(value.max(1)),
            1 => timeout = (value != 0).then(|| Duration::from_secs(value)),
            _ => bail!("用法：watch [间隔秒] [超时秒，0=不限时] [--dry-run]"),
        }
        numbers += 1;
    }

    println!(
        "监视 lesson {lesson}：每 {} 秒读取一次名额快照{}；仅在出现空位时选课{}",
        interval.as_secs(),
        match timeout {
            Some(value) => format!("，最长 {} 秒", value.as_secs()),
            None => "，不限时".to_string(),
        },
        if dry_run {
            "（dry-run：只观察不出手）"
        } else {
            ""
        },
    );

    let deadline = timeout
        .map(|duration| {
            tokio::time::Instant::now()
                .checked_add(duration)
                .context("监视超时时间过大")
        })
        .transpose()?;
    let mut election = ElectionContext::new(&state.session, profile, deadline);
    // Establish the query context once; refresh only after an explicit stale-token rejection.
    loop {
        if watch_expired(deadline) {
            return Ok(());
        }
        match election.prepare().await {
            Ok(()) => break,
            Err(_) if watch_expired(deadline) => return Ok(()),
            Err(error) if transient_request_error(&error) => {
                println!("建立选课上下文暂时失败：{error:#}；继续等待");
                watch_sleep(interval, deadline).await;
            }
            Err(error) => return Err(error),
        }
    }
    println!(
        "[{}] 已建立课选上下文并取得 token",
        format_time(now_fixed())
    );

    let mut last: Option<(i64, i64, i64)> = None;
    let mut rounds = 0usize;

    loop {
        if watch_expired(deadline) {
            return Ok(());
        }
        rounds += 1;
        let query = fetch_lesson_counts(&state.session, profile);
        let result = match deadline {
            Some(deadline) => match tokio::time::timeout_at(deadline, query).await {
                Ok(result) => result,
                Err(_) => {
                    watch_expired(Some(deadline));
                    return Ok(());
                }
            },
            None => query.await,
        };
        let counts = match result {
            Ok(counts) => counts,
            Err(error) if transient_request_error(&error) => {
                println!(
                    "[{}] 名额查询暂时失败：{error:#}；继续等待",
                    format_time(now_fixed())
                );
                watch_sleep(interval, deadline).await;
                continue;
            }
            Err(error) => return Err(error),
        };
        let Some(count) = counts.get(lesson) else {
            println!(
                "[{}] 第 {rounds} 轮：{lesson} 不在本轮名额快照里（轮次可能已切换）",
                format_time(now_fixed())
            );
            watch_sleep(interval, deadline).await;
            continue;
        };

        let snapshot = (count.selected, count.limit, count.reserved);
        let vacancy = count.limit - count.selected - count.reserved;
        if last != Some(snapshot) {
            last = Some(snapshot);
            println!(
                "[{}] 第 {rounds} 轮：{} {}/{}，空位 {}{}",
                format_time(now_fixed()),
                lesson,
                count.selected,
                count.limit,
                vacancy,
                if count.reserved > 0 {
                    format!("（保留 {}）", count.reserved)
                } else {
                    String::new()
                },
            );
        } else if rounds % WATCH_HEARTBEAT_ROUNDS == 0 {
            println!(
                "[{}] 第 {rounds} 轮：{}/{} 无变化，继续等待",
                format_time(now_fixed()),
                count.selected,
                count.limit
            );
        }

        if vacancy > 0 {
            if watch_expired(deadline) {
                return Ok(());
            }
            if dry_run {
                println!(
                    "[{}] 空位 {vacancy}，dry-run 不出手",
                    format_time(now_fixed())
                );
            } else {
                println!("[{}] 空位 {vacancy}，出手选课", format_time(now_fixed()));
                match election.submit(lesson, true).await {
                    Ok(body) => {
                        let message = summarize_selection_response(&body);
                        println!("[{}] {message}", format_time(now_fixed()));
                        if selection_succeeded(&body) {
                            println!(
                                "[{}] 已命中，停止监视（共 {rounds} 轮）",
                                format_time(now_fixed())
                            );
                            return Ok(());
                        }
                    }
                    Err(error) => {
                        println!("[{}] 选课请求失败：{error:#}", format_time(now_fixed()))
                    }
                }
            }
        }

        watch_sleep(interval, deadline).await;
    }
}

fn watch_expired(deadline: Option<tokio::time::Instant>) -> bool {
    let expired = deadline.is_some_and(|limit| tokio::time::Instant::now() >= limit);
    if expired {
        println!("[{}] 达到最长等待时间，停止监视", format_time(now_fixed()));
    }
    expired
}

async fn watch_sleep(interval: Duration, deadline: Option<tokio::time::Instant>) {
    let duration = deadline.map_or(interval, |end| {
        interval.min(end.saturating_duration_since(tokio::time::Instant::now()))
    });
    tokio::time::sleep(duration).await;
}

async fn wait_until_or_cancel(
    target: DateTime<FixedOffset>,
    readline: &mut Readline,
) -> Result<bool> {
    let wait = target.signed_duration_since(Utc::now());
    let Ok(wait) = wait.to_std() else {
        return Ok(false);
    };
    println!("等待至 {}，输入 cancel 取消", target.to_rfc3339());
    resume_readline(readline, ARM_PROMPT)?;
    tokio::select! {
        _ = tokio::time::sleep(wait) => {
            suspend_readline(readline)?;
            Ok(false)
        },
        event = readline.readline() => {
            match event? {
                ReadlineEvent::Line(line) => {
                    suspend_readline(readline)?;
                    println!("{ARM_PROMPT}{line}");
                    let line = line.trim();
                    if line == "cancel" {
                        println!("已取消");
                        Ok(true)
                    } else {
                        bail!("定时 arm 期间只接受 cancel")
                    }
                }
                ReadlineEvent::Interrupted => {
                    suspend_readline(readline)?;
                    println!("已取消");
                    Ok(true)
                }
                ReadlineEvent::Eof => {
                    disable_raw_mode()?;
                    Ok(true)
                },
            }
        }
    }
}

async fn run_fire_command(state: &State, argument: &str) -> Result<()> {
    let (attempts, interval) = parse_retry_args(argument)?;
    run_action(state, attempts, interval, true).await
}

async fn run_drop_command(state: &State, argument: &str) -> Result<()> {
    let (attempts, interval) = parse_retry_args(argument)?;
    run_action(state, attempts, interval, false).await
}

fn parse_retry_args(argument: &str) -> Result<(usize, Duration)> {
    let mut parts = argument.split_whitespace();
    let attempts = parts.next().unwrap_or("1").parse::<usize>()?;
    let interval = parts.next().unwrap_or("500").parse::<u64>()?;
    if parts.next().is_some() {
        bail!("用法：[fire|drop] [次数，0=无限] [间隔毫秒]");
    }
    Ok((attempts, Duration::from_millis(interval)))
}

async fn run_action(
    state: &State,
    attempts: usize,
    interval: Duration,
    select: bool,
) -> Result<()> {
    let result = run_action_inner(state, attempts, interval, select).await;
    persist_action_cookies(&state.session);
    result
}

fn persist_action_cookies(session: &Session) {
    if let Err(error) = session.persist_cookies() {
        eprintln!("保存 Cookie 失败：{error:#}");
    }
}

async fn run_action_inner(
    state: &State,
    attempts: usize,
    interval: Duration,
    select: bool,
) -> Result<()> {
    let profile = require_profile(state)?;
    let lesson = require_target(state)?;
    let mut attempt = 0usize;
    // Never share a token across commands: refresh/find --selected can replace it.
    let mut election = ElectionContext::new(&state.session, profile, None);
    loop {
        attempt += 1;
        let result = election.submit(lesson, select).await;
        match result {
            Ok(body) => {
                println!("[{attempt}] {}", summarize_selection_response(&body));
                if selection_succeeded(&body) {
                    return Ok(());
                }
            }
            Err(error) => println!("[{attempt}] 请求失败：{error:#}"),
        }
        if attempts > 0 && attempt >= attempts {
            bail!("达到最大尝试次数");
        }
        tokio::time::sleep(interval).await;
    }
}

fn run_clear(state: &mut State, argument: &str) -> Result<()> {
    let all = match argument.trim() {
        "" => false,
        "all" => true,
        _ => bail!("用法：clear [all]"),
    };
    clear_login_state()?;
    state.session = Session::empty()?;
    println!("已清除登录状态");
    if all {
        clear_derived_caches()?;
        state.profile_id = None;
        state.lesson_id = None;
        println!("已清除课程映射和容量缓存");
    }
    Ok(())
}

fn require_profile(state: &State) -> Result<&str> {
    state
        .profile_id
        .as_deref()
        .ok_or_else(|| anyhow!("请先执行 profile <id>"))
}

fn require_target(state: &State) -> Result<&str> {
    state
        .lesson_id
        .as_deref()
        .ok_or_else(|| anyhow!("请先执行 target <lesson-id>"))
}

fn print_help() {
    println!(
        "命令：login <用户名> | status | channels | profile <id> | refresh | find [--selected] [--id ID|--code CODE|名称] | target <lesson-id|完整课程名> | export-schedule [semester-id] [output.html] | arm [RFC3339时间] | watch [间隔秒] [超时秒，0=不限时] [--dry-run] | fire/drop [次数] [间隔ms] | clear [all] | quit"
    );
}
