use anyhow::{Result, anyhow, bail};
use clap::Parser;
use std::collections::HashMap;
use std::thread;

use crate::app::browser::{LoginAutofillOptions, ensure_login_with_options};
use crate::app::cache::{
    flush_derived_caches, flush_login_state, list_count_cache_statuses,
    list_mapping_cache_statuses, load_channel_cache, load_mapping_cache, load_saved_cookies,
};
use crate::app::cli::{ActionArgs, Cli, Commands, FlushStateArgs, QueryArgs, WarmupArgs};
use crate::app::http::{
    Session, drop_lesson, fetch_and_cache_channels, fetch_elected_lesson_ids,
    load_or_fetch_channels, query_class_schedule_html, query_course_data, select_lesson,
};
use crate::app::output::{
    LessonQueryFilter, build_lesson_display_entries, format_lesson_display_entry,
};
use crate::app::parser::{
    resolve_lesson_id_by_name, selection_succeeded, summarize_selection_response,
};
use crate::app::support::{channels_cache_path, format_time};

pub(crate) fn run() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Status => run_status(),
        Commands::Query(args) => run_query(args),
        Commands::Warmup(args) => run_warmup(args),
        Commands::FlushState(args) => run_flush_state(args),
        Commands::Select(args) => run_action(args, true),
        Commands::Drop(args) => run_action(args, false),
    }
}

fn run_status() -> Result<()> {
    match load_saved_cookies()
        .ok()
        .and_then(|saved| Session::new(saved.cookies).ok())
    {
        Some(session) => {
            println!("cookieFile: present ({} cookies)", session.cookies.len());
            for cookie in &session.cookies {
                if let Some(expires) = cookie.expires.as_ref() {
                    println!(
                        "{}: expires at {}",
                        cookie.name,
                        format_time(expires.with_timezone(&chrono_tz::Asia::Shanghai))
                    );
                } else {
                    println!("{}: session cookie", cookie.name);
                }
            }
            println!("sessionValid: {}", session.is_session_valid());
        }
        None => println!("cookieFile: missing"),
    }

    println!("channelsCache: {}", channels_cache_path().exists());

    let mapping_statuses = list_mapping_cache_statuses()?;
    println!("mappingCaches: {}", mapping_statuses.len());
    for (profile_id, lesson_count, fetched_at) in mapping_statuses {
        println!("mapping[{profile_id}]: {lesson_count} lessons, fetched at {fetched_at}");
    }

    let count_statuses = list_count_cache_statuses()?;
    println!("countsCaches: {}", count_statuses.len());
    for (profile_id, count_count, fetched_at) in count_statuses {
        println!("counts[{profile_id}]: {count_count} entries, fetched at {fetched_at}");
    }
    Ok(())
}

fn run_query(args: QueryArgs) -> Result<()> {
    let session = load_saved_cookies()
        .ok()
        .and_then(|saved| Session::new(saved.cookies).ok());
    let session_valid = session.as_ref().is_some_and(Session::is_session_valid);

    if args.class_schedule {
        let session = session
            .filter(Session::is_session_valid)
            .ok_or_else(|| anyhow!("当前 Cookie 无效，无法查询 class-schedule，请先执行 warmup"))?;
        let html = query_class_schedule_html(&session)?;
        print!("{html}");
        return Ok(());
    }

    let Some(profile_id) = args.profile.as_deref() else {
        if args.selected_lessons {
            bail!("--selected-lessons 必须配合 --profile 使用");
        }
        let channels = if session_valid {
            load_or_fetch_channels(session.as_ref().expect("checked above"))?
        } else {
            load_channel_cache()
                .map(|cache| cache.channels)
                .map_err(|_| anyhow!("当前 Cookie 无效，且本地没有通道缓存，请先执行 warmup"))?
        };

        for ch in channels {
            let state = if ch.opened { "已开放" } else { "未开放" };
            let mut line = format!(
                "[{}] {} | profile={} | {}",
                ch.round_no, ch.name, ch.profile_id, state
            );
            if !ch.open_time.is_empty() {
                line.push_str(" | ");
                line.push_str(&ch.open_time);
            }
            println!("{line}");
        }
        return Ok(());
    };

    let data = query_course_data(
        session_valid.then_some(session.as_ref().expect("checked above")),
        profile_id,
    )?;

    let selected_lesson_ids = if args.selected_lessons {
        let session = session
            .filter(Session::is_session_valid)
            .ok_or_else(|| anyhow!("当前 Cookie 无效，无法查询已选课程，请先执行 warmup"))?;
        fetch_elected_lesson_ids(&session, profile_id)?
    } else {
        HashMap::new()
    };

    let filter = LessonQueryFilter {
        name: args.name,
        lesson_id: args.lesson_id,
        code: args.code,
        selected_only: args.selected_lessons,
        selected_lesson_ids,
    };
    let entries = build_lesson_display_entries(&data.mapping, data.counts.as_ref(), &filter);
    for (idx, entry) in entries.iter().enumerate() {
        print!("{}", format_lesson_display_entry(idx + 1, entry));
    }
    println!("共 {} 门课程", entries.len());
    if data.counts_from_cache {
        if let Some(snapshot) = data.counts {
            println!(
                "警告：容量数据获取失败，使用本地副本：{}，数据可能不准确",
                format_time(
                    snapshot
                        .fetched_at
                        .with_timezone(&chrono_tz::Asia::Shanghai)
                )
            );
        }
    }
    Ok(())
}

fn run_warmup(args: WarmupArgs) -> Result<()> {
    let opts = LoginAutofillOptions {
        name: args.username,
        password: args.password,
        autofill_captcha: args.autofill_captcha,
    };
    let (session, relogin) = ensure_login_with_options(opts)?;
    if relogin {
        println!("已重新登录并刷新 Cookie");
    } else {
        println!("复用现有 Cookie");
    }
    match fetch_and_cache_channels(&session) {
        Ok(channels) => println!(
            "已缓存 {} 个选课通道到 {}",
            channels.len(),
            channels_cache_path().display()
        ),
        Err(err) => eprintln!("警告：Cookie 已保存，但选课通道解析失败: {err}"),
    }
    Ok(())
}

fn run_flush_state(args: FlushStateArgs) -> Result<()> {
    flush_login_state()?;
    println!("已清除登录状态");
    if args.all {
        flush_derived_caches()?;
        println!("已清除 mapping/counts 缓存");
    }
    Ok(())
}

fn run_action(args: ActionArgs, select_mode: bool) -> Result<()> {
    let profile_id = args
        .profile
        .as_deref()
        .ok_or_else(|| anyhow!("缺少 --profile"))?;

    if args.lesson_id.is_some() == args.course_name.is_some() {
        bail!("必须二选一传入 --lesson-id 或 --name");
    }

    let session = load_session_from_saved_login()?;
    if !session.is_session_valid() {
        bail!("当前 Cookie 无效，请先执行 warmup");
    }

    let resolved_lesson_id = if let Some(lesson_id) = args.lesson_id.clone() {
        lesson_id
    } else {
        let mapping =
            load_mapping_cache(profile_id).map_err(|err| anyhow!("读取课程映射缓存失败: {err}"))?;
        let lesson_id = resolve_lesson_id_by_name(
            &mapping,
            args.course_name.as_deref().expect("validated above"),
        )?;
        println!("resolved lessonID: {lesson_id}");
        lesson_id
    };

    let mut attempt = 0usize;
    loop {
        attempt += 1;
        let result = if select_mode {
            select_lesson(&session, profile_id, &resolved_lesson_id)
        } else {
            drop_lesson(&session, profile_id, &resolved_lesson_id)
        };

        match result {
            Ok(body) => {
                let summary = summarize_selection_response(&body);
                println!("[{attempt}] {summary}");
                if selection_succeeded(&body) {
                    return Ok(());
                }
            }
            Err(err) => {
                println!("[{attempt}] 请求失败: {err}");
            }
        }

        if args.retry > 0 && attempt >= args.retry {
            bail!("达到最大重试次数，仍未成功");
        }
        thread::sleep(args.interval);
    }
}

fn load_session_from_saved_login() -> Result<Session> {
    let saved = load_saved_cookies().ok();
    let Some(saved) = saved else {
        bail!("未找到可用 Cookie");
    };
    if saved.cookies.is_empty() {
        bail!("未找到可用 Cookie");
    }
    Session::new(saved.cookies)
}
