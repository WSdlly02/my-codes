//! Human-readable CLI output. `--json` prints the raw structures instead.
use super::{
    cache,
    protocol::{IntentView, LogEvent, Response, Status, Trigger},
    support::{format_age, now_ms},
};
use crate::model::{ChannelEntry, LessonMappingCache, SelectedSnapshot};
use chrono::{DateTime, FixedOffset, TimeZone};
use chrono_tz::Asia::Shanghai;
use std::{collections::HashMap, time::Duration};

pub(crate) fn response(response: &Response) -> String {
    let mut names = Names::default();
    match response {
        Response::Status(status) => render_status(status),
        Response::Jobs(jobs) if jobs.is_empty() => "没有运行中的意图".into(),
        Response::Jobs(jobs) => lines(jobs.iter().map(|job| intent(job, &mut names))),
        Response::Job(job) => intent(job, &mut names),
        Response::LoggedIn => "已登录".into(),
        Response::LoggedOut => "已退出登录".into(),
        Response::Profiles(profiles) => self::profiles(profiles),
        Response::ProfileInUse { profile } => {
            format!("已进入 profile {profile}，选课页面就绪")
        }
        Response::CoursesSynced {
            courses,
            counts,
            counts_from_cache,
        } => {
            let counts = match counts {
                Some(n) if *counts_from_cache => format!("{n} 条（读取失败，沿用旧缓存）"),
                Some(n) => format!("{n} 条"),
                None => "无".into(),
            };
            format!("已同步课程目录 {courses} 门；名额 {counts}")
        }
        Response::Selected(snapshot) => selected(snapshot),
        Response::Schedule { semester, .. } => format!("已取得学期 {semester} 的课表"),
        Response::Logs { events, .. } if events.is_empty() => "暂无日志".into(),
        Response::Logs { events, .. } => lines(events.iter().map(log)),
    }
}

pub(crate) fn log(event: &LogEvent) -> String {
    format!("{}  {}", clock(event.at_ms, true), event.message)
}

pub(crate) fn selected(snapshot: &SelectedSnapshot) -> String {
    let mut names = Names::default();
    let mut ids: Vec<&String> = snapshot.selected.keys().collect();
    ids.sort();
    let header = format!(
        "profile {} 已选 {} 门（{} 更新）",
        snapshot.profile,
        ids.len(),
        clock(snapshot.at_ms, false)
    );
    let rows = ids
        .into_iter()
        .map(|id| format!("  {}", names.label(&snapshot.profile, id)));
    lines(std::iter::once(header).chain(rows))
}

pub(crate) fn profiles(channels: &[ChannelEntry]) -> String {
    if channels.is_empty() {
        return "没有选课轮次".into();
    }
    lines(channels.iter().map(|c| {
        let state = if c.opened { "已开放" } else { "未开放" };
        format!(
            "profile {}  {}  {}  {}",
            pad(&c.profile_id, 6),
            pad(state, 6),
            c.name,
            c.open_time
        )
    }))
}

/// A cache timestamp, in Beijing time.
pub(crate) fn when(at: &DateTime<FixedOffset>) -> String {
    at.with_timezone(&Shanghai)
        .format("%m-%d %H:%M:%S")
        .to_string()
}

fn render_status(s: &Status) -> String {
    let context = match (&s.profile, s.context_ready) {
        (None, _) => "未选择".into(),
        (Some(p), true) => format!("{p}，选课页面就绪"),
        (Some(p), false) => format!("{p}，选课页面未打开"),
    };
    let mut reads = format!("每 {}", humantime::format_duration(ms(s.poll_ms)));
    match s.counts_at_ms {
        Some(at) => {
            let age = (now_ms() - at).max(0) as f64 / 1000.0;
            reads.push_str(&format!("；最近一次 {age:.1}s 前"));
        }
        None => reads.push_str("；尚未读取"),
    }
    if let Some(error) = &s.read_error {
        reads.push_str(&format!("；读取失败：{error}"));
    }
    let login = match (s.logged_in_at_ms, s.login_lost_at_ms) {
        (Some(at), None) => format!(
            "{} 登录，已 {}",
            clock(at, false),
            format_age(now_ms() - at)
        ),
        (Some(at), Some(lost)) => format!(
            "已失效：{} 登录，{} 发现被重定向到登录页（登录后约 {}）",
            clock(at, false),
            clock(lost, false),
            format_age(lost - at)
        ),
        (None, Some(lost)) => format!("已失效：{} 发现被重定向到登录页", clock(lost, false)),
        (None, None) => "未记录登录时间".into(),
    };
    let mut rows = vec![
        format!("{}{login}", pad("登录", 10)),
        format!("{}{context}", pad("profile", 10)),
        format!("{}{reads}", pad("名额读取", 10)),
        format!("{}{} 个运行中", pad("意图", 10), s.intents),
    ];
    if s.stopping {
        rows.push("daemon 正在关停".into());
    }
    lines(rows.into_iter())
}

fn intent(job: &IntentView, names: &mut Names) -> String {
    let p = &job.progress;
    let (trigger, attempts) = match &job.spec.trigger {
        Trigger::Fire {
            select,
            at_ms,
            attempts,
            ..
        } => {
            let action = if *select { "fire" } else { "drop" };
            let at = at_ms.map_or_else(|| "立即".into(), |at| clock(at, false));
            (
                format!("{action} {at}"),
                format!("{}/{attempts} 次", p.attempts),
            )
        }
        Trigger::Watch {
            timeout_ms,
            dry_run,
        } => {
            let limit = if *timeout_ms == 0 {
                "不限时".into()
            } else {
                humantime::format_duration(ms(*timeout_ms)).to_string()
            };
            let dry = if *dry_run { " dry-run" } else { "" };
            (format!("watch {limit}{dry}"), format!("{} 次", p.attempts))
        }
    };
    let mut row = format!(
        "#{}  {}  {}  {}  {}",
        pad(&job.id.to_string(), 3),
        pad(&p.phase.to_string(), 8),
        pad(&attempts, 8),
        pad(&trigger, 18),
        names.label(&job.profile, &job.spec.lesson)
    );
    if !p.last_result.is_empty() {
        row.push_str(&format!(" · {}", p.last_result));
    }
    row
}

/// Course names from the local mapping cache, loaded once per profile; IDs alone if absent.
#[derive(Default)]
struct Names(HashMap<String, Option<LessonMappingCache>>);

impl Names {
    fn label(&mut self, profile: &str, lesson: &str) -> String {
        let mapping = self
            .0
            .entry(profile.into())
            .or_insert_with(|| cache::load_mapping_cache(profile).ok());
        match mapping.as_ref().and_then(|m| m.by_lesson_id.get(lesson)) {
            Some(found) => format!("{lesson} {}", found.name),
            None => lesson.into(),
        }
    }
}

fn clock(at_ms: i64, millis: bool) -> String {
    let format = if millis { "%H:%M:%S%.3f" } else { "%H:%M:%S" };
    Shanghai
        .timestamp_millis_opt(at_ms)
        .single()
        .map_or_else(|| at_ms.to_string(), |t| t.format(format).to_string())
}

fn ms(millis: u64) -> Duration {
    Duration::from_millis(millis)
}

/// Pads to a terminal width, counting CJK and other wide characters as two columns.
fn pad(text: &str, width: usize) -> String {
    let used: usize = text.chars().map(|c| if c.is_ascii() { 1 } else { 2 }).sum();
    format!("{text}{}", " ".repeat(width.saturating_sub(used)))
}

fn lines(rows: impl Iterator<Item = String>) -> String {
    rows.collect::<Vec<_>>().join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pads_wide_characters_by_display_width() {
        assert_eq!(pad("成功", 6), "成功  ");
        assert_eq!(pad("ok", 4), "ok  ");
        assert_eq!(pad("过长的文本", 4), "过长的文本");
    }
}
