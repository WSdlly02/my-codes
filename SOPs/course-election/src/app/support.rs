use anyhow::{Context, Result, bail};
use chrono::{DateTime, Datelike, FixedOffset, TimeZone, Utc};
use chrono_tz::Asia::Shanghai;
use std::env;
use std::fmt::Display;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

pub(crate) const BASE_URL: &str = "https://jwxt.shmtu.edu.cn/shmtu";
pub(crate) const CACHE_DIR: &str = "cache";
pub(crate) const COOKIE_FILE: &str = "cache/cookies.json";
pub(crate) const CHROME_PROFILE: &str = "chrome-profile";
pub(crate) const SEMESTER_ID_ENV: &str = "COURSE_ELECTION_SEMESTER_ID";
pub(crate) const SEMESTER_ID_OF_AUTUMN_SEMESTER_OF_2025_ACADEMIC_YEAR: i32 = 395;
pub(crate) const TIME_LAYOUT: &str = "%Y-%m-%d %H:%M:%S %Z";
pub(crate) const RETRY_ATTEMPTS: usize = 3;
pub(crate) const DEFAULT_TIMEOUT_SECS: u64 = 10;
pub(crate) const DEFAULT_OCR_MODEL: &str = "qwen3-vl:8b-instruct";
pub(crate) const DEFAULT_OLLAMA_URL: &str = "http://10.144.144.64:11434/api/generate";

pub(crate) fn now_fixed() -> DateTime<FixedOffset> {
    Shanghai
        .from_utc_datetime(&Utc::now().naive_utc())
        .fixed_offset()
}

pub(crate) fn format_time<Tz>(dt: DateTime<Tz>) -> String
where
    Tz: TimeZone,
    Tz::Offset: Display,
{
    dt.format(TIME_LAYOUT).to_string()
}

pub(crate) fn should_retry_status(status: u16) -> bool {
    matches!(status, 408 | 429 | 500 | 502 | 503 | 504)
}

pub(crate) fn normalize_index_key(input: &str) -> String {
    input.trim().to_lowercase()
}

pub(crate) fn split_teachers(input: &str) -> Vec<String> {
    input
        .split([',', '，'])
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .map(ToOwned::to_owned)
        .collect()
}

pub(crate) fn urlencoding(input: &str) -> String {
    let mut out = String::new();
    for byte in input.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(byte as char)
            }
            b' ' => out.push('+'),
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}

pub(crate) fn parse_duration_arg(input: &str) -> std::result::Result<Duration, String> {
    let value = input.trim();
    if let Some(ms) = value.strip_suffix("ms") {
        let parsed = ms.parse::<u64>().map_err(|err| err.to_string())?;
        return Ok(Duration::from_millis(parsed));
    }
    if let Some(sec) = value.strip_suffix('s') {
        let parsed = sec.parse::<u64>().map_err(|err| err.to_string())?;
        return Ok(Duration::from_secs(parsed));
    }
    Err("仅支持 ms 或 s，如 500ms / 1s".to_string())
}

pub(crate) fn resolve_semester_id(manual: Option<&str>) -> Result<String> {
    if let Some(value) = manual {
        return normalize_semester_id(value, "--semester-id");
    }
    if let Ok(value) = env::var(SEMESTER_ID_ENV) {
        return normalize_semester_id(&value, SEMESTER_ID_ENV);
    }
    Ok(semester_id_for_date(now_fixed()).to_string())
}

fn normalize_semester_id(value: &str, source: &str) -> Result<String> {
    let trimmed = value.trim();
    if trimmed.is_empty() || !trimmed.bytes().all(|byte| byte.is_ascii_digit()) {
        bail!("{source} 必须是数字学期 ID");
    }
    Ok(trimmed.to_string())
}

pub(crate) fn semester_id_for_date(now: DateTime<FixedOffset>) -> i32 {
    let year = now.year();
    let month = now.month();
    let academic_year = if month >= 9 { year } else { year - 1 };
    let autumn_id =
        SEMESTER_ID_OF_AUTUMN_SEMESTER_OF_2025_ACADEMIC_YEAR + (academic_year - 2025) * 20;
    if (2..=8).contains(&month) {
        autumn_id + 1
    } else {
        autumn_id
    }
}

pub(crate) fn ensure_cache_dir() -> Result<()> {
    fs::create_dir_all(CACHE_DIR).context("创建 cache 目录失败")
}

pub(crate) fn cookie_file_path() -> PathBuf {
    PathBuf::from(COOKIE_FILE)
}

pub(crate) fn chrome_profile_path() -> Result<PathBuf> {
    let path = if let Some(value) = env::var_os("COURSE_ELECTION_CHROME_PROFILE") {
        PathBuf::from(value)
    } else if cfg!(target_os = "windows") {
        env::var_os("LOCALAPPDATA")
            .map(PathBuf::from)
            .unwrap_or(env::current_dir().context("获取当前目录失败")?)
            .join("course-election")
            .join(CHROME_PROFILE)
    } else {
        PathBuf::from(CHROME_PROFILE)
    };
    let path = if path.is_absolute() {
        path
    } else {
        env::current_dir().context("获取当前目录失败")?.join(path)
    };
    fs::create_dir_all(&path)
        .with_context(|| format!("创建 Chrome profile 目录失败: {}", path.display()))?;
    Ok(path)
}

pub(crate) fn channels_cache_path() -> PathBuf {
    Path::new(CACHE_DIR).join("channels.json")
}

pub(crate) fn mapping_cache_path(profile_id: &str) -> PathBuf {
    Path::new(CACHE_DIR).join(format!("mapping_{profile_id}.json"))
}

pub(crate) fn counts_cache_path(profile_id: &str) -> PathBuf {
    Path::new(CACHE_DIR).join(format!("counts_{profile_id}.json"))
}

#[cfg(test)]
mod tests {
    use super::semester_id_for_date;
    use chrono::TimeZone;
    use chrono_tz::Asia::Shanghai;

    #[test]
    fn semester_id_matches_known_2025_spring_window() {
        let date = Shanghai
            .with_ymd_and_hms(2026, 2, 1, 0, 0, 0)
            .single()
            .unwrap()
            .fixed_offset();
        assert_eq!(semester_id_for_date(date), 396);
    }

    #[test]
    fn semester_id_matches_known_2026_autumn_window() {
        let date = Shanghai
            .with_ymd_and_hms(2026, 9, 1, 0, 0, 0)
            .single()
            .unwrap()
            .fixed_offset();
        assert_eq!(semester_id_for_date(date), 415);
    }

    #[test]
    fn semester_id_keeps_january_in_previous_autumn() {
        let date = Shanghai
            .with_ymd_and_hms(2027, 1, 31, 23, 59, 59)
            .single()
            .unwrap()
            .fixed_offset();
        assert_eq!(semester_id_for_date(date), 415);
    }
}
