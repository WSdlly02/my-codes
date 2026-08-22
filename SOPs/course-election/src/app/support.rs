use anyhow::{Context, Result, bail};
use chrono::{DateTime, Datelike, FixedOffset, TimeZone, Utc};
use chrono_tz::Asia::Shanghai;
use std::env;
use std::fmt::Display;
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) const BASE_URL: &str = "https://jwxt.shmtu.edu.cn/shmtu";
pub(crate) const CACHE_DIR: &str = "cache";
pub(crate) const RETRY_ATTEMPTS: usize = 3;
pub(crate) const DEFAULT_TIMEOUT_SECS: u64 = 10;
pub(crate) const DEFAULT_OCR_MODEL: &str = "qwen3-vl:8b-instruct";
pub(crate) const DEFAULT_OLLAMA_URL: &str = "http://10.144.144.64:11434/api/generate";
pub(crate) const SEMESTER_ID_ENV: &str = "COURSE_ELECTION_SEMESTER_ID";
pub(crate) const SEMESTER_ID_OF_AUTUMN_SEMESTER_OF_2025_ACADEMIC_YEAR: i32 = 395;

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
    dt.format("%Y-%m-%d %H:%M:%S %Z").to_string()
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

pub(crate) fn resolve_semester_id(manual: Option<&str>) -> Result<String> {
    if let Some(value) = manual {
        return normalize_semester_id(value, "semester-id");
    }
    if let Ok(value) = env::var(SEMESTER_ID_ENV) {
        return normalize_semester_id(&value, SEMESTER_ID_ENV);
    }
    Ok(semester_id_for_date(now_fixed()).to_string())
}

fn normalize_semester_id(value: &str, source: &str) -> Result<String> {
    let value = value.trim();
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        bail!("{source} 必须是数字学期 ID");
    }
    Ok(value.to_string())
}

pub(crate) fn semester_id_for_date(now: DateTime<FixedOffset>) -> i32 {
    let academic_year = if now.month() >= 9 {
        now.year()
    } else {
        now.year() - 1
    };
    let autumn = SEMESTER_ID_OF_AUTUMN_SEMESTER_OF_2025_ACADEMIC_YEAR + (academic_year - 2025) * 20;
    autumn + i32::from((2..=8).contains(&now.month()))
}

pub(crate) fn ensure_cache_dir() -> Result<()> {
    fs::create_dir_all(CACHE_DIR).context("创建 cache 目录失败")
}

pub(crate) fn cookie_file_path() -> PathBuf {
    Path::new(CACHE_DIR).join("cookies.json")
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
    fn semester_id_rolls_between_autumn_and_spring() {
        let spring = Shanghai
            .with_ymd_and_hms(2026, 2, 1, 0, 0, 0)
            .single()
            .unwrap()
            .fixed_offset();
        let next_autumn = Shanghai
            .with_ymd_and_hms(2026, 9, 1, 0, 0, 0)
            .single()
            .unwrap()
            .fixed_offset();
        let january = Shanghai
            .with_ymd_and_hms(2027, 1, 31, 23, 59, 59)
            .single()
            .unwrap()
            .fixed_offset();
        assert_eq!(semester_id_for_date(spring), 396);
        assert_eq!(semester_id_for_date(next_autumn), 415);
        assert_eq!(semester_id_for_date(january), 415);
    }
}
