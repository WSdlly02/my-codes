use anyhow::{Context, Result};
use chrono::{DateTime, FixedOffset, TimeZone, Utc};
use chrono_tz::Asia::Shanghai;
use std::fmt::Display;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

pub(crate) const BASE_URL: &str = "https://jwxt.shmtu.edu.cn/shmtu";
pub(crate) const CACHE_DIR: &str = "cache";
pub(crate) const COOKIE_FILE: &str = "cache/cookies.json";
pub(crate) const CHROME_PROFILE: &str = "chrome-profile";
pub(crate) const SEMESTER_ID: &str = "396";
pub(crate) const TIME_LAYOUT: &str = "%Y-%m-%d %H:%M:%S %Z";
pub(crate) const RETRY_ATTEMPTS: usize = 3;
pub(crate) const DEFAULT_TIMEOUT_SECS: u64 = 10;
pub(crate) const DEFAULT_OCR_MODEL: &str = "qwen3-vl:8b-instruct";
pub(crate) const DEFAULT_OLLAMA_URL: &str = "http://localhost:11434/api/generate";

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

pub(crate) fn ensure_cache_dir() -> Result<()> {
    fs::create_dir_all(CACHE_DIR).context("创建 cache 目录失败")
}

pub(crate) fn cookie_file_path() -> PathBuf {
    PathBuf::from(COOKIE_FILE)
}

pub(crate) fn chrome_profile_path() -> PathBuf {
    PathBuf::from(CHROME_PROFILE)
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

