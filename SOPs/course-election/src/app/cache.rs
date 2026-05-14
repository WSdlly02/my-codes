use anyhow::{Context, Result};
use glob::glob;
use std::fs;
use std::path::Path;

use crate::app::support::{
    channels_cache_path, cookie_file_path, counts_cache_path, ensure_cache_dir, format_time,
    mapping_cache_path,
};
use crate::model::{
    ChannelCache, LessonCountSnapshot, LessonMappingCache, SavedCookie, SavedCookies,
};

pub(crate) fn load_saved_cookies() -> Result<SavedCookies> {
    let text = fs::read_to_string(cookie_file_path()).context("读取 cookies 失败")?;
    serde_json::from_str(&text).context("解析 cookies 失败")
}

pub(crate) fn save_cookies(cookies: &[SavedCookie]) -> Result<()> {
    save_json(&cookie_file_path(), &SavedCookies {
        cookies: cookies.to_vec(),
    })
}

pub(crate) fn load_channel_cache() -> Result<ChannelCache> {
    load_json(&channels_cache_path())
}

pub(crate) fn save_channel_cache(cache: &ChannelCache) -> Result<()> {
    save_json(&channels_cache_path(), cache)
}

pub(crate) fn load_mapping_cache(profile_id: &str) -> Result<LessonMappingCache> {
    load_json(&mapping_cache_path(profile_id))
}

pub(crate) fn save_mapping_cache(profile_id: &str, cache: &LessonMappingCache) -> Result<()> {
    save_json(&mapping_cache_path(profile_id), cache)
}

pub(crate) fn load_count_snapshot(profile_id: &str) -> Result<LessonCountSnapshot> {
    load_json(&counts_cache_path(profile_id))
}

pub(crate) fn save_count_snapshot(profile_id: &str, snapshot: &LessonCountSnapshot) -> Result<()> {
    save_json(&counts_cache_path(profile_id), snapshot)
}

pub(crate) fn flush_login_state() -> Result<()> {
    match fs::remove_file(cookie_file_path()) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(err) => Err(err).context("删除 cookies 失败"),
    }
}

pub(crate) fn flush_derived_caches() -> Result<()> {
    for pattern in ["cache/mapping_*.json", "cache/counts_*.json"] {
        for entry in glob(pattern).context("读取缓存文件列表失败")? {
            let path = entry.context("解析缓存路径失败")?;
            match fs::remove_file(&path) {
                Ok(()) => {}
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => return Err(err).with_context(|| format!("删除缓存失败: {}", path.display())),
            }
        }
    }
    Ok(())
}

pub(crate) fn list_mapping_cache_statuses() -> Result<Vec<(String, usize, String)>> {
    let mut statuses = Vec::new();
    for entry in glob("cache/mapping_*.json").context("读取 mapping 缓存失败")? {
        let path = entry.context("解析 mapping 缓存路径失败")?;
        let cache: LessonMappingCache = load_json(&path)?;
        let profile_id = if cache.profile_id.is_empty() {
            profile_id_from_path(&path, "mapping_")
        } else {
            cache.profile_id.clone()
        };
        statuses.push((
            profile_id,
            cache.lessons.len(),
            format_time(cache.fetched_at.with_timezone(&chrono_tz::Asia::Shanghai)),
        ));
    }
    statuses.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(statuses)
}

pub(crate) fn list_count_cache_statuses() -> Result<Vec<(String, usize, String)>> {
    let mut statuses = Vec::new();
    for entry in glob("cache/counts_*.json").context("读取 counts 缓存失败")? {
        let path = entry.context("解析 counts 缓存路径失败")?;
        let cache: LessonCountSnapshot = load_json(&path)?;
        let profile_id = if cache.profile_id.is_empty() {
            profile_id_from_path(&path, "counts_")
        } else {
            cache.profile_id.clone()
        };
        statuses.push((
            profile_id,
            cache.counts.len(),
            format_time(cache.fetched_at.with_timezone(&chrono_tz::Asia::Shanghai)),
        ));
    }
    statuses.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(statuses)
}

fn load_json<T>(path: &Path) -> Result<T>
where
    T: for<'de> serde::Deserialize<'de>,
{
    let text = fs::read_to_string(path).with_context(|| format!("读取失败: {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("解析失败: {}", path.display()))
}

fn save_json<T>(path: &Path, value: &T) -> Result<()>
where
    T: serde::Serialize,
{
    ensure_cache_dir()?;
    let body = serde_json::to_string_pretty(value).context("序列化 JSON 失败")?;
    fs::write(path, body).with_context(|| format!("写入失败: {}", path.display()))
}

fn profile_id_from_path(path: &Path, prefix: &str) -> String {
    let file_name = path.file_stem().and_then(|s| s.to_str()).unwrap_or_default();
    file_name.strip_prefix(prefix).unwrap_or(file_name).to_string()
}
