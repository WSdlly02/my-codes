use anyhow::{Context, Result};
use std::fs;
use std::io::Write;
use std::path::Path;

use crate::app::support::{
    channels_cache_path, cookie_file_path, counts_cache_path, ensure_cache_dir, mapping_cache_path,
    selected_cache_path,
};
use crate::model::{
    ChannelCache, LessonCountSnapshot, LessonMappingCache, SavedCookie, SavedCookies,
    SelectedSnapshot,
};

pub(crate) fn load_saved_cookies() -> Result<SavedCookies> {
    let text = fs::read_to_string(cookie_file_path()).context("读取 cookies 失败")?;
    serde_json::from_str(&text).context("解析 cookies 失败")
}

pub(crate) fn save_cookies(cookies: &[SavedCookie]) -> Result<()> {
    save_json(
        &cookie_file_path(),
        &SavedCookies {
            cookies: cookies.to_vec(),
        },
    )
}

pub(crate) fn save_channel_cache(cache: &ChannelCache) -> Result<()> {
    save_json(&channels_cache_path(), cache)
}

pub(crate) fn load_channel_cache() -> Result<ChannelCache> {
    load_json(&channels_cache_path())
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

pub(crate) fn load_selected_snapshot(profile_id: &str) -> Result<SelectedSnapshot> {
    load_json(&selected_cache_path(profile_id))
}

pub(crate) fn save_selected_snapshot(snapshot: &SelectedSnapshot) -> Result<()> {
    save_json(&selected_cache_path(&snapshot.profile), snapshot)
}

pub(crate) fn clear_login_state() -> Result<()> {
    remove_if_present(&cookie_file_path())
}

fn load_json<T>(path: &Path) -> Result<T>
where
    T: for<'de> serde::Deserialize<'de>,
{
    let text = fs::read_to_string(path).with_context(|| format!("读取失败: {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("解析失败: {}", path.display()))
}

fn remove_if_present(path: &Path) -> Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("删除失败: {}", path.display())),
    }
}

fn save_json<T>(path: &Path, value: &T) -> Result<()>
where
    T: serde::Serialize,
{
    ensure_cache_dir()?;
    let body = serde_json::to_string_pretty(value).context("序列化 JSON 失败")?;
    let temporary = path.with_extension(format!("tmp.{}", std::process::id()));
    let mut options = fs::OpenOptions::new();
    options.create(true).truncate(true).write(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut file = options
        .open(&temporary)
        .with_context(|| format!("创建临时文件失败: {}", temporary.display()))?;
    file.write_all(body.as_bytes())?;
    file.sync_all()?;
    drop(file);
    if let Err(error) = fs::rename(&temporary, path) {
        #[cfg(windows)]
        if error.kind() == std::io::ErrorKind::AlreadyExists {
            // ponytail: Windows lacks atomic replace here; add a tiny atomic-write crate if crashes prove relevant.
            fs::remove_file(path)?;
            return fs::rename(&temporary, path)
                .with_context(|| format!("替换失败: {}", path.display()));
        }
        return Err(error).with_context(|| format!("替换失败: {}", path.display()));
    }
    Ok(())
}
