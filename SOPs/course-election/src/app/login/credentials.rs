use anyhow::{Context, Result, anyhow};
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Serialize, Deserialize)]
pub(crate) struct Credentials {
    pub(crate) username: String,
    pub(super) password: String,
    pub(super) totp_base32: String,
    pub(super) api_key: String,
}

impl Credentials {
    pub(crate) fn read(path: &Path) -> Result<Self> {
        let pairs = dotenvy::from_path_iter(path)
            .with_context(|| format!("读取凭据文件 {} 失败", path.display()))?;
        let mut username = None;
        let mut password = None;
        let mut totp = None;
        let mut api_key = None;
        for pair in pairs {
            // dotenvy's syntax errors may contain the original line, including its secret.
            let (key, value) = pair.map_err(|_| anyhow!("凭据文件语法错误"))?;
            match key.as_str() {
                "USERNAME" => username = Some(value),
                "PASSWORD" => password = Some(value),
                "TOTP_BASE32" => totp = Some(value),
                "DEEPSEEK_API_KEY" => api_key = Some(value),
                _ => {}
            }
        }
        Ok(Self {
            username: username
                .filter(|s| !s.is_empty())
                .context("缺少 USERNAME")?,
            password: password
                .filter(|s| !s.is_empty())
                .context("缺少 PASSWORD")?,
            totp_base32: std::env::var("TOTP_BASE32")
                .ok()
                .or(totp)
                .filter(|s| !s.trim().is_empty())
                .context("缺少 TOTP_BASE32")?,
            api_key: std::env::var("DEEPSEEK_API_KEY")
                .ok()
                .or(api_key)
                .filter(|s| !s.trim().is_empty())
                .context("缺少 DEEPSEEK_API_KEY")?,
        })
    }
}
