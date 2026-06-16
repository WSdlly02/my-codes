use std::path::PathBuf;

/// One line in `<course>/manifest.jsonl`.
///
/// This is deliberately PDF-level, not course/atom/chapter-level. The OCR stage
/// only knows how to map one PDF to one cache directory.
#[derive(serde::Deserialize, Debug, Clone)]
pub struct PdfManifestRow {
    /// Maps to `<cache-dir>/<id>/`.
    pub id: String,
    /// Reader-facing name used in generated markdown metadata and heading.
    pub pdf_name: String,
    pub pdf_path: PathBuf,
}

#[derive(serde::Deserialize, Debug)]
pub struct ApiEnvelope<T> {
    pub data: T,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SubmitJobData {
    pub job_id: String,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct JobStatusData {
    pub state: String,
    pub extract_progress: Option<ExtractProgress>,
    pub result_url: Option<ResultUrl>,
    pub error_msg: Option<String>,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExtractProgress {
    pub total_pages: Option<u64>,
    pub extracted_pages: Option<u64>,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ResultUrl {
    pub json_url: String,
}

use anyhow::Result;
use std::path::{Component, Path};
use tokio::fs;

pub async fn cleanup_tmp(output_dir: &Path) -> Result<()> {
    remove_path_if_exists(&output_dir.join(".tmp")).await
}

pub async fn write_error_log(output_dir: &Path, err: &anyhow::Error) -> Result<()> {
    fs::create_dir_all(output_dir).await?;
    fs::write(output_dir.join("error.log"), format!("{err:#}\n")).await?;
    Ok(())
}

pub async fn remove_path_if_exists(path: &Path) -> Result<()> {
    match fs::metadata(path).await {
        Ok(metadata) if metadata.is_dir() => {
            fs::remove_dir_all(path).await?;
        }
        Ok(_) => {
            fs::remove_file(path).await?;
        }
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
        Err(err) => return Err(err.into()),
    }
    Ok(())
}

pub async fn is_nonempty_file(path: &Path) -> bool {
    fs::metadata(path)
        .await
        .map(|metadata| metadata.is_file() && metadata.len() > 0)
        .unwrap_or(false)
}

pub fn sanitize_relative_path(path: &str) -> Option<PathBuf> {
    let mut safe = PathBuf::new();
    for component in Path::new(path).components() {
        match component {
            Component::Normal(part) => safe.push(part),
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => return None,
        }
    }
    if safe.as_os_str().is_empty() {
        None
    } else {
        Some(safe)
    }
}

pub fn stable_name(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.') {
            output.push(ch);
        } else {
            output.push('_');
        }
    }
    if output.is_empty() {
        "image".to_string()
    } else {
        output
    }
}
