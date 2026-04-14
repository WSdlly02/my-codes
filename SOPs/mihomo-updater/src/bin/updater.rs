use std::{path::Path, time::Duration};

use anyhow::{Context, Result, bail};
use futures::future::join_all;
use reqwest::Client;
use time::{OffsetDateTime, format_description::FormatItem, macros::format_description};
use tokio::{fs, process::Command};

// ======== Configuration =======

const MIHOMO_DIR: &str = "/home/wsdlly02/.config/mihomo";
const LOCAL_CONFIG: &str = "/home/wsdlly02/.config/mihomo/config.yaml";
const BACKUP_CONFIG: &str = "/home/wsdlly02/Disks/Files/mihomo-config.yaml";
const SERVICE_NAME: &str = "mihomo.service";
const RESOLVER_URL: &str = "http://127.0.0.1:8088/config/full";

const DB_SOURCES: &[DBSource] = &[
    DBSource {
        name: "geoip",
        url: "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip-lite.dat",
        filename: "geoip-lite.dat",
    },
    DBSource {
        name: "geosite",
        url: "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geosite.dat",
        filename: "geosite.dat",
    },
    DBSource {
        name: "mmdb",
        url: "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip.metadb",
        filename: "geoip.metadb",
    },
    DBSource {
        name: "asn",
        url: "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/GeoLite2-ASN.mmdb",
        filename: "GeoLite2-ASN.mmdb",
    },
];

#[derive(Clone, Copy)]
struct DBSource {
    name: &'static str,
    url: &'static str,
    filename: &'static str,
}

// ======== Core Logic ========

#[tokio::main]
async fn main() -> Result<()> {
    println!(
        "=== Mihomo Updater: {} ===",
        now_string(format_description!(
            "[year]-[month]-[day] [hour]:[minute]:[second]"
        ))
    );

    fs::create_dir_all(MIHOMO_DIR)
        .await
        .with_context(|| format!("failed to create mihomo dir: {MIHOMO_DIR}"))?;

    let client = Client::builder()
        .timeout(Duration::from_secs(30))
        .build()
        .context("failed to build http client")?;

    // 1. Fetch config from Resolver
    println!("Fetching config from {RESOLVER_URL}...");
    let remote_data = fetch_config(&client, RESOLVER_URL).await?;
    if remote_data.is_empty() {
        bail!("config is empty");
    }
    println!("Remote config fetched successfully");

    // 2. Backup
    backup_existing_config().await?;

    // 3. Save new config
    write_atomic(Path::new(LOCAL_CONFIG), &remote_data).await?;
    write_atomic(Path::new(BACKUP_CONFIG), &remote_data).await?;
    println!("Config and backup saved");

    // 4. Download databases in parallel
    println!("[db] Checking for database updates...");
    let download_tasks = DB_SOURCES
        .iter()
        .map(|source| download_data(&client, source));

    for outcome in join_all(download_tasks).await {
        if let Err(err) = outcome {
            eprintln!("Warning: database update failed: {err:#}");
        }
    }
    println!("All databases updated");

    // 5. Restart Service
    println!("[service] Restarting Mihomo Service...");
    restart_service().await?;

    println!("=== Completed ===");
    Ok(())
}

async fn fetch_config(client: &Client, url: &str) -> Result<Vec<u8>> {
    let response = client
        .get(url)
        .header(reqwest::header::USER_AGENT, "Clash/Meta")
        .send()
        .await
        .with_context(|| format!("request failed: {url}"))?;

    let status = response.status();
    if !status.is_success() {
        bail!("http error while fetching config: {status}");
    }

    response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .with_context(|| format!("failed to read config response: {url}"))
}

async fn backup_existing_config() -> Result<()> {
    if fs::metadata(LOCAL_CONFIG).await.is_ok() {
        let ts = compact_timestamp();
        let backup_path = format!("{MIHOMO_DIR}/backup/config.yaml.bak.{ts}");
        if let Some(parent) = Path::new(&backup_path).parent() {
            fs::create_dir_all(parent)
                .await
                .with_context(|| format!("failed to create backup dir: {}", parent.display()))?;
        }
        fs::rename(LOCAL_CONFIG, &backup_path)
            .await
            .with_context(|| format!("failed to backup config to {backup_path}"))?;
    }

    Ok(())
}

async fn download_data(client: &Client, source: &DBSource) -> Result<()> {
    let dst = Path::new(MIHOMO_DIR).join(source.filename);
    let response = client
        .get(source.url)
        .header(reqwest::header::USER_AGENT, "Clash/Meta")
        .send()
        .await
        .with_context(|| format!("request failed for {}", source.name))?;

    let status = response.status();
    if !status.is_success() {
        bail!("bad status for {}: {status}", source.name);
    }

    let data = response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .with_context(|| format!("failed to read body for {}", source.name))?;

    if data.len() < 100 {
        bail!(
            "downloaded data too short for {}: {}",
            source.name,
            data.len()
        );
    }

    write_atomic(&dst, &data).await?;
    println!("Downloaded: {}", source.filename);
    Ok(())
}

async fn write_atomic(path: &Path, data: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .await
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }

    let tmp_path = path.with_extension(format!(
        "{}.tmp",
        path.extension()
            .and_then(|ext| ext.to_str())
            .unwrap_or("tmp")
    ));

    fs::write(&tmp_path, data)
        .await
        .with_context(|| format!("failed to write temp file {}", tmp_path.display()))?;
    fs::rename(&tmp_path, path)
        .await
        .with_context(|| format!("failed to move temp file to {}", path.display()))?;
    Ok(())
}

async fn restart_service() -> Result<()> {
    let output = Command::new("sudo")
        .arg("systemctl")
        .arg("restart")
        .arg(SERVICE_NAME)
        .output()
        .await
        .context("failed to start restart command")?;

    if !output.status.success() {
        bail!(
            "failed to restart service: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

fn now_string(pattern: &'static [FormatItem<'static>]) -> String {
    let now = OffsetDateTime::now_local().unwrap_or_else(|_| OffsetDateTime::now_utc());
    now.format(pattern)
        .unwrap_or_else(|_| "unknown-time".to_string())
}

fn compact_timestamp() -> String {
    now_string(format_description!(
        "[year][month][day]-[hour][minute][second]"
    ))
}
