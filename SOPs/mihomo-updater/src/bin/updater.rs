use std::{env, path::Path, time::Duration};

use anyhow::{Context, Result, bail};
use futures::future::join_all;
use reqwest::Client;
use time::{OffsetDateTime, format_description::FormatItem, macros::format_description};
use tokio::{fs, process::Command};
use url::Url;

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

struct Args {
    access_token: String,
}

// ======== Core Logic ========

#[tokio::main]
async fn main() -> Result<()> {
    let args = parse_args()?;

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
    let resolver_url_with_token = build_resolver_url_with_token(&args.access_token)?;
    println!("Fetching config from {RESOLVER_URL}...");
    let remote_data = fetch_config(&client, resolver_url_with_token.as_str()).await?;
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

fn parse_args() -> Result<Args> {
    let mut args = env::args().skip(1);
    let mut access_token = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--access-token" => {
                let token = args
                    .next()
                    .context("--access-token requires a non-empty value")?;
                if token.is_empty() {
                    bail!("--access-token requires a non-empty value");
                }
                access_token = Some(token);
            }
            "--help" | "-h" => {
                println!("Usage: mihomo-updater --access-token <token>");
                std::process::exit(0);
            }
            _ => bail!("unknown argument: {arg}"),
        }
    }

    Ok(Args {
        access_token: access_token.context("missing required argument: --access-token <token>")?,
    })
}

fn build_resolver_url_with_token(access_token: &str) -> Result<String> {
    let mut url = Url::parse(RESOLVER_URL).context("invalid RESOLVER_URL")?;
    url.query_pairs_mut()
        .append_pair("access_token", access_token);
    Ok(url.to_string())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolver_url_includes_access_token_query() {
        let url = build_resolver_url_with_token("abc123").unwrap();
        assert_eq!(url, "http://127.0.0.1:8088/config/full?access_token=abc123");
    }

    #[test]
    fn resolver_url_percent_encodes_access_token() {
        let url = build_resolver_url_with_token("a b+c").unwrap();
        assert_eq!(
            url,
            "http://127.0.0.1:8088/config/full?access_token=a+b%2Bc"
        );
    }

    #[test]
    fn resolver_url_includes_uuid_access_token() {
        let url = build_resolver_url_with_token("a88759a2-b1b3-4af5-b395-987d3f2f4e50").unwrap();
        assert_eq!(
            url,
            "http://127.0.0.1:8088/config/full?access_token=a88759a2-b1b3-4af5-b395-987d3f2f4e50"
        );
    }
}
