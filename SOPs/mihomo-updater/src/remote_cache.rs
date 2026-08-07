//! Seven-day file cache shared by airport configuration and ACL4SSR rule lists.
//!
//! Successful fetches always win. The cache is only a fallback when an upstream fetch fails.

use std::{
    fs::{File, FileTimes, OpenOptions},
    io::Write,
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use anyhow::{Context, Result, bail};
use tempfile::NamedTempFile;
use tokio::fs;
use tracing::{info, warn};

#[derive(Debug, Clone)]
pub struct RemoteFileCache {
    root: PathBuf,
    ttl: Duration,
}

impl RemoteFileCache {
    pub async fn new(root: PathBuf, ttl: Duration) -> Result<Self> {
        fs::create_dir_all(&root)
            .await
            .with_context(|| format!("failed to create cache directory: {}", root.display()))?;
        fs::set_permissions(&root, std::fs::Permissions::from_mode(0o700))
            .await
            .with_context(|| format!("failed to secure cache directory: {}", root.display()))?;

        Ok(Self { root, ttl })
    }

    /// Returns validated upstream bytes, or a still-fresh cached copy after an upstream failure.
    pub async fn resolve(
        &self,
        namespace: &str,
        extension: &str,
        source: &str,
        upstream: Result<Vec<u8>>,
    ) -> Result<Vec<u8>> {
        let path = self.cache_path(namespace, extension, source);

        match upstream {
            Ok(bytes) => {
                // Equal content refreshes its mtime; changed content is replaced atomically.
                self.store_success(&path, &bytes).await?;
                Ok(bytes)
            }
            // A stale, missing, or unreadable cache never hides the original upstream failure.
            Err(upstream_error) => match self.read_fresh(&path).await {
                Ok(Some(bytes)) => {
                    warn!(
                        "remote source unavailable, using cached file {}: {upstream_error:#}",
                        path.display()
                    );
                    Ok(bytes)
                }
                Ok(None) => Err(upstream_error).with_context(|| {
                    format!("no cache file available for remote namespace: {namespace}")
                }),
                Err(cache_error) => Err(upstream_error).with_context(|| {
                    format!(
                        "remote source failed and cache {} is unusable: {cache_error:#}",
                        path.display()
                    )
                }),
            },
        }
    }

    fn cache_path(&self, namespace: &str, extension: &str, source: &str) -> PathBuf {
        let source_hash = hex_digest(source.as_bytes());
        self.root
            .join(namespace)
            .join(format!("{source_hash}.{extension}"))
    }

    async fn read_fresh(&self, path: &Path) -> Result<Option<Vec<u8>>> {
        let metadata = match fs::metadata(path).await {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(error) => {
                return Err(error)
                    .with_context(|| format!("failed to inspect cache file: {}", path.display()));
            }
        };

        let modified = metadata
            .modified()
            .with_context(|| format!("failed to read cache timestamp: {}", path.display()))?;
        let age = SystemTime::now()
            .duration_since(modified)
            .with_context(|| format!("cache timestamp is in the future: {}", path.display()))?;

        if age > self.ttl {
            bail!(
                "cache file is older than {} seconds: {}",
                self.ttl.as_secs(),
                path.display()
            );
        }

        fs::read(path)
            .await
            .map(Some)
            .with_context(|| format!("failed to read cache file: {}", path.display()))
    }

    async fn store_success(&self, path: &Path, bytes: &[u8]) -> Result<()> {
        let parent = path
            .parent()
            .context("cache path does not have a parent directory")?;
        fs::create_dir_all(parent)
            .await
            .with_context(|| format!("failed to create cache namespace: {}", parent.display()))?;
        fs::set_permissions(parent, std::fs::Permissions::from_mode(0o700))
            .await
            .with_context(|| format!("failed to secure cache namespace: {}", parent.display()))?;

        let unchanged = match fs::read(path).await {
            Ok(existing) => hex_digest(&existing) == hex_digest(bytes),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => false,
            Err(error) => {
                return Err(error)
                    .with_context(|| format!("failed to read cache file: {}", path.display()));
            }
        };

        if unchanged {
            // mtime is the TTL timestamp, so a successful unchanged fetch starts a new TTL window.
            touch(path).await?;
            info!(
                "remote source unchanged; refreshed cache TTL: {}",
                path.display()
            );
            return Ok(());
        }

        atomic_replace(path.to_path_buf(), bytes.to_vec()).await?;
        info!("updated remote cache file: {}", path.display());
        Ok(())
    }
}

fn hex_digest(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

async fn touch(path: &Path) -> Result<()> {
    let path = path.to_path_buf();
    tokio::task::spawn_blocking(move || {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .with_context(|| format!("failed to open cache file: {}", path.display()))?;
        file.set_times(FileTimes::new().set_modified(SystemTime::now()))
            .with_context(|| format!("failed to refresh cache timestamp: {}", path.display()))
    })
    .await
    .context("cache timestamp task failed")?
}

async fn atomic_replace(path: PathBuf, bytes: Vec<u8>) -> Result<()> {
    tokio::task::spawn_blocking(move || {
        let parent = path
            .parent()
            .context("cache path does not have a parent directory")?;
        let mut temporary = NamedTempFile::new_in(parent)
            .with_context(|| format!("failed to create temporary file in {}", parent.display()))?;
        temporary
            .write_all(&bytes)
            .with_context(|| format!("failed to write temporary cache for {}", path.display()))?;
        temporary
            .as_file_mut()
            .sync_all()
            .with_context(|| format!("failed to sync temporary cache for {}", path.display()))?;
        temporary
            .persist(&path)
            .map_err(|error| error.error)
            .with_context(|| format!("failed to replace cache file: {}", path.display()))?;

        File::open(parent)
            .and_then(|directory| directory.sync_all())
            .with_context(|| format!("failed to sync cache directory: {}", parent.display()))
    })
    .await
    .context("atomic cache write task failed")?
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{FileTimes, OpenOptions},
        time::{Duration, SystemTime},
    };

    use anyhow::anyhow;

    use super::RemoteFileCache;

    #[tokio::test]
    async fn successful_fetch_updates_and_failure_uses_fresh_cache() {
        let directory = tempfile::tempdir().unwrap();
        let cache = RemoteFileCache::new(directory.path().to_path_buf(), Duration::from_secs(60))
            .await
            .unwrap();

        let first = cache
            .resolve(
                "rules",
                "list",
                "https://example.com/rules.list",
                Ok(b"DOMAIN,example.com".to_vec()),
            )
            .await
            .unwrap();
        assert_eq!(first, b"DOMAIN,example.com");

        let fallback = cache
            .resolve(
                "rules",
                "list",
                "https://example.com/rules.list",
                Err(anyhow!("offline")),
            )
            .await
            .unwrap();
        assert_eq!(fallback, first);
    }

    #[tokio::test]
    async fn stale_cache_is_rejected() {
        let directory = tempfile::tempdir().unwrap();
        let cache = RemoteFileCache::new(directory.path().to_path_buf(), Duration::ZERO)
            .await
            .unwrap();

        cache
            .resolve(
                "airport",
                "yaml",
                "https://example.com/sub",
                Ok(b"proxies: []".to_vec()),
            )
            .await
            .unwrap();

        let error = cache
            .resolve(
                "airport",
                "yaml",
                "https://example.com/sub",
                Err(anyhow!("offline")),
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("cache"));
    }

    #[tokio::test]
    async fn unchanged_success_refreshes_ttl_without_replacing_content() {
        let directory = tempfile::tempdir().unwrap();
        let cache = RemoteFileCache::new(directory.path().to_path_buf(), Duration::from_secs(60))
            .await
            .unwrap();
        let source = "https://example.com/sub";
        let bytes = b"proxies: [node]".to_vec();

        cache
            .resolve("airport", "yaml", source, Ok(bytes.clone()))
            .await
            .unwrap();
        let path = cache.cache_path("airport", "yaml", source);
        let old_time = SystemTime::now() - Duration::from_secs(120);
        OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .unwrap()
            .set_times(FileTimes::new().set_modified(old_time))
            .unwrap();

        cache
            .resolve("airport", "yaml", source, Ok(bytes.clone()))
            .await
            .unwrap();
        let fallback = cache
            .resolve("airport", "yaml", source, Err(anyhow!("offline")))
            .await
            .unwrap();

        assert_eq!(fallback, bytes);
    }
}
