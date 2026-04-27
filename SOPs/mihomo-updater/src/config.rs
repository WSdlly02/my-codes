use std::{
    collections::{HashMap, HashSet},
    env, fs,
    net::IpAddr,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use tracing::warn;

use super::models::{ResolverConfig, VpsConfig, VpsConfigFile, VpsToml};

impl ResolverConfig {
    pub fn load() -> Result<Self> {
        let airport_url = load_env_required("AIRPORT_URL")?;
        let origin_config_path = load_env_required("ORIGIN_CONFIG_PATH")?;
        let vps_configs_dir = PathBuf::from(load_env_required("VPS_CONFIGS_DIR")?);
        let subconverter_host = load_env_default("SUBCONVERTER_HOST", "http://127.0.0.1:25500");
        let port = load_env_default("RESOLVER_PORT", "8088")
            .parse::<u16>()
            .context("failed to parse RESOLVER_PORT")?;
        let rules_url = load_env_default(
            "RULES_URL",
            "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini",
        );

        let custom_proxies = load_vps_configs(&vps_configs_dir)?;
        let mut custom_rules: Vec<String> = vec![
            "IP-CIDR,100.64.0.0/10,DIRECT,no-resolve".to_string(),
            "DOMAIN-SUFFIX,tailscale.com,DIRECT".to_string(),
        ];
        custom_rules.extend(
            custom_proxies
                .iter()
                .filter(|node| node.direct_rule)
                .map(|node| format!("IP-CIDR,{}/32,DIRECT,no-resolve", node.ip)),
        );

        let auto_group_map = build_auto_group_map(&custom_proxies);

        Ok(Self {
            airport_url,
            origin_config_path,
            subconverter_host,
            port,
            rules_url,
            custom_proxies,
            custom_rules,
            auto_group_map,
        })
    }
}

fn load_vps_configs(dir: &Path) -> Result<Vec<VpsConfig>> {
    let entries = fs::read_dir(dir)
        .with_context(|| format!("failed to read VPS_CONFIGS_DIR: {}", dir.display()))?;

    let mut paths = Vec::new();
    for entry in entries {
        let entry = entry.with_context(|| format!("failed to read entry in {}", dir.display()))?;
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("toml") {
            paths.push(path);
        }
    }
    paths.sort();

    if paths.is_empty() {
        bail!(
            "no *.toml files found in VPS_CONFIGS_DIR: {}",
            dir.display()
        );
    }

    let mut configs = Vec::new();
    let mut ids = HashSet::new();
    let mut names = HashSet::new();

    for path in paths {
        let content = fs::read_to_string(&path)
            .with_context(|| format!("failed to read VPS config: {}", path.display()))?;
        let file: VpsConfigFile = toml::from_str(&content)
            .with_context(|| format!("failed to parse VPS config: {}", path.display()))?;

        if let Some(version) = file.version
            && version != 1
        {
            bail!(
                "unsupported VPS config version {version} in {}",
                path.display()
            );
        }

        for raw in file.vps {
            if !raw.enabled {
                continue;
            }

            validate_vps_toml(&raw, &path)?;
            if !ids.insert(raw.id.clone()) {
                bail!("duplicate VPS id `{}` found in {}", raw.id, path.display());
            }
            if !names.insert(raw.name.clone()) {
                bail!(
                    "duplicate VPS node name `{}` found in {}",
                    raw.name,
                    path.display()
                );
            }

            configs.push(VpsConfig {
                name: raw.name,
                uuid: raw.uuid,
                ip: raw.server,
                port: raw.port,
                public_key: raw.reality_opts.public_key,
                short_id: raw.reality_opts.short_id,
                kind: raw.kind,
                flow: raw.flow,
                udp: raw.udp,
                tls: raw.tls,
                servername: raw.servername,
                client_fingerprint: raw.client_fingerprint,
                groups: raw.groups,
                direct_rule: raw.direct_rule,
            });
        }
    }

    if configs.is_empty() {
        bail!(
            "no enabled VPS entries found in VPS_CONFIGS_DIR: {}",
            dir.display()
        );
    }

    Ok(configs)
}

fn validate_vps_toml(raw: &VpsToml, path: &Path) -> Result<()> {
    if raw.id.trim().is_empty() {
        bail!("empty VPS id in {}", path.display());
    }
    if raw.name.trim().is_empty() {
        bail!("empty VPS name for id `{}` in {}", raw.id, path.display());
    }
    if raw.server.trim().is_empty() {
        bail!("empty VPS server for id `{}` in {}", raw.id, path.display());
    }
    if raw.uuid.trim().is_empty() {
        bail!("empty VPS uuid for id `{}` in {}", raw.id, path.display());
    }
    if raw.groups.is_empty() || raw.groups.iter().any(|group| group.trim().is_empty()) {
        bail!(
            "VPS `{}` must declare at least one non-empty group in {}",
            raw.id,
            path.display()
        );
    }
    let mut groups = HashSet::new();
    for group in &raw.groups {
        if !groups.insert(group) {
            bail!(
                "duplicate group `{}` for VPS `{}` in {}",
                group,
                raw.id,
                path.display()
            );
        }
    }
    if raw.kind.trim().is_empty() {
        bail!("empty VPS type for id `{}` in {}", raw.id, path.display());
    }
    if raw.flow.trim().is_empty() {
        bail!("empty VPS flow for id `{}` in {}", raw.id, path.display());
    }
    if raw.servername.trim().is_empty() {
        bail!(
            "empty VPS servername for id `{}` in {}",
            raw.id,
            path.display()
        );
    }
    if raw.client_fingerprint.trim().is_empty() {
        bail!(
            "empty VPS client_fingerprint for id `{}` in {}",
            raw.id,
            path.display()
        );
    }
    if raw.reality_opts.public_key.trim().is_empty() {
        bail!(
            "empty reality_opts.public_key for id `{}` in {}",
            raw.id,
            path.display()
        );
    }
    if raw.reality_opts.short_id.trim().is_empty() {
        bail!(
            "empty reality_opts.short_id for id `{}` in {}",
            raw.id,
            path.display()
        );
    }
    if raw.direct_rule && raw.server.parse::<IpAddr>().is_err() {
        bail!(
            "VPS `{}` has direct_rule = true but server is not an IP address in {}",
            raw.id,
            path.display()
        );
    }
    Ok(())
}

fn build_auto_group_map(proxies: &[VpsConfig]) -> HashMap<String, Vec<String>> {
    let mut groups: HashMap<String, Vec<String>> = HashMap::new();

    for proxy in proxies {
        for group in &proxy.groups {
            groups
                .entry(group.clone())
                .or_default()
                .push(proxy.name.clone());
        }
    }

    groups
}

fn load_env_required(key: &str) -> Result<String> {
    match env::var(key) {
        Ok(value) if !value.trim().is_empty() => Ok(value),
        _ => bail!("配置错误: 必需的环境变量 {key} 未设置或为空"),
    }
}

fn load_env_default(key: &str, default: &str) -> String {
    match env::var(key) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            warn!("环境变量 {key} 未设置或为空，使用默认值: {default}");
            default.to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_vps_configs_from_drop_in_toml_dir() {
        let configs = load_vps_configs(Path::new("examples/vps-configs")).unwrap();

        assert_eq!(configs.len(), 1);
        assert_eq!(configs[0].name, "JP ByteVirt VPS");
        assert_eq!(configs[0].ip, "203.0.113.10");
        assert_eq!(configs[0].groups, ["日本", "自动", "手动"]);
        assert!(configs[0].direct_rule);
    }
}
