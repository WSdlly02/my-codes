use std::{
    collections::{HashMap, HashSet},
    env, fs,
    net::IpAddr,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use serde_json::{Map, Value};
use tracing::warn;

use super::models::{ResolverConfig, VpsConfig, VpsConfigFile, VpsConfigFileVersion, VpsToml};

const BASE_CUSTOM_RULES: [&str; 3] = [
    "IP-CIDR,10.144.144.0/24,DIRECT,no-resolve", // easytier
    "IP-CIDR,100.64.0.0/10,DIRECT,no-resolve",   // tailscale
    "DOMAIN-SUFFIX,tailscale.com,DIRECT",
];

impl ResolverConfig {
    pub fn load() -> Result<Self> {
        let airport_url = load_env_required("AIRPORT_URL")?;
        let origin_config_path = load_env_required("ORIGIN_CONFIG_PATH")?;
        let access_token = load_env_required("ACCESS_TOKEN")?;
        let vps_configs_dir = PathBuf::from(load_env_required("VPS_CONFIGS_DIR")?);
        let subconverter_host = load_env_default("SUBCONVERTER_HOST", "http://127.0.0.1:25500");
        let port = load_env_default("RESOLVER_PORT", "8088")
            .parse::<u16>()
            .context("failed to parse RESOLVER_PORT")?;
        let rules_url = load_env_default(
            "RULES_URL",
            "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini",
        );

        let vps_configs = load_vps_configs(&vps_configs_dir)?;

        Ok(Self {
            airport_url,
            origin_config_path,
            access_token,
            subconverter_host,
            port,
            rules_url,
            vps_configs,
        })
    }

    /// Returns an iterator over the custom proxy configurations that should be included in the generated config.
    ///
    /// items are references to the `proxy` field of each `VpsConfig`.
    ///
    /// proxy is a JSON object with fields like `name`, `type`, `server`, etc.
    pub fn custom_proxies(&self) -> impl ExactSizeIterator<Item = &Map<String, Value>> {
        self.vps_configs.iter().map(VpsConfig::proxy)
    }

    /// Returns a list of custom rules that should be appended to the generated config.
    ///
    /// ["IP-CIDR,1.1.1.0/24,DIRECT,no-resolve", "IP-CIDR,8.8.8.8/32,DIRECT,no-resolve"]
    pub fn custom_rules(&self) -> Box<[String]> {
        BASE_CUSTOM_RULES
            .into_iter()
            .map(str::to_string)
            .chain(
                self.vps_configs
                    .iter()
                    .filter_map(VpsConfig::direct_rule_server_ip)
                    .map(|ip| format!("IP-CIDR,{ip}/32,DIRECT,no-resolve")),
            )
            .collect()
    }

    /// Returns a map of group names to the list of VPS node names that belong to that group.
    ///
    /// Example:
    /// {
    ///     "日本": ["JP ByteVirt VPS", "JP Hysteria2 VPS"],
    ///     "自动": ["JP ByteVirt VPS"],
    ///     "手动": ["JP Hysteria2 VPS"],
    /// }
    pub fn auto_group_map(&self) -> HashMap<String, Box<[String]>> {
        let mut groups: HashMap<String, Vec<String>> = HashMap::new();

        for proxy in &self.vps_configs {
            for group in proxy.groups() {
                groups
                    .entry(group.clone())
                    .or_default()
                    .push(proxy.name().to_string());
            }
        }

        groups
            .into_iter()
            .map(|(group, nodes)| (group, nodes.into_boxed_slice()))
            .collect()
    }
}

fn load_vps_configs(dir: &Path) -> Result<Box<[VpsConfig]>> {
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
        let file = parse_vps_config_file(&content, &path)?;

        for raw in file.vps {
            if !raw.enabled {
                continue;
            }

            let proxy = parse_proxy_json_obj(&raw, &path)?;
            let name = proxy_get(&proxy, "name", &raw.id, &path)?.to_string();
            if raw.direct_rule {
                let server = proxy_get(&proxy, "server", &raw.id, &path)?;
                if server.parse::<IpAddr>().is_err() {
                    bail!(
                        "VPS `{}` has direct-rule = true but proxy-json.server is not an IP address in {}",
                        raw.id,
                        path.display()
                    );
                }
            }

            validate_vps_metadata(&raw, &path)?;
            if !ids.insert(raw.id.clone()) {
                bail!("duplicate VPS id `{}` found in {}", raw.id, path.display());
            }
            if !names.insert(name.clone()) {
                bail!(
                    "duplicate VPS node name `{}` found in {}",
                    name,
                    path.display()
                );
            }

            configs.push(VpsConfig {
                groups: raw.groups,
                direct_rule: raw.direct_rule,
                proxy,
            });
        }
    }

    if configs.is_empty() {
        bail!(
            "no enabled VPS entries found in VPS_CONFIGS_DIR: {}",
            dir.display()
        );
    }

    Ok(configs.into_boxed_slice())
}

fn parse_vps_config_file(content: &str, path: &Path) -> Result<VpsConfigFile> {
    let version: VpsConfigFileVersion = toml::from_str(content)
        .with_context(|| format!("failed to parse VPS config: {}", path.display()))?;

    if version.version != 2 {
        bail!(
            "unsupported VPS config version {} in {}; expected version 2",
            version.version,
            path.display()
        );
    }

    let file: VpsConfigFile = toml::from_str(content)
        .with_context(|| format!("failed to parse version 2 VPS config: {}", path.display()))?;
    debug_assert_eq!(file.version, version.version);
    Ok(file)
}

fn parse_proxy_json_obj(raw: &VpsToml, path: &Path) -> Result<Map<String, Value>> {
    let proxy: Value = serde_json::from_str(&raw.proxy_json).with_context(|| {
        format!(
            "failed to parse proxy-json for VPS `{}` in {}",
            raw.id,
            path.display()
        )
    })?;

    match proxy {
        Value::Object(proxy) => Ok(proxy),
        _ => bail!(
            "proxy-json for VPS `{}` must be a JSON object in {}",
            raw.id,
            path.display()
        ),
    }
}

fn proxy_get<'a>(
    proxy: &'a Map<String, Value>,
    field: &str,
    id: &str,
    path: &Path,
) -> Result<&'a str> {
    match proxy.get(field).and_then(Value::as_str) {
        Some(value) if !value.trim().is_empty() => Ok(value),
        _ => bail!(
            "proxy-json.{field} for VPS `{id}` must be a non-empty string in {}",
            path.display()
        ),
    }
}

fn validate_vps_metadata(raw: &VpsToml, path: &Path) -> Result<()> {
    if raw.id.trim().is_empty() {
        bail!("empty VPS id in {}", path.display());
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
    Ok(())
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
        let vps_configs = load_vps_configs(Path::new("examples/vps-configs")).unwrap();

        assert_eq!(vps_configs.len(), 1);
        assert_eq!(vps_configs[0].name(), "JP ByteVirt VPS");
        assert_eq!(vps_configs[0].proxy()["server"], "203.0.113.10");
        assert_eq!(vps_configs[0].groups(), ["日本", "自动", "手动"]);
        assert_eq!(vps_configs[0].direct_rule_server_ip(), Some("203.0.113.10"));

        let config = ResolverConfig {
            airport_url: String::new(),
            origin_config_path: String::new(),
            access_token: String::new(),
            subconverter_host: String::new(),
            port: 0,
            rules_url: String::new(),
            vps_configs,
        };

        let proxies: Box<[_]> = config.custom_proxies().collect();
        assert_eq!(proxies[0]["type"], "vless");
        assert!(
            config
                .custom_rules()
                .contains(&"IP-CIDR,203.0.113.10/32,DIRECT,no-resolve".to_string())
        );
        let group_map = config.auto_group_map();
        assert_eq!(group_map["日本"].len(), 1);
        assert_eq!(group_map["日本"][0], "JP ByteVirt VPS");
    }

    #[test]
    fn requires_exact_vps_config_version() {
        let path = Path::new("vps.toml");

        let version_1 = r#"
version = 1

[[vps]]
id = "legacy"
enabled = true
direct-rule = true
groups = ["旧配置"]
name = "Legacy VLESS"
type = "vless"
server = "203.0.113.10"
"#;
        let error = parse_vps_config_file(version_1, path).unwrap_err();
        assert!(error.to_string().contains("expected version 2"));

        let error = parse_vps_config_file("vps = []", path).unwrap_err();
        assert!(error.to_string().contains("failed to parse VPS config"));
    }

    #[test]
    fn rejects_legacy_fields_in_version_2() {
        let content = r#"
version = 2

[[vps]]
id = "legacy"
enabled = true
direct-rule = false
groups = ["旧配置"]
name = "Legacy VLESS"
proxy-json = '''{"name":"Current JSON"}'''
"#;

        let error = parse_vps_config_file(content, Path::new("vps.toml")).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("failed to parse version 2 VPS config")
        );
    }

    #[test]
    fn accepts_protocol_specific_proxy_json() {
        let raw: VpsToml = toml::from_str(
            r#"
id = "jp-hysteria2"
enabled = true
direct-rule = false
groups = ["日本"]
proxy-json = '''
{
  "name": "JP Hysteria2 VPS",
  "type": "hysteria2",
  "server": "example.com",
  "port": 443,
  "password": "secret",
  "sni": "www.cloudflare.com",
  "skip-cert-verify": false
}
'''
"#,
        )
        .unwrap();

        let proxy = parse_proxy_json_obj(&raw, Path::new("hysteria2.toml")).unwrap();
        assert_eq!(proxy["type"], "hysteria2");
        assert_eq!(proxy["password"], "secret");
        assert_eq!(proxy["skip-cert-verify"], false);
    }
}
