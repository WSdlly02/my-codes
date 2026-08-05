use serde::Deserialize;
use serde_json::{Map, Value};

/// 解析器配置和相关数据模型
#[derive(Debug)]
pub struct ResolverConfig {
    pub airport_url: String,        // 机场订阅链接
    pub origin_config_path: String, // 原始配置文件路径
    pub access_token: String,       // 配置端点访问令牌
    pub subconverter_host: String,  // subconverter 服务地址
    pub port: u16,                  // 监听端口
    pub rules_url: String,          // 规则链接

    pub vps_configs: Box<[VpsConfig]>, // VPS 配置
}

/// Validated single VPS configuration used as the runtime source of truth.
#[derive(Debug)]
pub struct VpsConfig {
    pub(super) groups: Box<[String]>,
    pub(super) direct_rule: bool,
    pub(super) proxy: Map<String, Value>,
}

impl VpsConfig {
    pub fn groups(&self) -> &[String] {
        &self.groups
    }

    pub fn proxy(&self) -> &Map<String, Value> {
        &self.proxy
    }

    pub fn name(&self) -> &str {
        self.proxy_get("name")
    }

    pub fn direct_rule_server_ip(&self) -> Option<&str> {
        self.direct_rule.then(|| self.proxy_get("server"))
    }

    fn proxy_get(&self, field: &str) -> &str {
        self.proxy
            .get(field)
            .and_then(Value::as_str)
            .expect("VpsConfig is only constructed after proxy field validation")
    }
}

/// Config file structure, can contain multiple VPS configurations
///
/// Not used in runtime, only used for deserialization of the config file.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct VpsConfigFile {
    pub version: u32,
    pub vps: Box<[VpsToml]>,
}

#[derive(Debug, Deserialize)]
pub(super) struct VpsConfigFileVersion {
    pub version: u32,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct VpsToml {
    pub id: String,
    pub enabled: bool,
    #[serde(rename = "direct-rule")]
    pub direct_rule: bool,
    pub groups: Box<[String]>,
    #[serde(rename = "proxy-json")]
    pub proxy_json: String,
}
