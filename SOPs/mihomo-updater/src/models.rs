use std::collections::HashMap;

use serde::{Deserialize, Serialize};

// 解析器配置和相关数据模型
#[derive(Clone, Debug)]
pub struct ResolverConfig {
    pub airport_url: String,                          // 机场订阅链接
    pub origin_config_path: String,                   // 原始配置文件路径
    pub subconverter_host: String,                    // subconverter 服务地址
    pub port: u16,                                    // 监听端口
    pub rules_url: String,                            // 规则链接
    pub custom_proxies: Vec<VpsConfig>,               // 自定义代理配置
    pub custom_rules: Vec<String>,                    // 自定义规则
    pub auto_group_map: HashMap<String, Vec<String>>, // 自动分组映射
}

#[derive(Clone, Debug)]
pub struct VpsConfig {
    pub name: String,
    pub uuid: String,
    pub ip: String,
    pub port: u16,
    pub public_key: String,
    pub short_id: String,
    pub kind: String,
    pub flow: String,
    pub udp: bool,
    pub tls: bool,
    pub servername: String,
    pub client_fingerprint: String,
    pub groups: Vec<String>,
    pub direct_rule: bool,
}

#[derive(Debug, Deserialize)]
pub(super) struct VpsConfigFile {
    pub version: Option<u32>,
    #[serde(default)]
    pub vps: Vec<VpsToml>,
}

#[derive(Debug, Deserialize)]
pub(super) struct VpsToml {
    pub id: String,
    #[serde(default = "default_true")]
    pub enabled: bool,
    pub name: String,
    #[serde(default = "default_proxy_type", rename = "type")]
    pub kind: String,
    pub uuid: String,
    pub server: String,
    pub port: u16,
    #[serde(default = "default_flow")]
    pub flow: String,
    #[serde(default = "default_true")]
    pub udp: bool,
    #[serde(default = "default_true")]
    pub tls: bool,
    #[serde(default = "default_servername")]
    pub servername: String,
    #[serde(default = "default_client_fingerprint")]
    pub client_fingerprint: String,
    #[serde(rename = "reality-opts", alias = "reality_opts")]
    pub reality_opts: RealityOptsToml,
    pub groups: Vec<String>,
    #[serde(default = "default_true")]
    pub direct_rule: bool,
}

#[derive(Debug, Deserialize)]
pub(super) struct RealityOptsToml {
    #[serde(rename = "public-key", alias = "public_key")]
    pub public_key: String,
    #[serde(rename = "short-id", alias = "short_id")]
    pub short_id: String,
}

#[derive(Serialize)]
pub struct RenderedProxy<'a> {
    pub name: &'a str,
    #[serde(rename = "type")]
    pub kind: &'a str,
    pub uuid: &'a str,
    pub server: &'a str,
    pub port: u16,
    pub flow: &'a str,
    pub udp: bool,
    pub tls: bool,
    pub servername: &'a str,
    #[serde(rename = "client-fingerprint")]
    pub client_fingerprint: &'a str,
    #[serde(rename = "reality-opts")]
    pub reality_opts: RealityOpts<'a>,
}

#[derive(Serialize)]
pub struct RealityOpts<'a> {
    #[serde(rename = "public-key")]
    pub public_key: &'a str,
    #[serde(rename = "short-id")]
    pub short_id: &'a str,
}

fn default_true() -> bool {
    true
}

fn default_proxy_type() -> String {
    "vless".to_string()
}

fn default_flow() -> String {
    "xtls-rprx-vision".to_string()
}

fn default_servername() -> String {
    "www.cloudflare.com".to_string()
}

fn default_client_fingerprint() -> String {
    "chrome".to_string()
}
