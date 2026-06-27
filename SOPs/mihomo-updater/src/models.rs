use std::collections::HashMap;

use serde::{Deserialize, Serialize};

// 解析器配置和相关数据模型
#[derive(Clone, Debug)]
pub struct ResolverConfig {
    pub airport_url: String,                          // 机场订阅链接
    pub origin_config_path: String,                   // 原始配置文件路径
    pub access_token: String,                         // 配置端点访问令牌
    pub subconverter_host: String,                    // subconverter 服务地址
    pub port: u16,                                    // 监听端口
    pub rules_url: String,                            // 规则链接
    pub custom_proxies: Vec<VpsConfig>,               // 自定义代理配置
    pub custom_rules: Vec<String>,                    // 自定义规则
    pub auto_group_map: HashMap<String, Vec<String>>, // 自动分组映射
}

#[derive(Clone, Debug)]
pub struct VpsConfig {
    pub direct_rule: bool,
    pub groups: Vec<String>,

    pub name: String,
    pub kind: String, // alias for "type"
    pub server: String,
    pub port: u16,
    pub uuid: String,

    pub flow: String,
    pub packet_encoding: String,
    pub network: String,
    pub udp: bool,
    pub tls: bool,
    pub servername: String,
    pub client_fingerprint: String,

    pub public_key: String,
    pub short_id: String,
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
    #[serde(rename = "direct-rule")]
    pub direct_rule: bool,
    pub groups: Vec<String>,

    pub name: String,
    #[serde(rename = "type")]
    pub kind: String,
    pub server: String,
    pub port: u16,
    pub uuid: String,

    pub flow: String,
    #[serde(rename = "packet-encoding")]
    pub packet_encoding: String,
    pub network: String,
    pub udp: bool,
    pub tls: bool,
    pub servername: String,
    #[serde(rename = "client-fingerprint")]
    pub client_fingerprint: String,

    #[serde(rename = "reality-opts")]
    pub reality_opts: RealityOptsToml,
}

#[derive(Debug, Deserialize)]
pub(super) struct RealityOptsToml {
    #[serde(rename = "public-key")]
    pub public_key: String,
    #[serde(rename = "short-id")]
    pub short_id: String,
}

#[derive(Serialize)]
pub struct RenderedProxy<'a> {
    pub name: &'a str,
    #[serde(rename = "type")]
    pub kind: &'a str,
    pub server: &'a str,
    pub port: u16,
    pub uuid: &'a str,

    pub flow: &'a str,
    #[serde(rename = "packet-encoding")]
    pub packet_encoding: &'a str,
    pub network: &'a str,
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
