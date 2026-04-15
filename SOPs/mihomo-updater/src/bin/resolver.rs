use std::{collections::HashMap, env, net::SocketAddr, process::Stdio, sync::Arc, time::Duration};

use anyhow::{Context, Result, bail};
use axum::{
    Router,
    extract::State,
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    routing::get,
};
use mimalloc::MiMalloc;
use reqwest::Client;
use serde::Serialize;
use serde_json::to_string;
use tokio::{io::AsyncWriteExt, process::Command};
use tracing::{error, info, warn};
use url::Url;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Clone)]
struct AppState {
    client: Client,
    config: Arc<ResolverConfig>,
}

#[derive(Clone, Debug)]
struct ResolverConfig {
    airport_url: String,
    origin_config_path: String,
    subconverter_host: String,
    port: u16,
    rules_url: String,
    custom_proxies: Vec<VpsConfig>,
    custom_rules: Vec<String>,
    auto_group_map: HashMap<String, Vec<String>>,
}

#[derive(Clone, Debug)]
struct VpsConfig {
    name: String,
    uuid: String,
    ip: String,
    port: u16,
    public_key: String,
    short_id: String,
}

#[derive(Debug)]
struct AppError(anyhow::Error);

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        error!("{:#}", self.0);
        (StatusCode::INTERNAL_SERVER_ERROR, self.0.to_string()).into_response()
    }
}

impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(value: E) -> Self {
        Self(value.into())
    }
}

#[derive(Serialize)]
struct RenderedProxy<'a> {
    name: &'a str,
    #[serde(rename = "type")]
    kind: &'static str,
    uuid: &'a str,
    server: &'a str,
    port: u16,
    flow: &'static str,
    udp: bool,
    tls: bool,
    servername: &'static str,
    #[serde(rename = "client-fingerprint")]
    client_fingerprint: &'static str,
    #[serde(rename = "reality-opts")]
    reality_opts: RealityOpts<'a>,
}

#[derive(Serialize)]
struct RealityOpts<'a> {
    #[serde(rename = "public-key")]
    public_key: &'a str,
    #[serde(rename = "short-id")]
    short_id: &'a str,
}

#[tokio::main]
async fn main() -> Result<()> {
    let config = ResolverConfig::load()?;
    let client = Client::builder()
        .timeout(Duration::from_secs(30))
        .build()
        .context("failed to build http client")?;

    let state = AppState {
        client,
        config: Arc::new(config),
    };

    let app = Router::new()
        .route("/health", get(health))
        .route("/config/minimal", get(handle_minimal))
        .route("/config/full", get(handle_full))
        .with_state(state.clone());

    let addr = SocketAddr::from(([0, 0, 0, 0], state.config.port));
    info!("server listening on {addr}");

    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .context("failed to bind tcp listener")?;

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .context("server stopped with error")
}

async fn shutdown_signal() {
    #[cfg(unix)]
    {
        use tokio::signal::unix::{SignalKind, signal};

        let mut term = signal(SignalKind::terminate()).expect("install SIGTERM handler");
        let mut quit = signal(SignalKind::quit()).expect("install SIGQUIT handler");

        tokio::select! {
            _ = tokio::signal::ctrl_c() => {}
            _ = term.recv() => {}
            _ = quit.recv() => {}
        }
    }

    info!("shutdown signal received");
}

async fn health() -> &'static str {
    "OK"
}

async fn handle_minimal(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Response, AppError> {
    if !validate_user_agent(&headers) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    info!("handling minimal config request");

    let generated = generate_config(&state).await?;
    Ok(config_response(generated))
}

async fn handle_full(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Response, AppError> {
    if !validate_user_agent(&headers) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    info!("handling full config request");

    let generated = generate_config(&state).await?;
    let merged = merge_with_origin(&generated, &state.config.origin_config_path).await?;
    Ok(config_response(merged))
}

fn validate_user_agent(headers: &HeaderMap) -> bool {
    let ua = headers
        .get(axum::http::header::USER_AGENT)
        .and_then(|value| value.to_str().ok())
        .unwrap_or_default();

    if ["Clash", "ClashMeta", "mihomo"]
        .iter()
        .any(|allowed| ua.contains(allowed))
    {
        return true;
    }

    warn!("blocked request with user-agent: {ua}");
    false
}

fn config_response(body: Vec<u8>) -> Response {
    (
        [(
            axum::http::header::CONTENT_TYPE,
            "application/x-yaml; charset=utf-8",
        )],
        body,
    )
        .into_response()
}

async fn generate_config(state: &AppState) -> Result<Vec<u8>> {
    let url = build_subconverter_url(&state.config)?;
    info!("fetching from subconverter: {url}");

    let bytes = fetch_url(&state.client, &url).await?;
    if bytes.len() < 100 {
        bail!("response too short");
    }

    let custom_proxies_json = render_proxies_json(&state.config.custom_proxies)?;
    let custom_rules_json =
        to_string(&state.config.custom_rules).context("failed to serialize custom rules")?;

    let mut filter_parts = vec![
        format!(
            r#"{custom_proxies_json} as $new | .proxies |= (map(select([.name] - ($new | map(.name)) | length > 0)) + $new)"#
        ),
        format!(r#".rules = {custom_rules_json} + .rules"#),
    ];

    for (keyword, nodes) in &state.config.auto_group_map {
        let nodes_json = to_string(nodes).context("failed to serialize group nodes")?;
        filter_parts.push(format!(
            r#".["proxy-groups"][] |= (select(.name | test("{keyword}")) | .proxies = {nodes_json} + (.proxies - {nodes_json}))"#
        ));
    }

    let filter = filter_parts.join(" | ");
    run_yq("eval", &bytes, &filter, &["-"]).await
}

async fn fetch_url(client: &Client, target: &str) -> Result<Vec<u8>> {
    let response = client
        .get(target)
        .header(reqwest::header::USER_AGENT, "Clash/Meta")
        .send()
        .await
        .with_context(|| format!("request failed: {target}"))?;

    let status = response.status();
    if !status.is_success() {
        bail!("http error {status} while fetching {target}");
    }

    response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .with_context(|| format!("failed to read response body: {target}"))
}

fn build_subconverter_url(config: &ResolverConfig) -> Result<String> {
    let mut url = Url::parse(&format!(
        "{}/sub",
        config.subconverter_host.trim_end_matches('/')
    ))
    .context("invalid SUBCONVERTER_HOST")?;

    {
        let mut pairs = url.query_pairs_mut();
        pairs.append_pair("target", "clash");
        pairs.append_pair("url", &config.airport_url);
        pairs.append_pair("config", &config.rules_url);
        pairs.append_pair("insert", "true");
        pairs.append_pair("emoji", "true");
        pairs.append_pair("list", "false");
        pairs.append_pair("tfo", "true");
        pairs.append_pair("scv", "false");
        pairs.append_pair("fdn", "true");
        pairs.append_pair("expand", "true");
        pairs.append_pair("sort", "false");
        pairs.append_pair("udp", "true");
        pairs.append_pair("new_name", "true");
    }

    Ok(url.to_string())
}

async fn merge_with_origin(generated: &[u8], origin_path: &str) -> Result<Vec<u8>> {
    let filter = r#"select(fileIndex == 0) as $origin | select(fileIndex == 1) as $gen | $origin | .proxies = $gen.proxies | .["proxy-groups"] = $gen.["proxy-groups"] | .rules = $gen.rules"#;
    run_yq("eval-all", generated, filter, &[origin_path, "-"]).await
}

fn render_proxies_json(proxies: &[VpsConfig]) -> Result<String> {
    let rendered: Vec<_> = proxies
        .iter()
        .map(|proxy| RenderedProxy {
            name: &proxy.name,
            kind: "vless",
            uuid: &proxy.uuid,
            server: &proxy.ip,
            port: proxy.port,
            flow: "xtls-rprx-vision",
            udp: true,
            tls: true,
            servername: "www.cloudflare.com",
            client_fingerprint: "chrome",
            reality_opts: RealityOpts {
                public_key: &proxy.public_key,
                short_id: &proxy.short_id,
            },
        })
        .collect();

    to_string(&rendered).context("failed to serialize custom proxies")
}

async fn run_yq(mode: &str, input: &[u8], expression: &str, args: &[&str]) -> Result<Vec<u8>> {
    let mut command = Command::new("yq");
    command.arg(mode).arg("-o=json").arg(expression).args(args);
    command.stdin(Stdio::piped());
    command.stdout(Stdio::piped());
    command.stderr(Stdio::piped());

    let mut child = command
        .spawn()
        .with_context(|| format!("failed to start yq {mode}"))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(input)
            .await
            .with_context(|| format!("failed to write stdin to yq {mode}"))?;
    }

    let output = child
        .wait_with_output()
        .await
        .with_context(|| format!("failed to wait for yq {mode}"))?;

    if !output.status.success() {
        bail!(
            "yq {mode} failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }

    Ok(output.stdout)
}

impl ResolverConfig {
    fn load() -> Result<Self> {
        let airport_url = load_env_required("AIRPORT_URL")?;
        let origin_config_path = load_env_required("ORIGIN_CONFIG_PATH")?;
        let subconverter_host = load_env_default("SUBCONVERTER_HOST", "http://127.0.0.1:25500");
        let port = load_env_default("RESOLVER_PORT", "8088")
            .parse::<u16>()
            .context("failed to parse RESOLVER_PORT")?;
        let rules_url = load_env_default(
            "RULES_URL",
            "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini",
        );

        let jp_vps = VpsConfig {
            name: "🇯🇵 日本 ByteVirt VPS".to_string(),
            uuid: load_env_required("JP_BYTEVIRT_VPS_UUID")?,
            ip: load_env_required("JP_BYTEVIRT_VPS_IP")?,
            port: load_env_required("JP_BYTEVIRT_VPS_PORT")?
                .parse::<u16>()
                .context("failed to parse JP_BYTEVIRT_VPS_PORT")?,
            public_key: load_env_required("JP_BYTEVIRT_VPS_PUBKEY")?,
            short_id: load_env_required("JP_BYTEVIRT_VPS_SHORT_ID")?,
        };

        let nl_vps = VpsConfig {
            name: "🇳🇱 荷兰 ExtraVM VPS".to_string(),
            uuid: load_env_required("NL_EXTRAVM_VPS_UUID")?,
            ip: load_env_required("NL_EXTRAVM_VPS_IP")?,
            port: load_env_required("NL_EXTRAVM_VPS_PORT")?
                .parse::<u16>()
                .context("failed to parse NL_EXTRAVM_VPS_PORT")?,
            public_key: load_env_required("NL_EXTRAVM_VPS_PUBKEY")?,
            short_id: load_env_required("NL_EXTRAVM_VPS_SHORT_ID")?,
        };

        let custom_proxies: Vec<VpsConfig> = vec![jp_vps.clone(), nl_vps.clone()];
        let mut custom_rules: Vec<String> = vec![
            "IP-CIDR,100.64.0.0/10,DIRECT,no-resolve".to_string(),
            "DOMAIN-SUFFIX,tailscale.com,DIRECT".to_string(),
        ];
        custom_rules.extend(
            custom_proxies
                .iter()
                .map(|node| format!("IP-CIDR,{}/32,DIRECT,no-resolve", node.ip)),
        );

        let auto_group_map: HashMap<String, Vec<String>> = HashMap::from([
            ("日本".to_string(), vec![jp_vps.name.clone()]),
            ("荷兰".to_string(), vec![nl_vps.name.clone()]),
            (
                "自动".to_string(),
                vec![jp_vps.name.clone(), nl_vps.name.clone()],
            ),
            (
                "手动".to_string(),
                vec![jp_vps.name.clone(), nl_vps.name.clone()],
            ),
        ]);

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
