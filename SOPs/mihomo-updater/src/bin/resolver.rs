//! HTTP service that replaces the required subconverter pipeline.
//!
//! Airport proxy objects remain opaque. This service only generates proxy groups and rules,
//! then delegates YAML extraction and merging to yq.

use std::{net::SocketAddr, sync::Arc, time::Duration};

use anyhow::{Context, Result, bail};
use axum::{
    Router,
    extract::{Query, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    routing::get,
};
use mihomo_updater::{
    acl4ssr::{Acl4SsrConfig, RenderedProxyGroup},
    models::ResolverConfig,
    remote_cache::RemoteFileCache,
    yq::{ensure_yq_available, run_yq},
};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use tracing::{error, info, warn};

const REMOTE_CONFIG_CACHE_TTL: Duration = Duration::from_secs(7 * 24 * 60 * 60); // 7 days

#[derive(Clone)]
struct AppState {
    acl4ssr: Arc<Acl4SsrConfig>,
    cache: RemoteFileCache,
    client: Client,
    resolver_config: Arc<ResolverConfig>,
}

#[derive(Deserialize)]
struct AccessQuery {
    access_token: Option<String>,
}

#[derive(Serialize)]
struct GeneratedSections<'a> {
    proxies: Box<[&'a Map<String, Value>]>,
    #[serde(rename = "proxy-groups")]
    proxy_groups: Box<[RenderedProxyGroup]>,
    rules: Box<[String]>,
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

#[tokio::main]
async fn main() -> Result<()> {
    // Fail at startup instead of reporting a misleading remote-cache error on the first request.
    ensure_yq_available().await?;

    let config = ResolverConfig::load()?;
    let acl4ssr = Acl4SsrConfig::load(&config.acl4ssr_config_path)?;
    let cache = RemoteFileCache::new(config.cache_dir.clone(), REMOTE_CONFIG_CACHE_TTL).await?;
    let client = Client::builder()
        .timeout(Duration::from_secs(30))
        .build()
        .context("failed to build http client")?;

    let state = AppState {
        acl4ssr: Arc::new(acl4ssr),
        cache,
        client,
        resolver_config: Arc::new(config),
    };

    let addr = format!("0.0.0.0:{}", state.resolver_config.port);
    let addr = addr
        .parse::<SocketAddr>()
        .context("failed to parse socket address")?;

    let app = Router::new()
        .route("/health", get(health))
        .route("/config/minimal", get(handle_minimal))
        .route("/config/full", get(handle_full))
        .with_state(state);

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
    Query(query): Query<AccessQuery>,
    headers: HeaderMap,
) -> Result<Response, AppError> {
    if !validate_access_token(&query, &state.resolver_config.access_token) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    if !validate_user_agent(&headers) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    info!("handling minimal config request");

    let generated = generate_config(&state).await?;
    Ok(config_response(generated))
}

async fn handle_full(
    State(state): State<AppState>,
    Query(query): Query<AccessQuery>,
    headers: HeaderMap,
) -> Result<Response, AppError> {
    if !validate_access_token(&query, &state.resolver_config.access_token) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    if !validate_user_agent(&headers) {
        return Ok((StatusCode::FORBIDDEN, "Forbidden").into_response());
    }
    info!("handling full config request");

    let generated = generate_config(&state).await?;
    let merged = merge_with_origin(&generated, &state.resolver_config.origin_config_path).await?;
    Ok(config_response(merged))
}

fn validate_access_token(query: &AccessQuery, expected: &str) -> bool {
    if query.access_token.as_deref() == Some(expected) {
        return true;
    }

    warn!("blocked request with missing or invalid access_token");
    false
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
    let airport_config = fetch_airport_config_with_fallback(state).await?;

    // ACL4SSR only needs names for regex group matching. Node fields stay untouched in YAML.
    let airport_node_names = extract_airport_node_names(&airport_config).await?;

    // Render proxy groups
    // Example: { "name": "ACL4SSR Group", "type": "select", "proxies": ["Airport Hysteria2", "HongKong V2Ray"] }
    let proxy_groups = state
        .acl4ssr
        .render_groups(&airport_node_names, state.resolver_config.auto_group_map())?;

    // Render rules
    // Example: ["IP-CIDR,127.0.0.0/8,DIRECT,no-resolve", "DOMAIN-SUFFIX,example.com,ACL4SSR Group"]
    let acl_rules = state
        .acl4ssr
        .render_rules(&state.client, &state.cache)
        .await?;

    // Local direct rules take precedence over the downloaded ACL4SSR rules.
    let mut rules = state.resolver_config.custom_rules().into_vec();
    rules.extend(acl_rules);

    // These are the only sections owned by the resolver; yq preserves everything else.
    let sections = GeneratedSections {
        proxies: state.resolver_config.custom_proxies().collect(),
        proxy_groups,
        rules: rules.into_boxed_slice(),
    };
    let sections =
        serde_json::to_vec(&sections).context("failed to serialize generated sections")?;
    merge_generated_sections(&airport_config, &sections).await
}

async fn fetch_airport_config_with_fallback(state: &AppState) -> Result<Vec<u8>> {
    info!("fetching airport config");

    // Invalid upstream YAML must never replace a previously usable cache entry.
    let upstream = match fetch_url(&state.client, &state.resolver_config.airport_url).await {
        Ok(bytes) => match extract_airport_node_names(&bytes).await {
            Ok(_) => Ok(bytes),
            Err(error) => Err(error).context("airport config validation failed"),
        },
        Err(error) => Err(error),
    };

    state
        .cache
        .resolve(
            "airport",
            "yaml",
            &state.resolver_config.airport_url,
            upstream,
        )
        .await
}

/// Extracts the names of all airport nodes from the config, for ACL4SSR group matching.
///
/// Example: ["Airport Hysteria2", "HongKong V2Ray", "Local Override"]
async fn extract_airport_node_names(config: &[u8]) -> Result<Box<[String]>> {
    if config.len() < 100 {
        bail!("airport config response too short");
    }

    let names = run_yq("eval", config, "[.proxies[].name]", &["-"]).await?;
    let names: Box<[String]> =
        serde_json::from_slice(&names).context("airport config has invalid proxy names")?;
    if names.is_empty() {
        bail!("airport config does not contain any proxies");
    }
    Ok(names)
}

async fn merge_generated_sections(airport: &[u8], sections: &[u8]) -> Result<Vec<u8>> {
    let mut documents = Vec::with_capacity(airport.len() + sections.len() + 6);
    documents.extend_from_slice(airport);
    documents.extend_from_slice(b"\n---\n");
    documents.extend_from_slice(sections);

    // Keep the airport document as the base. A same-name local VPS replaces the airport node.
    let filter = r#"
select(documentIndex == 0) as $base |
select(documentIndex == 1) as $sections |
$base |
($sections.proxies // []) as $new |
.proxies = ((.proxies // []) | map(select([.name] - ($new | map(.name)) | length > 0)) + $new) |
.["proxy-groups"] = $sections.["proxy-groups"] |
.rules = $sections.rules
"#;
    run_yq("eval-all", &documents, filter, &["-"]).await
}

async fn fetch_url(client: &Client, target: &str) -> Result<Vec<u8>> {
    let response = client
        .get(target)
        .header(reqwest::header::USER_AGENT, "Clash/Meta")
        .send()
        .await
        .context("airport config request failed")?;

    let status = response.status();
    if !status.is_success() {
        bail!("airport config returned http error {status}");
    }

    response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .context("failed to read airport config response body")
}

async fn merge_with_origin(generated: &[u8], origin_path: &str) -> Result<Vec<u8>> {
    // Full config keeps the local shell and replaces only resolver-owned dynamic sections.
    let filter = r#"select(fileIndex == 0) as $origin | select(fileIndex == 1) as $gen | $origin | .proxies = $gen.proxies | .["proxy-groups"] = $gen.["proxy-groups"] | .rules = $gen.rules"#;
    run_yq("eval-all", generated, filter, &[origin_path, "-"]).await
}

#[cfg(test)]
mod tests {
    use serde_json::Value;

    use super::{extract_airport_node_names, merge_generated_sections};

    const AIRPORT: &[u8] = br#"
mixed-port: 7890
proxies:
  - name: Airport Hysteria2
    type: hysteria2
    server: airport.example.com
    password: opaque-secret
    obfs: salamander
  - name: Local Override
    type: ss
    server: old.example.com
    port: 443
proxy-groups:
  - name: Airport Group
    type: select
    proxies: [Airport Hysteria2]
rules:
  - MATCH,DIRECT
"#;

    const SECTIONS: &[u8] = br#"
{
  "proxies": [
    {
      "name": "Local Override",
      "type": "vless",
      "server": "192.0.2.10",
      "port": 8443,
      "future-protocol-field": {"preserved": true}
    }
  ],
  "proxy-groups": [
    {
      "name": "Generated Group",
      "type": "select",
      "proxies": ["Local Override", "Airport Hysteria2"]
    }
  ],
  "rules": ["IP-CIDR,192.0.2.10/32,DIRECT,no-resolve", "MATCH,Generated Group"]
}
"#;

    #[tokio::test]
    async fn generated_sections_replace_only_resolver_owned_sections() {
        let names = extract_airport_node_names(AIRPORT).await.unwrap();
        assert_eq!(names.as_ref(), ["Airport Hysteria2", "Local Override"]);

        let merged = merge_generated_sections(AIRPORT, SECTIONS).await.unwrap();
        let merged: Value = serde_json::from_slice(&merged).unwrap();

        assert_eq!(merged["mixed-port"], 7890);
        assert_eq!(merged["proxies"].as_array().unwrap().len(), 2);
        assert_eq!(merged["proxies"][0]["password"], "opaque-secret");
        assert_eq!(merged["proxies"][0]["obfs"], "salamander");
        assert_eq!(
            merged["proxies"][1]["future-protocol-field"]["preserved"],
            true
        );
        assert_eq!(merged["proxy-groups"][0]["name"], "Generated Group");
        assert_eq!(merged["rules"][1], "MATCH,Generated Group");
    }
}
