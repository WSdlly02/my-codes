use std::{net::SocketAddr, sync::Arc, time::Duration};

use anyhow::{Context, Result, bail};
use axum::{
    Router,
    extract::State,
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    routing::get,
};
use mihomo_updater::{
    models::{RealityOpts, RenderedProxy, ResolverConfig, VpsConfig},
    yq::run_yq,
};
use reqwest::Client;
use serde_json::to_string;
use tracing::{error, info, warn};
use url::Url;

#[derive(Clone)]
struct AppState {
    client: Client,
    config: Arc<ResolverConfig>,
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
            kind: &proxy.kind,
            uuid: &proxy.uuid,
            server: &proxy.ip,
            port: proxy.port,
            flow: &proxy.flow,
            udp: proxy.udp,
            tls: proxy.tls,
            servername: &proxy.servername,
            client_fingerprint: &proxy.client_fingerprint,
            reality_opts: RealityOpts {
                public_key: &proxy.public_key,
                short_id: &proxy.short_id,
            },
        })
        .collect();

    to_string(&rendered).context("failed to serialize custom proxies")
}
