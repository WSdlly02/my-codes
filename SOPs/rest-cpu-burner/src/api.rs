use axum::{
    Json, Router,
    extract::{Query, State},
    http::StatusCode,
    routing::{get, post},
};
use serde::{Deserialize, Serialize};
use std::sync::atomic::Ordering;

use crate::{control::bp_to_percent, state::AppState};

#[derive(Deserialize)]
struct SetLoadRequest {
    /// 希望系统最终达到的总 CPU 使用率（0-100）
    percent: u8,
}

#[derive(Deserialize)]
struct AuthQuery {
    access_token: Option<String>,
}

#[derive(Serialize)]
struct StatusResponse {
    /// 用户设定的目标系统使用率
    target_percent: u8,
    /// 当前实测系统总 CPU 使用率
    system_usage_percent: f64,
    /// 平滑后的系统 CPU 使用率
    smoothed_usage_percent: f64,
    /// 扣除本进程估算消耗后的外部 CPU 使用率
    external_usage_percent: f64,
    /// 本进程实际正在消耗的占用率
    process_burn_percent: f64,
    cpu_count: usize,
    message: String,
}

pub fn router(state: AppState) -> Router {
    Router::new()
        .route("/status", get(get_status))
        .route("/load", post(set_load))
        .with_state(state)
}

fn check_auth(query: &AuthQuery, state: &AppState) -> bool {
    query
        .access_token
        .as_deref()
        .map(|token| token == state.access_token.as_str())
        .unwrap_or(false)
}

fn status_response(state: &AppState, message: String) -> StatusResponse {
    StatusResponse {
        target_percent: state.target.load(Ordering::Relaxed),
        system_usage_percent: bp_to_percent(state.system_usage.load(Ordering::Relaxed)),
        smoothed_usage_percent: bp_to_percent(state.smoothed_usage.load(Ordering::Relaxed)),
        external_usage_percent: bp_to_percent(state.external_usage.load(Ordering::Relaxed)),
        process_burn_percent: bp_to_percent(state.current_burn.load(Ordering::Relaxed)),
        cpu_count: num_cpus::get(),
        message,
    }
}

async fn get_status(
    State(state): State<AppState>,
    Query(query): Query<AuthQuery>,
) -> Result<Json<StatusResponse>, StatusCode> {
    if !check_auth(&query, &state) {
        return Err(StatusCode::UNAUTHORIZED);
    }

    Ok(Json(status_response(&state, "ok".into())))
}

async fn set_load(
    State(state): State<AppState>,
    Query(query): Query<AuthQuery>,
    Json(body): Json<SetLoadRequest>,
) -> Result<Json<StatusResponse>, (StatusCode, String)> {
    if !check_auth(&query, &state) {
        return Err((StatusCode::UNAUTHORIZED, "invalid access_token".into()));
    }
    if body.percent > 100 {
        return Err((StatusCode::BAD_REQUEST, "percent must be 0-100".into()));
    }

    state.target.store(body.percent, Ordering::Relaxed);
    println!(
        "[INFO] target={}%, current burn={:.2}%",
        body.percent,
        bp_to_percent(state.current_burn.load(Ordering::Relaxed))
    );

    Ok(Json(status_response(
        &state,
        format!("target={}%", body.percent),
    )))
}
