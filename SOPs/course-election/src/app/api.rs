//! Local HTTP over a Unix socket. Access control is the socket file's permissions.
use super::{
    protocol::{Command, Response},
    runtime::Runtime,
};
use axum::{Json, Router, extract::State, http::StatusCode, routing::post};
use std::sync::Arc;

pub(crate) fn router(runtime: Arc<Runtime>) -> Router {
    Router::new()
        .route("/v1/command", post(command))
        .with_state(runtime)
}

async fn command(
    State(runtime): State<Arc<Runtime>>,
    Json(command): Json<Command>,
) -> Result<Json<Response>, (StatusCode, String)> {
    runtime
        .handle(command)
        .await
        .map(Json)
        .map_err(|e| (StatusCode::CONFLICT, format!("{e:#}")))
}
