//! Local HTTP over a Unix socket. Access control is the socket file's permissions.
use super::{protocol::Command, runtime::Envelope};
use axum::{Json, Router, extract::State, http::StatusCode, routing::post};
use tokio::sync::{mpsc, oneshot};

pub(crate) fn router(sender: mpsc::Sender<Envelope>) -> Router {
    Router::new()
        .route("/v1/command", post(command))
        .with_state(sender)
}

async fn command(
    State(sender): State<mpsc::Sender<Envelope>>,
    Json(command): Json<Command>,
) -> Result<Json<super::protocol::Response>, (StatusCode, String)> {
    let unavailable = |message: &str| (StatusCode::SERVICE_UNAVAILABLE, message.to_owned());
    let (reply, response) = oneshot::channel();
    sender
        .send(Envelope { command, reply })
        .await
        .map_err(|_| unavailable("daemon 已停止"))?;
    response
        .await
        .map_err(|_| unavailable("响应中断；请查询状态，不要重复提交"))?
        .map(Json)
        .map_err(|error| (StatusCode::CONFLICT, error))
}
