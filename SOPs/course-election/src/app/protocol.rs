use super::intent::Intent;
use crate::model::{ChannelEntry, SelectedSnapshot};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub(crate) const RUNTIME_DIR: &str = "cache/runtime";
pub(crate) const SOCKET_FILE: &str = "cache/runtime/daemon.sock";

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Mode {
    Watch,
    Fire,
    Drop,
    Arm,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Spec {
    pub lesson: String,
    pub mode: Mode,
    pub interval_ms: u64,
    /// zero = unlimited; includes failed preparation attempts for fire/drop/arm.
    pub attempts: u64,
    /// zero = no deadline.
    pub timeout_ms: u64,
    pub at_ms: Option<i64>,
    pub dry_run: bool,
}

// Never derive Debug: Login contains a secret.
#[derive(Serialize, Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub(crate) enum Command {
    Status,
    Jobs,
    Job { id: u64 },
    Logs,
    Add { spec: Spec },
    Pause { id: u64 },
    Resume { id: u64 },
    Cancel { id: u64 },
    Maintenance(Maintenance),
}

/// Operations that need the Executor's Session. Accepted one at a time; while one is
/// pending or running, no submission is granted.
#[derive(Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub(crate) enum Maintenance {
    Reconcile { id: u64 },
    Profile { id: String, force: bool },
    Login { username: String, password: String },
    Logout { force: bool },
    Refresh,
    Selected,
    Channels,
    Export { semester: String },
    Prepare,
    ClearCache,
}

impl From<Maintenance> for Command {
    fn from(maintenance: Maintenance) -> Self {
        Command::Maintenance(maintenance)
    }
}

pub(crate) type Reply = Result<Response, String>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Status {
    pub profile: Option<String>,
    pub context_ready: bool,
    pub maintenance: bool,
    pub stopping: bool,
    pub inflight: Option<u64>,
    pub counts_at_ms: Option<i64>,
    pub read_error: Option<String>,
    pub jobs: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct LogEvent {
    pub sequence: u64,
    pub at_ms: i64,
    pub message: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "data", rename_all = "snake_case")]
pub(crate) enum Response {
    Status(Status),
    Jobs(Vec<Intent>),
    Job(Intent),
    Profile {
        profile: String,
    },
    LoggedIn,
    LoggedOut,
    Refreshed {
        mapping: usize,
        counts: Option<usize>,
        counts_from_cache: bool,
    },
    Selected(SelectedSnapshot),
    Reconciled {
        id: u64,
        selected: HashMap<String, bool>,
    },
    Channels(Vec<ChannelEntry>),
    Exported {
        html: String,
        semester: String,
    },
    Prepared,
    Cleared,
    Logs {
        events: Vec<LogEvent>,
        latest: u64,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maintenance_nests_inside_command() {
        let json = serde_json::to_value(Command::from(Maintenance::Profile {
            id: "3112".into(),
            force: false,
        }))
        .unwrap();
        assert_eq!(
            json,
            serde_json::json!({"command": "maintenance", "op": "profile", "id": "3112", "force": false})
        );
        let back: Command = serde_json::from_value(json).unwrap();
        assert!(matches!(
            back,
            Command::Maintenance(Maintenance::Profile { .. })
        ));
    }
}
