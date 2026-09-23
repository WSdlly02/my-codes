use crate::model::{ChannelEntry, SelectedSnapshot};
use serde::{Deserialize, Serialize};

pub(crate) const RUNTIME_DIR: &str = "cache/runtime";
pub(crate) const SOCKET_FILE: &str = "cache/runtime/daemon.sock";

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Spec {
    pub lesson: String,
    #[serde(flatten)]
    pub trigger: Trigger,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum Trigger {
    /// Submit at `at_ms` (or right away) regardless of capacity. `select: false` drops.
    Fire {
        select: bool,
        at_ms: Option<i64>,
        attempts: u32,
        interval_ms: u64,
    },
    /// Select whenever a capacity read shows a vacancy. `timeout_ms` zero = no deadline.
    Watch { timeout_ms: u64, dry_run: bool },
}

#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Phase {
    #[default]
    Waiting,
    /// Handed to the Executor: no longer cancellable.
    Submitting,
    Succeeded,
    Failed,
    Cancelled,
    Expired,
    /// A POST may have been executed; never retried.
    Unknown,
}

impl Phase {
    pub fn ended(self) -> bool {
        !matches!(self, Phase::Waiting | Phase::Submitting)
    }
}

impl std::fmt::Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Phase::Waiting => "等待中",
            Phase::Submitting => "提交中",
            Phase::Succeeded => "成功",
            Phase::Failed => "失败",
            Phase::Cancelled => "已取消",
            Phase::Expired => "已过期",
            Phase::Unknown => "结果未知",
        })
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub(crate) struct Progress {
    pub phase: Phase,
    pub attempts: u32,
    pub last_result: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct IntentView {
    pub id: u64,
    pub profile: String,
    pub spec: Spec,
    pub progress: Progress,
}

// Never derive Debug: Login contains a secret.
#[derive(Serialize, Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub(crate) enum Command {
    Status,
    Jobs,
    Job {
        id: u64,
    },
    /// Blocks until the intent ends.
    Wait {
        id: u64,
    },
    Logs,
    /// With `wait`, blocks until the new intent ends.
    Add {
        spec: Spec,
        wait: bool,
    },
    /// Stops the intent's waiting; a submission already handed off still completes.
    Cancel {
        id: u64,
    },
    Maintenance(Maintenance),
}

/// Operations executed by the Executor, in order with submissions. Intents whose profile
/// is no longer the current one end by themselves.
#[derive(Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub(crate) enum Maintenance {
    Login {
        username: String,
        password: String,
    },
    Logout,
    /// Fetches the list of election rounds (profiles) into the cache.
    SyncProfiles,
    /// Opens the election page of this profile, obtaining its token.
    UseProfile {
        id: String,
    },
    /// Reopens the page and caches the current profile's courses and capacity.
    SyncCourses,
    /// Caches the current profile's selected courses.
    SyncSelected,
    ExportSchedule {
        semester: String,
    },
}

impl std::fmt::Display for Maintenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Maintenance::Login { username, .. } => write!(f, "登录 {username}"),
            Maintenance::Logout => f.write_str("退出登录"),
            Maintenance::SyncProfiles => f.write_str("同步轮次列表"),
            Maintenance::UseProfile { id } => write!(f, "进入 profile {id}"),
            Maintenance::SyncCourses => f.write_str("同步课程目录"),
            Maintenance::SyncSelected => f.write_str("同步已选课程"),
            Maintenance::ExportSchedule { semester } => write!(f, "导出学期 {semester} 的课表"),
        }
    }
}

impl From<Maintenance> for Command {
    fn from(maintenance: Maintenance) -> Self {
        Command::Maintenance(maintenance)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Status {
    pub profile: Option<String>,
    pub context_ready: bool,
    pub stopping: bool,
    pub poll_ms: u64,
    pub counts_at_ms: Option<i64>,
    pub read_error: Option<String>,
    pub intents: usize,
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
    Jobs(Vec<IntentView>),
    Job(IntentView),
    LoggedIn,
    LoggedOut,
    Profiles(Vec<ChannelEntry>),
    ProfileInUse {
        profile: String,
    },
    CoursesSynced {
        courses: usize,
        counts: Option<usize>,
        counts_from_cache: bool,
    },
    Selected(SelectedSnapshot),
    Schedule {
        html: String,
        semester: String,
    },
    Logs {
        events: Vec<LogEvent>,
        latest: u64,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_format() {
        let json =
            serde_json::to_value(Command::from(Maintenance::UseProfile { id: "3112".into() }))
                .unwrap();
        assert_eq!(
            json,
            serde_json::json!({"command": "maintenance", "op": "use_profile", "id": "3112"})
        );
        let spec = Spec {
            lesson: "1".into(),
            trigger: Trigger::Watch {
                timeout_ms: 0,
                dry_run: false,
            },
        };
        assert_eq!(
            serde_json::to_value(&spec).unwrap(),
            serde_json::json!({"lesson": "1", "kind": "watch", "timeout_ms": 0, "dry_run": false})
        );
    }
}
