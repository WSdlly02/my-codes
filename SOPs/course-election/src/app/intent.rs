use super::{
    parser,
    protocol::{Mode, Spec},
};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

pub(crate) fn now_ms() -> i64 {
    chrono::Utc::now().timestamp_millis()
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Phase {
    Waiting,
    Preparing,
    InFlight,
    Paused,
    Cancelled,
    Succeeded,
    Failed,
    Expired,
    Unknown,
}

impl Phase {
    pub fn active(self) -> bool {
        matches!(
            self,
            Phase::Waiting | Phase::Preparing | Phase::InFlight | Phase::Paused
        )
    }
}

/// What the Executor did with one dispatch of an intent.
pub(crate) enum Outcome {
    /// Nothing was sent; the error happened while preparing the page.
    BeforeError(String),
    /// Prepare-only dispatch finished.
    Prepared,
    /// The Manager refused the grant; nothing was sent.
    Skipped,
    /// The server gave a recognizable answer.
    Response(String),
    /// A POST may have been executed; never retried automatically.
    Unknown(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Intent {
    pub id: u64,
    pub profile: String,
    pub spec: Spec,
    pub phase: Phase,
    pub attempts: u64,
    pub next_ms: i64,
    pub deadline_ms: Option<i64>,
    pub last_result: String,
    /// Arm only: the early context preparation has been dispatched.
    #[serde(skip)]
    pub arm_prepared: bool,
    /// Pause/cancel requested while InFlight; applied when the POST returns.
    #[serde(skip)]
    pub stop_after_flight: Option<Phase>,
}

impl Intent {
    pub fn new(id: u64, profile: String, spec: Spec, now: i64) -> Result<Self> {
        ensure!(
            spec.mode != Mode::Watch || spec.interval_ms >= 1000,
            "watch 间隔至少 1s"
        );
        ensure!(
            spec.mode != Mode::Arm || spec.at_ms.is_some_and(|at| at > now),
            "arm 需要未来的 --at"
        );
        ensure!(
            spec.mode == Mode::Arm || spec.at_ms.is_none(),
            "只有 arm 接受 --at"
        );
        ensure!(
            !spec.dry_run || spec.mode == Mode::Watch,
            "只有 watch 接受 --dry-run"
        );
        Ok(Self {
            id,
            profile,
            next_ms: spec.at_ms.unwrap_or(now),
            deadline_ms: (spec.timeout_ms != 0).then(|| now + spec.timeout_ms as i64),
            spec,
            phase: Phase::Waiting,
            attempts: 0,
            last_result: String::new(),
            arm_prepared: false,
            stop_after_flight: None,
        })
    }

    pub fn due(&self, now: i64) -> bool {
        self.phase == Phase::Waiting && now >= self.next_ms
    }

    pub fn expired(&self, now: i64) -> bool {
        self.deadline_ms.is_some_and(|end| now >= end)
    }

    fn exhausted(&self) -> bool {
        self.spec.attempts > 0 && self.attempts >= self.spec.attempts
    }

    /// `target` is Paused or Cancelled. An InFlight POST is never interrupted.
    pub fn stop(&mut self, target: Phase) {
        if self.phase == Phase::InFlight {
            self.stop_after_flight = Some(target);
        } else {
            self.phase = target;
        }
    }

    pub fn resume(&mut self, now: i64) -> Result<()> {
        ensure!(self.phase == Phase::Paused, "只有 paused 意图可以恢复");
        ensure!(!self.expired(now), "意图已过截止时间；请新建");
        self.phase = Phase::Waiting;
        self.next_ms = self.spec.at_ms.unwrap_or(now).max(now);
        self.arm_prepared = false;
        Ok(())
    }

    /// The cancellation linearization point: after this, exactly one POST is sent.
    pub fn grant(&mut self, now: i64) -> bool {
        if self.phase != Phase::Preparing || self.expired(now) {
            return false;
        }
        self.phase = Phase::InFlight;
        self.attempts += 1;
        true
    }

    pub fn settle(&mut self, outcome: Outcome, prepare_only: bool, now: i64) {
        let retry_at = now + self.spec.interval_ms as i64;
        match outcome {
            Outcome::Response(body) => {
                self.last_result = parser::summarize_selection_response(&body);
                let stop = self.stop_after_flight.take();
                self.phase = if parser::selection_succeeded(&body) {
                    Phase::Succeeded
                } else if let Some(stop) = stop {
                    stop
                } else if self.exhausted() {
                    Phase::Failed
                } else {
                    self.next_ms = retry_at;
                    Phase::Waiting
                };
            }
            Outcome::Unknown(error) => {
                self.last_result = error;
                self.stop_after_flight = None;
                self.phase = Phase::Unknown;
            }
            // A stop or expiry during preparation already moved the phase; keep it.
            _ if self.phase != Phase::Preparing => {}
            Outcome::BeforeError(error) => {
                self.last_result = error;
                if !prepare_only {
                    self.attempts += 1;
                }
                // An arm's early preparation must not move its scheduled time.
                if !prepare_only || self.spec.mode == Mode::Watch {
                    self.next_ms = retry_at;
                }
                self.phase = if self.exhausted() {
                    Phase::Failed
                } else {
                    Phase::Waiting
                };
            }
            Outcome::Prepared => self.phase = Phase::Waiting,
            Outcome::Skipped => {
                self.next_ms = retry_at;
                self.phase = Phase::Waiting;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn job(mode: Mode, attempts: u64) -> Intent {
        let spec = Spec {
            lesson: "2".into(),
            mode,
            interval_ms: 1000,
            attempts,
            timeout_ms: 0,
            at_ms: None,
            dry_run: false,
        };
        Intent::new(1, "1".into(), spec, 0).unwrap()
    }

    #[test]
    fn cancellation_before_grant_blocks_submission() {
        let mut j = job(Mode::Watch, 0);
        j.phase = Phase::Preparing;
        j.stop(Phase::Cancelled);
        assert!(!j.grant(0));
        assert_eq!(j.attempts, 0);
        j.settle(Outcome::Skipped, false, 0);
        assert_eq!(j.phase, Phase::Cancelled);
    }

    #[test]
    fn cancellation_after_grant_applies_when_post_returns() {
        let mut j = job(Mode::Watch, 0);
        j.phase = Phase::Preparing;
        assert!(j.grant(0));
        j.stop(Phase::Cancelled);
        assert_eq!(j.phase, Phase::InFlight);
        j.settle(Outcome::Response("选课失败:名额已满".into()), false, 0);
        assert_eq!(j.phase, Phase::Cancelled);
    }

    #[test]
    fn success_wins_over_a_pending_stop() {
        let mut j = job(Mode::Fire, 1);
        j.phase = Phase::Preparing;
        assert!(j.grant(0));
        j.stop(Phase::Paused);
        j.settle(Outcome::Response("选课成功".into()), false, 0);
        assert_eq!(j.phase, Phase::Succeeded);
    }

    #[test]
    fn unknown_is_never_retried() {
        let mut j = job(Mode::Fire, 0);
        j.phase = Phase::Preparing;
        assert!(j.grant(0));
        j.settle(Outcome::Unknown("timeout".into()), false, 0);
        assert_eq!(j.phase, Phase::Unknown);
        assert!(!j.due(i64::MAX));
    }

    #[test]
    fn preparation_failures_count_towards_attempts() {
        let mut j = job(Mode::Fire, 2);
        for _ in 0..2 {
            j.phase = Phase::Preparing;
            j.settle(Outcome::BeforeError("net".into()), false, 0);
        }
        assert_eq!(j.phase, Phase::Failed);
    }

    #[test]
    fn arm_preparation_failure_keeps_scheduled_time() {
        let at = 60_000;
        let mut j = job(Mode::Fire, 2);
        j.spec.mode = Mode::Arm;
        j.next_ms = at;
        j.phase = Phase::Preparing;
        j.settle(Outcome::BeforeError("net".into()), true, 0);
        assert_eq!((j.phase, j.next_ms, j.attempts), (Phase::Waiting, at, 0));
    }

    #[test]
    fn resuming_arm_does_not_move_scheduled_time_earlier() {
        let mut j = job(Mode::Fire, 2);
        let at = 60_000;
        j.spec.mode = Mode::Arm;
        j.spec.at_ms = Some(at);
        j.next_ms = at;
        j.arm_prepared = true;
        j.stop(Phase::Paused);
        j.resume(0).unwrap();
        assert_eq!(j.next_ms, at);
        assert!(!j.due(0) && !j.arm_prepared);
    }
}
