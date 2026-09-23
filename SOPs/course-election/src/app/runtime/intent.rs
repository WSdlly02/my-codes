//! One intent = one task. It waits for its trigger, hands a submission to the Executor and
//! repeats until done. Cancellation and the deadline cut only the waits: a submission
//! already handed to the Executor always completes and its result is recorded.
use super::{
    Capacity, Context,
    executor::{Executor, Submitted},
};
use crate::app::{
    protocol::{Phase, Progress, Trigger},
    support::now_ms,
};
use std::{future::Future, time::Duration};
use tokio::{
    sync::watch,
    time::{Instant, sleep, sleep_until, timeout_at},
};
use tokio_util::sync::CancellationToken;

/// How early a timed fire warms the connection and makes sure the page is open.
const LEAD: Duration = Duration::from_secs(5);

pub(super) struct Intent {
    pub profile: String,
    pub lesson: String,
    pub trigger: Trigger,
    pub exec: Executor,
    pub context: watch::Receiver<Context>,
    pub capacity: watch::Receiver<Option<Capacity>>,
    /// Pause between retries of a watch whose page could not be prepared.
    pub poll: Duration,
    pub stop: CancellationToken,
    pub progress: watch::Sender<Progress>,
}

/// Why an intent stopped waiting.
type Stopped = Phase;

impl Intent {
    pub(super) async fn run(self) {
        let end = match self.trigger.clone() {
            Trigger::Fire {
                select,
                at_ms,
                attempts,
                interval_ms,
            } => {
                let at = at_ms.map(instant_at);
                let interval = Duration::from_millis(interval_ms);
                self.fire(select, at, attempts, interval).await
            }
            Trigger::Watch {
                timeout_ms,
                dry_run,
            } => {
                let deadline =
                    (timeout_ms != 0).then(|| Instant::now() + Duration::from_millis(timeout_ms));
                self.watch(deadline, dry_run).await
            }
        };
        let phase = end.unwrap_or_else(|stopped| stopped);
        self.progress.send_modify(|p| p.phase = phase);
    }

    /// Waits for `fut`, unless cancelled, past the deadline, or the context is no longer ours.
    async fn wait<F: Future>(
        &self,
        deadline: Option<Instant>,
        fut: F,
    ) -> Result<F::Output, Stopped> {
        let bounded = async {
            match deadline {
                Some(deadline) => timeout_at(deadline, fut).await.map_err(|_| Phase::Expired),
                None => Ok(fut.await),
            }
        };
        let mut context = self.context.clone();
        let replaced = context.wait_for(|c| c.profile.as_deref() != Some(&self.profile));
        tokio::select! {
            () = self.stop.cancelled() => Err(Phase::Cancelled),
            _ = replaced => {
                self.note("上下文已切换".into());
                Err(Phase::Cancelled)
            }
            result = bounded => result,
        }
    }

    fn note(&self, message: String) {
        tracing::info!(lesson = %self.lesson, "{message}");
        self.progress.send_modify(|p| p.last_result = message);
    }

    /// Hands one submission to the Executor. `Some` is final; `None` means retry.
    async fn submit(&self, select: bool) -> Option<Phase> {
        self.progress.send_modify(|p| {
            p.phase = Phase::Submitting;
            p.attempts += 1;
        });
        let submitted = self.exec.submit(&self.profile, &self.lesson, select).await;
        self.progress.send_modify(|p| p.phase = Phase::Waiting);
        match submitted {
            Submitted::Succeeded(message) => {
                self.note(message);
                Some(Phase::Succeeded)
            }
            Submitted::Unknown(message) => {
                self.note(message);
                Some(Phase::Unknown)
            }
            Submitted::Rejected(message) | Submitted::NotSent(message) => {
                self.note(message);
                None
            }
        }
    }

    async fn fire(
        &self,
        select: bool,
        at: Option<Instant>,
        attempts: u32,
        interval: Duration,
    ) -> Result<Phase, Stopped> {
        if let Some(at) = at {
            self.wait(None, sleep_until(at.checked_sub(LEAD).unwrap_or(at)))
                .await?;
            if let Err(e) = self.exec.prepare(&self.profile, true).await {
                self.note(format!("预热失败，到点时重试：{e:#}"));
            }
            self.wait(None, sleep_until(at)).await?;
        }
        for attempt in 1..=attempts {
            if let Some(end) = self.submit(select).await {
                return Ok(end);
            }
            if attempt < attempts {
                // Reopen an invalidated page now rather than in the next attempt.
                let _ = self.exec.prepare(&self.profile, false).await;
                self.wait(None, sleep(interval)).await?;
            }
        }
        Ok(Phase::Failed)
    }

    async fn watch(&self, deadline: Option<Instant>, dry_run: bool) -> Result<Phase, Stopped> {
        let mut capacity = self.capacity.clone();
        capacity.borrow_and_update(); // only reads made from now on count
        loop {
            // Keep the page open while waiting, so a vacancy costs exactly one POST.
            if let Err(e) = self.exec.prepare(&self.profile, false).await {
                self.note(format!("准备选课页面失败：{e:#}"));
                self.wait(deadline, sleep(self.poll)).await?;
                continue;
            }
            if !self
                .wait(
                    deadline,
                    vacancy(&mut capacity, &self.profile, &self.lesson),
                )
                .await?
            {
                return Ok(Phase::Failed); // the capacity poller stopped
            }
            if dry_run {
                self.note("有空位（dry-run，未提交）".into());
                continue;
            }
            if let Some(end) = self.submit(true).await {
                return Ok(end);
            }
        }
    }
}

/// Resolves on the next fresh read showing a vacancy; false if reads have stopped.
async fn vacancy(
    capacity: &mut watch::Receiver<Option<Capacity>>,
    profile: &str,
    lesson: &str,
) -> bool {
    while capacity.changed().await.is_ok() {
        if capacity
            .borrow_and_update()
            .as_ref()
            .is_some_and(|c| c.profile == profile && c.vacant(lesson))
        {
            return true;
        }
    }
    false
}

fn instant_at(at_ms: i64) -> Instant {
    Instant::now() + Duration::from_millis(at_ms.saturating_sub(now_ms()).max(0) as u64)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app::runtime::executor::{Request, fake};
    use tokio::sync::mpsc;

    struct Rig {
        requests: mpsc::Receiver<Request>,
        context: watch::Sender<Context>,
        stop: CancellationToken,
        progress: watch::Receiver<Progress>,
        capacity: watch::Sender<Option<Capacity>>,
        task: tokio::task::JoinHandle<()>,
        started: Instant,
    }

    fn start(trigger: Trigger) -> Rig {
        let (exec, requests) = fake();
        let (capacity, capacity_rx) = watch::channel(Some(read(true)));
        let (progress_tx, progress) = watch::channel(Progress::default());
        let (context, context_rx) = watch::channel(Context {
            profile: Some("1".into()),
            reader: None,
        });
        let stop = CancellationToken::new();
        let intent = Intent {
            profile: "1".into(),
            lesson: "7".into(),
            trigger,
            exec,
            context: context_rx,
            capacity: capacity_rx,
            poll: Duration::from_secs(1),
            stop: stop.clone(),
            progress: progress_tx,
        };
        Rig {
            requests,
            context,
            stop,
            progress,
            capacity,
            task: tokio::spawn(intent.run()),
            started: Instant::now(),
        }
    }

    fn fire(at_in: Option<Duration>, attempts: u32) -> Trigger {
        Trigger::Fire {
            select: true,
            at_ms: at_in.map(|d| now_ms() + d.as_millis() as i64),
            attempts,
            interval_ms: 300,
        }
    }

    fn read(vacant: bool) -> Capacity {
        let count = crate::model::LessonCount {
            selected: if vacant { 59 } else { 60 },
            limit: 60,
            ..Default::default()
        };
        Capacity {
            profile: "1".into(),
            at_ms: 0,
            result: Ok([("7".to_string(), count)].into()),
        }
    }

    impl Rig {
        /// Answers the next request, which must be a Prepare; returns its `prewarm`.
        async fn prepared(&mut self) -> bool {
            match self.requests.recv().await {
                Some(Request::Prepare { prewarm, reply, .. }) => {
                    let _ = reply.send(Ok(()));
                    prewarm
                }
                _ => panic!("expected Prepare"),
            }
        }

        async fn submitted(&mut self) -> tokio::sync::oneshot::Sender<Submitted> {
            match self.requests.recv().await {
                Some(Request::Submit { reply, .. }) => reply,
                _ => panic!("expected Submit"),
            }
        }

        async fn end(self) -> Progress {
            self.task.await.unwrap();
            self.progress.borrow().clone()
        }
    }

    #[tokio::test(start_paused = true)]
    async fn timed_fire_prewarms_early_and_submits_on_time() {
        let mut rig = start(fire(Some(Duration::from_secs(60)), 1));
        assert!(
            rig.prepared().await,
            "the early prepare warms the connection"
        );
        let early = rig.started.elapsed();
        assert!(early <= Duration::from_secs(55) && early > Duration::from_millis(54_900));
        let reply = rig.submitted().await;
        let late = rig.started.elapsed();
        assert!(late <= Duration::from_secs(60) && late > Duration::from_millis(59_900));
        reply.send(Submitted::Succeeded("选课成功".into())).ok();
        assert_eq!(rig.end().await.phase, Phase::Succeeded);
    }

    #[tokio::test(start_paused = true)]
    async fn cancelling_before_hand_off_sends_nothing() {
        let mut rig = start(fire(Some(Duration::from_secs(60)), 1));
        rig.stop.cancel();
        assert!(rig.requests.recv().await.is_none());
        assert_eq!(rig.end().await.phase, Phase::Cancelled);
    }

    #[tokio::test(start_paused = true)]
    async fn cancelling_after_hand_off_keeps_the_result() {
        let mut rig = start(fire(None, 3));
        let reply = rig.submitted().await;
        rig.stop.cancel();
        reply.send(Submitted::Rejected("名额已满".into())).ok();
        // The rejected attempt re-prepares, then the cancelled interval wait ends it.
        assert!(!rig.prepared().await);
        let end = rig.end().await;
        assert_eq!((end.phase, end.attempts), (Phase::Cancelled, 1));
    }

    #[tokio::test(start_paused = true)]
    async fn rejections_retry_until_attempts_run_out() {
        let mut rig = start(fire(None, 3));
        for attempt in 1..=3 {
            rig.submitted()
                .await
                .send(Submitted::Rejected("未开放".into()))
                .ok();
            if attempt < 3 {
                rig.prepared().await;
            }
        }
        let end = rig.end().await;
        assert_eq!((end.phase, end.attempts), (Phase::Failed, 3));
    }

    #[tokio::test(start_paused = true)]
    async fn unknown_is_never_retried() {
        let mut rig = start(fire(None, 3));
        rig.submitted()
            .await
            .send(Submitted::Unknown("timeout".into()))
            .ok();
        assert!(rig.requests.recv().await.is_none());
        assert_eq!(rig.end().await.phase, Phase::Unknown);
    }

    #[tokio::test(start_paused = true)]
    async fn watch_submits_only_on_a_fresh_vacancy() {
        // The initial value shows a vacancy, but it predates the watch.
        let mut rig = start(Trigger::Watch {
            timeout_ms: 0,
            dry_run: false,
        });
        rig.prepared().await;
        rig.capacity.send_replace(Some(read(false)));
        tokio::task::yield_now().await;
        assert!(
            rig.requests.try_recv().is_err(),
            "a full read never submits"
        );
        rig.capacity.send_replace(Some(read(true)));
        rig.submitted()
            .await
            .send(Submitted::Succeeded("选课成功".into()))
            .ok();
        assert_eq!(rig.end().await.phase, Phase::Succeeded);
    }

    #[tokio::test(start_paused = true)]
    async fn replacing_the_context_ends_a_waiting_intent() {
        let mut rig = start(Trigger::Watch {
            timeout_ms: 0,
            dry_run: false,
        });
        rig.prepared().await;
        rig.context.send_replace(Context {
            profile: Some("2".into()),
            reader: None,
        });
        let end = rig.end().await;
        assert_eq!(
            (end.phase, end.last_result.as_str()),
            (Phase::Cancelled, "上下文已切换")
        );
    }

    #[tokio::test(start_paused = true)]
    async fn watch_expires_at_its_deadline() {
        let mut rig = start(Trigger::Watch {
            timeout_ms: 10_000,
            dry_run: false,
        });
        rig.prepared().await;
        let started = rig.started;
        let end = rig.end().await;
        assert_eq!(end.phase, Phase::Expired);
        assert_eq!(started.elapsed(), Duration::from_secs(10));
    }
}
