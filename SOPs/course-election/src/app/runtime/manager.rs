//! Owns intent state, scheduling and submission grants; performs no network I/O.
//!
//! Read-side invariant: the Executor only changes the page context (reload, login, profile
//! switch) after the Manager has drained the capacity read. So every read result that
//! arrives while a reader is set belongs to the current context.
use super::{Context, Counts, Envelope, Event, Work};
use crate::app::{
    cache,
    http::{self, CountReader},
    intent::{Intent, Phase, now_ms},
    parser,
    protocol::{Command, LogEvent, Maintenance, Mode, Reply, Response, Spec, Status},
};
use anyhow::{Context as _, Result, ensure};
use std::collections::{BTreeMap, VecDeque};
use tokio::sync::oneshot;
use tokio_util::sync::CancellationToken;

/// How early an arm prepares its page and connection before `--at`.
const ARM_LEAD_MS: i64 = 5000;
const LOG_CAPACITY: usize = 512;

enum Busy {
    /// Intent id being prepared or submitted.
    Job(u64),
    Maintenance(oneshot::Sender<Reply>),
}

pub(super) struct Manager {
    jobs: BTreeMap<u64, Intent>,
    next_id: u64,
    profile: Option<String>,
    /// Present only when the Executor's page context is ready.
    reader: Option<CountReader>,
    counts: Option<(i64, Counts)>,
    read_error: Option<String>,
    reading: Option<CancellationToken>,
    next_read: i64,
    busy: Option<Busy>,
    pending: Option<(Maintenance, oneshot::Sender<Reply>)>,
    stopping: bool,
    /// Round-robin cursor so one always-due intent cannot starve the others.
    last_dispatched: u64,
    logs: VecDeque<LogEvent>,
    sequence: u64,
}

impl Manager {
    pub(super) fn new() -> Self {
        Self {
            jobs: BTreeMap::new(),
            next_id: 1,
            profile: None,
            reader: None,
            counts: None,
            read_error: None,
            reading: None,
            next_read: 0,
            busy: None,
            pending: None,
            stopping: false,
            last_dispatched: 0,
            logs: VecDeque::new(),
            sequence: 0,
        }
    }

    pub(super) fn stopping(&self) -> bool {
        self.stopping
    }

    pub(super) fn log(&mut self, message: impl ToString) {
        self.sequence += 1;
        let event = LogEvent {
            sequence: self.sequence,
            at_ms: now_ms(),
            message: message.to_string(),
        };
        tracing::info!(sequence = event.sequence, message = %event.message);
        if self.logs.len() == LOG_CAPACITY {
            self.logs.pop_front();
        }
        self.logs.push_back(event);
    }

    // ---- requests ----

    pub(super) fn dispatch(&mut self, Envelope { command, reply }: Envelope) {
        let result = match command {
            Command::Status => Ok(Response::Status(self.status())),
            Command::Jobs => Ok(Response::Jobs(self.jobs.values().cloned().collect())),
            Command::Job { id } => self.job(id).map(|job| Response::Job(job.clone())),
            Command::Logs => Ok(Response::Logs {
                events: self.logs.iter().cloned().collect(),
                latest: self.sequence,
            }),
            Command::Add { spec } => self.add(spec),
            Command::Pause { id } => self.stop(id, Phase::Paused),
            Command::Cancel { id } => self.stop(id, Phase::Cancelled),
            Command::Resume { id } => self.resume(id),
            Command::Maintenance(maintenance) => match self.admit(&maintenance) {
                Ok(()) => {
                    self.pending = Some((maintenance, reply));
                    return;
                }
                Err(error) => Err(error),
            },
        };
        let _ = reply.send(result.map_err(|e| format!("{e:#}")));
    }

    fn maintaining(&self) -> bool {
        self.pending.is_some() || matches!(self.busy, Some(Busy::Maintenance(_)))
    }

    fn status(&self) -> Status {
        Status {
            profile: self.profile.clone(),
            context_ready: self.reader.is_some(),
            maintenance: self.maintaining(),
            stopping: self.stopping,
            inflight: match self.busy {
                Some(Busy::Job(id)) => Some(id),
                _ => None,
            },
            counts_at_ms: self.counts.as_ref().map(|(at, _)| *at),
            read_error: self.read_error.clone(),
            jobs: self.jobs.len(),
        }
    }

    fn job(&self, id: u64) -> Result<&Intent> {
        self.jobs.get(&id).context("意图不存在")
    }

    fn ensure_accepting(&self) -> Result<()> {
        ensure!(
            !self.stopping && !self.maintaining(),
            "维护或关停中，请稍后再试"
        );
        Ok(())
    }

    fn add(&mut self, mut spec: Spec) -> Result<Response> {
        self.ensure_accepting()?;
        ensure!(self.reader.is_some(), "请先成功执行 profile <id>");
        let profile = self.profile.clone().context("未选择 profile")?;
        if !spec.lesson.bytes().all(|b| b.is_ascii_digit()) {
            spec.lesson = parser::resolve_lesson_id_by_name(
                &cache::load_mapping_cache(&profile)?,
                &spec.lesson,
            )?;
        }
        ensure!(
            !self.jobs.values().any(|j| j.profile == profile
                && j.spec.lesson == spec.lesson
                && (j.phase.active() || j.phase == Phase::Unknown)),
            "该课程存在活动或结果未知的意图，请先处理旧意图"
        );
        let id = self.next_id;
        let job = Intent::new(id, profile, spec, now_ms())?;
        self.next_id += 1;
        self.jobs.insert(id, job.clone());
        self.log(format!("意图 {id} 已接受"));
        Ok(Response::Job(job))
    }

    fn stop(&mut self, id: u64, target: Phase) -> Result<Response> {
        let job = self.jobs.get_mut(&id).context("意图不存在")?;
        ensure!(job.phase.active(), "意图已经结束；结果未知不能通过取消抹除");
        job.stop(target);
        let job = job.clone();
        self.log(format!("意图 {id} {target:?}"));
        Ok(Response::Job(job))
    }

    fn resume(&mut self, id: u64) -> Result<Response> {
        self.ensure_accepting()?;
        ensure!(self.reader.is_some(), "上下文未就绪");
        let profile = self.profile.clone();
        let job = self.jobs.get_mut(&id).context("意图不存在")?;
        ensure!(
            Some(&job.profile) == profile.as_ref(),
            "意图属于其他 profile"
        );
        job.resume(now_ms())?;
        Ok(Response::Job(job.clone()))
    }

    /// Validates a maintenance request and applies its forced cancellations.
    fn admit(&mut self, maintenance: &Maintenance) -> Result<()> {
        self.ensure_accepting()?;
        let active = |j: &&Intent| j.phase.active();
        match maintenance {
            Maintenance::Reconcile { id } => {
                let job = self.job(*id)?;
                ensure!(job.phase == Phase::Unknown, "只有 unknown 意图需要核对");
                ensure!(
                    self.profile.as_ref() == Some(&job.profile),
                    "请先显式选择该意图的 profile"
                );
            }
            Maintenance::Profile { id, force } => {
                ensure!(
                    !id.is_empty() && id.bytes().all(|b| b.is_ascii_digit()),
                    "profile 必须是数字 ID"
                );
                let conflicts: Vec<u64> = self
                    .jobs
                    .values()
                    .filter(active)
                    .filter(|j| j.profile != *id)
                    .map(|j| j.id)
                    .collect();
                ensure!(
                    *force || conflicts.is_empty(),
                    "存在非目标 profile 的意图 {conflicts:?}；使用 --force 取消它们后切换"
                );
                for id in conflicts {
                    self.jobs.get_mut(&id).unwrap().stop(Phase::Cancelled);
                }
            }
            Maintenance::Login { .. } => ensure!(
                !self.jobs.values().any(|j| j.phase.active()),
                "登录会替换会话，请先取消或结束活动意图"
            ),
            Maintenance::Logout { force } => {
                ensure!(
                    *force || !self.jobs.values().any(|j| j.phase.active()),
                    "存在活动意图；使用 logout --force"
                );
                for job in self.jobs.values_mut().filter(|j| j.phase.active()) {
                    job.stop(Phase::Cancelled);
                }
            }
            Maintenance::Refresh | Maintenance::Selected | Maintenance::Prepare => {
                ensure!(self.profile.is_some(), "未选择 profile");
            }
            Maintenance::Channels | Maintenance::Export { .. } | Maintenance::ClearCache => {}
        }
        Ok(())
    }

    pub(super) fn shutdown(&mut self) {
        self.stopping = true;
        if let Some(token) = &self.reading {
            token.cancel();
        }
        self.log("正在关停：停止新授权，等待在途请求及已接受的维护操作");
    }

    // ---- facts from the Executor and the read side ----

    pub(super) fn event(&mut self, event: Event) {
        match event {
            Event::Grant { id, reply } => {
                let granted = self.grant(id);
                let _ = reply.send(granted);
            }
            Event::JobDone {
                id,
                prepare_only,
                outcome,
                context,
            } => {
                self.busy = None;
                self.set_context(context);
                let job = self.jobs.get_mut(&id).expect("jobs are never removed");
                job.settle(outcome, prepare_only, now_ms());
                let line = format!("意图 {id}: {:?} {}", job.phase, job.last_result);
                self.log(line);
            }
            Event::MaintenanceDone { result, context } => {
                self.set_context(context);
                if let Ok(Response::Reconciled { id, selected }) = &result {
                    self.reconcile(*id, selected);
                }
                self.log(if result.is_ok() {
                    "维护操作完成"
                } else {
                    "维护操作失败，详情见命令响应"
                });
                if let Some(Busy::Maintenance(reply)) = self.busy.take() {
                    let _ = reply.send(result.map_err(|e| format!("{e:#}")));
                }
            }
            Event::Read(result) => {
                self.reading = None;
                match result {
                    Some(Ok(counts)) if self.reader.is_some() => self.accept_counts(counts),
                    Some(Err(error)) if self.reader.is_some() => self.read_failed(error),
                    _ => {} // cancelled, or the context was left while reading
                }
            }
        }
    }

    fn grant(&mut self, id: u64) -> bool {
        if self.stopping || self.pending.is_some() {
            return false;
        }
        let Some(job) = self.jobs.get(&id) else {
            return false;
        };
        if self.profile.as_deref() != Some(&job.profile)
            || (job.spec.mode == Mode::Watch && !self.vacancy(job))
        {
            return false;
        }
        self.jobs.get_mut(&id).unwrap().grant(now_ms())
    }

    fn set_context(&mut self, Context { profile, reader }: Context) {
        if reader.is_none() {
            self.counts = None;
        } else if self.reader.is_none() {
            self.next_read = 0; // fresh context: read right away
        }
        self.profile = profile;
        self.reader = reader;
    }

    fn reconcile(&mut self, id: u64, selected: &std::collections::HashMap<String, bool>) {
        let job = self.jobs.get_mut(&id).expect("jobs are never removed");
        let present = selected.get(&job.spec.lesson).copied().unwrap_or(false);
        let desired = job.spec.mode != Mode::Drop;
        job.phase = if present == desired {
            Phase::Succeeded
        } else {
            Phase::Failed
        };
        job.last_result = format!("人工发起核对：当前已选={present}，不推断原 POST 的执行结果");
    }

    fn accept_counts(&mut self, counts: Counts) {
        if let Some(profile) = &self.profile {
            let snapshot = parser::build_lesson_count_snapshot(profile, counts.clone());
            let _ = cache::save_count_snapshot(profile, &snapshot);
        }
        self.counts = Some((now_ms(), counts));
        self.read_error = None;
    }

    fn read_failed(&mut self, error: anyhow::Error) {
        self.counts = None;
        self.read_error = Some(format!("{error:#}"));
        self.log(format!("名额读取失败：{error:#}"));
        if http::transient_request_error(&error) {
            return;
        }
        let profile = self.profile.clone();
        for job in self.jobs.values_mut().filter(|j| {
            j.phase.active() && j.spec.mode == Mode::Watch && Some(&j.profile) == profile.as_ref()
        }) {
            job.stop(Phase::Paused);
        }
        self.log("非暂时性读取错误，watch 已暂停；修复会话后显式 resume");
    }

    // ---- scheduling ----

    fn vacancy(&self, job: &Intent) -> bool {
        self.counts.as_ref().is_some_and(|(at, counts)| {
            now_ms() - at <= job.spec.interval_ms.saturating_mul(3).max(2000) as i64
                && counts
                    .get(&job.spec.lesson)
                    .is_some_and(|c| c.limit - c.selected - c.reserved > 0)
        })
    }

    fn wants_dispatch(&self, job: &Intent, now: i64) -> bool {
        let submit = job.due(now)
            && (job.spec.mode != Mode::Watch || self.reader.is_none() || self.vacancy(job));
        let arm_early = job.spec.mode == Mode::Arm
            && job.phase == Phase::Waiting
            && !job.arm_prepared
            && now >= job.next_ms - ARM_LEAD_MS;
        submit || arm_early
    }

    /// Cancels the running capacity read, if any. True once none is running.
    fn quiesce_reads(&self) -> bool {
        match &self.reading {
            Some(token) => {
                token.cancel();
                false
            }
            None => true,
        }
    }

    /// Called before any Work that may change the page context.
    fn leave_context(&mut self) {
        self.reader = None;
        self.counts = None;
    }

    pub(super) fn expire(&mut self, now: i64) {
        let mut expired = Vec::new();
        for job in self.jobs.values_mut() {
            if matches!(job.phase, Phase::Waiting | Phase::Preparing | Phase::Paused)
                && job.expired(now)
            {
                job.phase = Phase::Expired;
                expired.push(job.id);
            }
        }
        for id in expired {
            self.log(format!("意图 {id} 已过截止时间"));
        }
    }

    pub(super) fn next_work(&mut self, now: i64) -> Option<Work> {
        if self.busy.is_some() {
            return None;
        }
        if self.pending.is_some() {
            if !self.quiesce_reads() {
                return None; // the cancelled read's event wakes us
            }
            let (maintenance, reply) = self.pending.take()?;
            self.leave_context();
            self.busy = Some(Busy::Maintenance(reply));
            return Some(Work::Maintenance(maintenance));
        }
        if self.stopping {
            return None;
        }
        let profile = self.profile.as_deref()?;
        let id = self
            .jobs
            .values()
            .filter(|j| j.profile == profile && j.phase == Phase::Waiting)
            .filter(|j| self.wants_dispatch(j, now))
            .min_by_key(|j| (j.id <= self.last_dispatched, j.id))?
            .id;
        let ready = self.reader.is_some();
        let job = self.jobs.get_mut(&id)?;
        if job.spec.dry_run && ready {
            job.next_ms = now + job.spec.interval_ms as i64;
            job.last_result = "有空位（dry-run，未提交）".into();
            self.log(format!("意图 {id}: dry-run 有空位"));
            return self.next_work(now);
        }
        let prepare_only = (job.spec.mode == Mode::Watch && !ready)
            || (job.spec.mode == Mode::Arm && now < job.next_ms);
        if prepare_only || !ready {
            if !self.quiesce_reads() {
                return None;
            }
            self.leave_context();
        }
        let job = self.jobs.get_mut(&id)?;
        job.arm_prepared |= prepare_only;
        job.phase = Phase::Preparing;
        let job = job.clone();
        self.busy = Some(Busy::Job(id));
        self.last_dispatched = id;
        Some(Work::Advance { job, prepare_only })
    }

    fn watch_interval(&self) -> Option<u64> {
        self.jobs
            .values()
            .filter(|j| {
                j.spec.mode == Mode::Watch
                    && Some(&j.profile) == self.profile.as_ref()
                    && matches!(j.phase, Phase::Waiting | Phase::Preparing | Phase::InFlight)
            })
            .map(|j| j.spec.interval_ms)
            .min()
    }

    /// One shared poller for all watches, at the shortest active interval.
    pub(super) fn next_read(&mut self, now: i64) -> Option<(CountReader, CancellationToken)> {
        if self.stopping || self.maintaining() || self.reading.is_some() || now < self.next_read {
            return None;
        }
        let interval = self.watch_interval()?;
        let reader = self.reader.clone()?;
        self.next_read = now + interval as i64;
        let token = CancellationToken::new();
        self.reading = Some(token.clone());
        Some((reader, token))
    }

    /// The earliest future moment a timer (not an event) could change a decision.
    /// Past moments are skipped: they are blocked on something that will send an event.
    pub(super) fn next_wake(&self, now: i64) -> Option<i64> {
        self.jobs
            .values()
            .filter(|j| j.phase.active())
            .flat_map(|j| {
                [
                    Some(j.next_ms),
                    Some(j.next_ms - ARM_LEAD_MS),
                    j.deadline_ms,
                ]
            })
            .flatten()
            .chain(self.watch_interval().map(|_| self.next_read))
            .filter(|&at| at > now)
            .min()
    }

    pub(super) fn drained(&self) -> bool {
        self.stopping && self.busy.is_none() && self.pending.is_none() && self.reading.is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app::intent::Outcome;

    fn spec(lesson: &str, mode: Mode) -> Spec {
        Spec {
            lesson: lesson.into(),
            mode,
            interval_ms: 1000,
            attempts: 1,
            timeout_ms: 0,
            at_ms: None,
            dry_run: false,
        }
    }

    /// A Manager with profile "1" selected. There is no reader (it needs a live
    /// Session), so every dispatch is a context-changing one.
    fn manager(jobs: &[(&str, Mode, Option<i64>)]) -> Manager {
        let mut m = Manager::new();
        m.profile = Some("1".into());
        for &(lesson, mode, at) in jobs {
            let mut spec = spec(lesson, mode);
            spec.at_ms = at;
            let id = m.next_id;
            m.jobs
                .insert(id, Intent::new(id, "1".into(), spec, 0).unwrap());
            m.next_id += 1;
        }
        m
    }

    fn advance(work: Option<Work>) -> (u64, bool) {
        match work {
            Some(Work::Advance { job, prepare_only }) => (job.id, prepare_only),
            _ => panic!("expected an Advance"),
        }
    }

    fn done(m: &mut Manager, id: u64, prepare_only: bool, outcome: Outcome) {
        m.event(Event::JobDone {
            id,
            prepare_only,
            outcome,
            context: Context {
                profile: Some("1".into()),
                reader: None,
            },
        });
    }

    #[test]
    fn executor_is_serial_and_dispatch_is_round_robin() {
        let mut m = manager(&[("10", Mode::Fire, None), ("11", Mode::Fire, None)]);
        m.jobs.get_mut(&1).unwrap().spec.attempts = 0;
        assert_eq!(advance(m.next_work(0)), (1, false));
        assert!(
            m.next_work(0).is_none(),
            "busy executor gets no second Work"
        );
        done(&mut m, 1, false, Outcome::BeforeError("net".into()));
        m.jobs.get_mut(&1).unwrap().next_ms = 0;
        assert_eq!(advance(m.next_work(0)), (2, false));
    }

    #[test]
    fn watch_without_context_only_prepares() {
        let mut m = manager(&[("10", Mode::Watch, None)]);
        assert_eq!(advance(m.next_work(0)), (1, true));
    }

    #[test]
    fn arm_prepares_early_once_then_fires_at_time() {
        let at = 60_000;
        let mut m = manager(&[("10", Mode::Arm, Some(at))]);
        assert!(m.next_work(at - ARM_LEAD_MS - 1).is_none());
        assert_eq!(m.next_wake(0), Some(at - ARM_LEAD_MS));
        assert_eq!(advance(m.next_work(at - ARM_LEAD_MS)), (1, true));
        done(&mut m, 1, true, Outcome::Prepared);
        assert!(m.next_work(at - 1).is_none());
        assert_eq!(m.next_wake(at - 1), Some(at));
        assert_eq!(advance(m.next_work(at)), (1, false));
    }

    #[test]
    fn pending_maintenance_blocks_grants_and_waits_for_reads() {
        let mut m = manager(&[("10", Mode::Fire, None)]);
        advance(m.next_work(0));
        let (reply, _) = oneshot::channel();
        m.dispatch(Envelope {
            command: Maintenance::Refresh.into(),
            reply,
        });
        assert!(!m.grant(1), "no grant while maintenance is pending");
        done(&mut m, 1, false, Outcome::Skipped);
        let read = CancellationToken::new();
        m.reading = Some(read.clone());
        assert!(m.next_work(0).is_none());
        assert!(read.is_cancelled());
        m.event(Event::Read(None));
        assert!(matches!(m.next_work(0), Some(Work::Maintenance(_))));
    }

    #[test]
    fn past_moments_do_not_cause_busy_wakeups() {
        let mut m = manager(&[("10", Mode::Fire, None)]);
        advance(m.next_work(0));
        assert_eq!(m.next_wake(10), None);
    }

    #[test]
    fn shutdown_drains_only_after_inflight_work() {
        let mut m = manager(&[("10", Mode::Fire, None)]);
        advance(m.next_work(0));
        m.shutdown();
        assert!(!m.grant(1) && !m.drained());
        done(&mut m, 1, false, Outcome::Skipped);
        assert!(m.drained());
    }
}
