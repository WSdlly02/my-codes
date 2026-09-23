//! Daemon runtime: a registry of running intent tasks, one Executor actor that owns the
//! Session, and one capacity poller shared by all watches.
mod executor;
mod intent;

use crate::{
    app::{
        cache,
        http::CountReader,
        parser,
        protocol::{
            Command, IntentView, LogEvent, Maintenance, Progress, Response, Spec, Status, Trigger,
        },
        support::now_ms,
    },
    model::LessonCount,
};
use anyhow::{Context as _, Result, bail, ensure};
use executor::Executor;
use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    sync::{Arc, Mutex},
    time::Duration,
};
use tokio::{
    sync::{RwLock, watch},
    time::{MissedTickBehavior, interval},
};
use tokio_util::{sync::CancellationToken, task::TaskTracker};

const LOG_CAPACITY: usize = 512;

/// The Executor's page context. `reader` is present only while the page is open.
/// An intent lives only as long as `profile` is its own.
#[derive(Clone, Default)]
struct Context {
    profile: Option<String>,
    reader: Option<CountReader>,
}

/// One capacity read.
pub(super) struct Capacity {
    profile: String,
    at_ms: i64,
    result: Result<HashMap<String, LessonCount>, String>,
}

impl Capacity {
    fn vacant(&self, lesson: &str) -> bool {
        self.result.as_ref().is_ok_and(|counts| {
            counts
                .get(lesson)
                .is_some_and(|c| c.limit - c.selected - c.reserved > 0)
        })
    }
}

struct Handle {
    view: IntentView,
    stop: CancellationToken,
    progress: watch::Receiver<Progress>,
}

impl Handle {
    fn view(&self) -> IntentView {
        IntentView {
            progress: self.progress.borrow().clone(),
            ..self.view.clone()
        }
    }
}

#[derive(Default)]
struct State {
    next_id: u64,
    intents: BTreeMap<u64, Handle>,
    logs: VecDeque<LogEvent>,
    sequence: u64,
}

pub(crate) struct Runtime {
    exec: Executor,
    context: watch::Receiver<Context>,
    capacity: watch::Sender<Option<Capacity>>,
    poll: Duration,
    /// Never held across an await.
    state: Mutex<State>,
    /// Parent of every intent's token; cancelled on shutdown.
    shutdown: CancellationToken,
    tasks: TaskTracker,
}

impl Runtime {
    pub(crate) fn start(poll: Duration) -> Result<Arc<Self>> {
        let (context_tx, context) = watch::channel(Context::default());
        let gate = Arc::new(RwLock::new(()));
        let runtime = Arc::new(Self {
            exec: executor::spawn(context_tx, gate.clone())?,
            context,
            capacity: watch::channel(None).0,
            poll,
            state: Mutex::default(),
            shutdown: CancellationToken::new(),
            tasks: TaskTracker::new(),
        });
        tokio::spawn(runtime.clone().poll_capacity(gate));
        runtime.log("daemon 已启动；执行 profile use <ID> 进入选课轮次");
        Ok(runtime)
    }

    /// Stops all waiting, then waits for submissions already handed to the Executor.
    pub(crate) async fn shutdown(&self) {
        self.log("正在关停：取消全部意图，等待已提交的请求收尾");
        self.shutdown.cancel();
        self.tasks.close();
        self.tasks.wait().await;
    }

    fn log(&self, message: impl Into<String>) {
        let message = message.into();
        tracing::info!("{message}");
        let mut state = self.state.lock().unwrap();
        state.sequence += 1;
        let event = LogEvent {
            sequence: state.sequence,
            at_ms: now_ms(),
            message,
        };
        if state.logs.len() == LOG_CAPACITY {
            state.logs.pop_front();
        }
        state.logs.push_back(event);
    }

    pub(crate) async fn handle(self: &Arc<Self>, command: Command) -> Result<Response> {
        match command {
            Command::Status => Ok(Response::Status(self.status())),
            Command::Jobs => {
                let state = self.state.lock().unwrap();
                Ok(Response::Jobs(
                    state.intents.values().map(Handle::view).collect(),
                ))
            }
            Command::Job { id } => {
                let state = self.state.lock().unwrap();
                Ok(Response::Job(state.intents.get(&id).context(GONE)?.view()))
            }
            Command::Wait { id } => {
                let (view, progress) = {
                    let state = self.state.lock().unwrap();
                    let handle = state.intents.get(&id).context(GONE)?;
                    (handle.view.clone(), handle.progress.clone())
                };
                Ok(Response::Job(wait_end(view, progress).await))
            }
            Command::Logs => {
                let state = self.state.lock().unwrap();
                Ok(Response::Logs {
                    events: state.logs.iter().cloned().collect(),
                    latest: state.sequence,
                })
            }
            Command::Add { spec, wait } => {
                let (view, progress) = self.add(spec)?;
                if wait {
                    return Ok(Response::Job(wait_end(view, progress).await));
                }
                Ok(Response::Job(IntentView {
                    progress: progress.borrow().clone(),
                    ..view
                }))
            }
            Command::Cancel { id } => {
                let view = {
                    let state = self.state.lock().unwrap();
                    let handle = state.intents.get(&id).context(GONE)?;
                    handle.stop.cancel();
                    handle.view()
                };
                self.log(format!("意图 {id} 已取消等待"));
                Ok(Response::Job(view))
            }
            Command::Maintenance(maintenance) => {
                ensure!(!self.shutdown.is_cancelled(), "正在关停");
                if let Maintenance::UseProfile { id } = &maintenance {
                    ensure!(
                        !id.is_empty() && id.bytes().all(|b| b.is_ascii_digit()),
                        "profile 必须是数字 ID"
                    );
                }
                // Intents of a replaced context end by themselves once it is published.
                let what = maintenance.to_string();
                let result = self.exec.maintain(maintenance).await;
                self.log(match &result {
                    Ok(_) => format!("{what}：完成"),
                    Err(e) => format!("{what}：失败，{e:#}"),
                });
                result
            }
        }
    }

    fn status(&self) -> Status {
        let intents = self.state.lock().unwrap().intents.len();
        let context = self.context.borrow();
        let capacity = self.capacity.borrow();
        Status {
            profile: context.profile.clone(),
            context_ready: context.reader.is_some(),
            stopping: self.shutdown.is_cancelled(),
            poll_ms: self.poll.as_millis() as u64,
            counts_at_ms: capacity.as_ref().map(|c| c.at_ms),
            read_error: capacity.as_ref().and_then(|c| c.result.clone().err()),
            intents,
        }
    }

    fn add(self: &Arc<Self>, mut spec: Spec) -> Result<(IntentView, watch::Receiver<Progress>)> {
        ensure!(!self.shutdown.is_cancelled(), "正在关停");
        let context = self.context.borrow().clone();
        let profile = context
            .profile
            .filter(|_| context.reader.is_some())
            .context("请先 profile use <ID> 进入选课轮次")?;
        if let Trigger::Fire { attempts, .. } = spec.trigger {
            ensure!(attempts >= 1, "attempts 至少为 1");
        }
        if !spec.lesson.bytes().all(|b| b.is_ascii_digit()) {
            spec.lesson = parser::resolve_lesson_id_by_name(
                &cache::load_mapping_cache(&profile)?,
                &spec.lesson,
            )?;
        }
        let (progress_tx, progress) = watch::channel(Progress::default());
        let stop = self.shutdown.child_token();
        let view = {
            let mut state = self.state.lock().unwrap();
            if state
                .intents
                .values()
                .any(|h| h.view.profile == profile && h.view.spec.lesson == spec.lesson)
            {
                bail!("该课程已有运行中的意图");
            }
            state.next_id += 1;
            let view = IntentView {
                id: state.next_id,
                profile: profile.clone(),
                spec: spec.clone(),
                progress: Progress::default(),
            };
            state.intents.insert(
                view.id,
                Handle {
                    view: view.clone(),
                    stop: stop.clone(),
                    progress: progress.clone(),
                },
            );
            view
        };
        let task = intent::Intent {
            profile,
            lesson: spec.lesson,
            trigger: spec.trigger,
            exec: self.exec.clone(),
            context: self.context.clone(),
            capacity: self.capacity.subscribe(),
            poll: self.poll,
            stop,
            progress: progress_tx,
        };
        let id = view.id;
        let runtime = self.clone();
        let ended = progress.clone();
        self.tasks.spawn(async move {
            task.run().await;
            runtime.state.lock().unwrap().intents.remove(&id);
            let end = ended.borrow().clone();
            let detail = if end.last_result.is_empty() {
                String::new()
            } else {
                format!("（{}）", end.last_result)
            };
            runtime.log(format!("意图 {id} 结束：{}{detail}", end.phase));
        });
        self.log(format!("意图 {id} 已接受"));
        Ok((view, progress))
    }

    fn watching(&self) -> bool {
        let state = self.state.lock().unwrap();
        state
            .intents
            .values()
            .any(|h| matches!(h.view.spec.trigger, Trigger::Watch { .. }))
    }

    /// Reads capacity for all watches at one shared interval, and only while one runs.
    async fn poll_capacity(self: Arc<Self>, gate: Arc<RwLock<()>>) {
        let mut tick = interval(self.poll);
        tick.set_missed_tick_behavior(MissedTickBehavior::Delay);
        loop {
            tokio::select! {
                () = self.shutdown.cancelled() => return,
                _ = tick.tick() => {}
            }
            if !self.watching() {
                continue;
            }
            let _reading = gate.read().await; // no page change while reading
            let Context {
                profile: Some(profile),
                reader: Some(reader),
            } = self.context.borrow().clone()
            else {
                continue;
            };
            let result = reader.fetch().await;
            match &result {
                Ok(counts) => {
                    let snapshot = parser::build_lesson_count_snapshot(&profile, counts.clone());
                    let _ = cache::save_count_snapshot(&profile, &snapshot);
                }
                Err(e) => tracing::warn!("名额读取失败：{e:#}"),
            }
            self.capacity.send_replace(Some(Capacity {
                profile,
                at_ms: now_ms(),
                result: result.map_err(|e| format!("{e:#}")),
            }));
        }
    }
}

const GONE: &str = "意图不存在或已结束；结果见 logs";

async fn wait_end(view: IntentView, mut progress: watch::Receiver<Progress>) -> IntentView {
    // The task's final value stays readable after its sender is dropped.
    let _ = progress.wait_for(|p| p.phase.ended()).await;
    let progress = progress.borrow().clone();
    IntentView { progress, ..view }
}
