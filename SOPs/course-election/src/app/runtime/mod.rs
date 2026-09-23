//! Daemon runtime. The Manager owns intent state and decides; the Executor owns the
//! Session and acts, one Work at a time; capacity reads run beside it.
mod executor;
mod manager;

use crate::{
    app::{
        http::CountReader,
        intent::{Intent, Outcome, now_ms},
        protocol::{Command, Maintenance, Reply, Response},
    },
    model::LessonCount,
};
use anyhow::{Result, bail};
use manager::Manager;
use std::{collections::HashMap, time::Duration};
use tokio::sync::{mpsc, oneshot};
use tokio_util::sync::CancellationToken;

/// Upper bound on sleeping when nothing is scheduled; any event wakes the loop earlier.
const IDLE: Duration = Duration::from_secs(3600);

pub(crate) struct Envelope {
    pub command: Command,
    pub reply: oneshot::Sender<Reply>,
}

type Counts = HashMap<String, LessonCount>;

enum Work {
    Maintenance(Maintenance),
    Advance { job: Intent, prepare_only: bool },
}

/// The Executor's page context after a Work; `reader` is present only when it is ready.
struct Context {
    profile: Option<String>,
    reader: Option<CountReader>,
}

enum Event {
    Grant {
        id: u64,
        reply: oneshot::Sender<bool>,
    },
    JobDone {
        id: u64,
        prepare_only: bool,
        outcome: Outcome,
        context: Context,
    },
    MaintenanceDone {
        result: Result<Response>,
        context: Context,
    },
    /// `None` when the read was cancelled.
    Read(Option<Result<Counts>>),
}

pub(crate) async fn run(
    mut incoming: mpsc::Receiver<Envelope>,
    shutdown: CancellationToken,
) -> Result<()> {
    let mut manager = Manager::new();
    let (events, mut results) = mpsc::channel(64);
    let (work, rx) = mpsc::channel(1);
    let mut worker = tokio::spawn(executor::run(rx, events.clone()));
    manager.log("daemon 已启动；执行 profile <id> 显式选择上下文");
    let mut delay = Duration::ZERO;
    loop {
        tokio::select! {
            Some(envelope) = incoming.recv() => manager.dispatch(envelope),
            Some(event) = results.recv() => manager.event(event),
            () = tokio::time::sleep(delay) => {}
            () = shutdown.cancelled(), if !manager.stopping() => manager.shutdown(),
            result = &mut worker => bail!("Executor 意外退出：{result:?}"),
        }
        let now = now_ms();
        manager.expire(now);
        if let Some(next) = manager.next_work(now) {
            work.send(next).await?;
        }
        if let Some((reader, token)) = manager.next_read(now) {
            let events = events.clone();
            tokio::spawn(async move {
                let result = token.run_until_cancelled(reader.fetch()).await;
                let _ = events.send(Event::Read(result)).await;
            });
        }
        if manager.drained() {
            break;
        }
        delay = manager
            .next_wake(now)
            .map_or(IDLE, |at| Duration::from_millis((at - now) as u64));
    }
    drop(work);
    worker.await?
}
