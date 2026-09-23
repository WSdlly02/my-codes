//! The only Session owner: an actor that runs one request at a time, in arrival order.
//!
//! Page context changes (reloading defaultPage, login, profile switch) happen under the
//! write side of `gate`; capacity reads hold the read side. So no read overlaps a change,
//! and the reader published in `context` always matches the server-side page.
use super::Context;
use crate::{
    app::{
        cache,
        http::{self, Session},
        login, parser,
        protocol::{Maintenance, Response},
        support::now_ms,
    },
    model::SelectedSnapshot,
};
use anyhow::{Context as _, Result, anyhow};
use std::sync::Arc;
use tokio::sync::{RwLock, mpsc, oneshot, watch};

pub(super) enum Submitted {
    Succeeded(String),
    /// The server refused (full, not open yet, ...); retrying is safe.
    Rejected(String),
    /// Nothing was sent.
    NotSent(String),
    /// A POST may have been executed; never retried.
    Unknown(String),
}

pub(super) enum Request {
    Prepare {
        profile: String,
        prewarm: bool,
        reply: oneshot::Sender<Result<()>>,
    },
    Submit {
        profile: String,
        lesson: String,
        select: bool,
        reply: oneshot::Sender<Submitted>,
    },
    Maintain {
        maintenance: Maintenance,
        reply: oneshot::Sender<Result<Response>>,
    },
}

#[derive(Clone)]
pub(super) struct Executor(mpsc::Sender<Request>);

const STOPPED: &str = "Executor 已停止";

/// An Executor whose requests the test answers by hand.
#[cfg(test)]
pub(super) fn fake() -> (Executor, mpsc::Receiver<Request>) {
    let (sender, requests) = mpsc::channel(8);
    (Executor(sender), requests)
}

impl Executor {
    /// Opens the election page unless it is already open; never rotates a valid token.
    pub(super) async fn prepare(&self, profile: &str, prewarm: bool) -> Result<()> {
        let (reply, result) = oneshot::channel();
        let profile = profile.into();
        self.0
            .send(Request::Prepare {
                profile,
                prewarm,
                reply,
            })
            .await
            .map_err(|_| anyhow!(STOPPED))?;
        result.await.map_err(|_| anyhow!(STOPPED))?
    }

    /// Exactly one POST (after opening the page if needed). Once sent, it always completes.
    pub(super) async fn submit(&self, profile: &str, lesson: &str, select: bool) -> Submitted {
        let (reply, result) = oneshot::channel();
        let request = Request::Submit {
            profile: profile.into(),
            lesson: lesson.into(),
            select,
            reply,
        };
        if self.0.send(request).await.is_err() {
            return Submitted::NotSent(STOPPED.into());
        }
        result
            .await
            .unwrap_or_else(|_| Submitted::Unknown(format!("{STOPPED}，提交结果未知")))
    }

    pub(super) async fn maintain(&self, maintenance: Maintenance) -> Result<Response> {
        let (reply, result) = oneshot::channel();
        self.0
            .send(Request::Maintain { maintenance, reply })
            .await
            .map_err(|_| anyhow!(STOPPED))?;
        result.await.map_err(|_| anyhow!(STOPPED))?
    }
}

pub(super) fn spawn(context: watch::Sender<Context>, gate: Arc<RwLock<()>>) -> Result<Executor> {
    let session = match cache::load_saved_cookies() {
        Ok(saved) => Session::new(saved.cookies)?,
        Err(_) => Session::empty()?,
    };
    let (sender, requests) = mpsc::channel(64);
    let actor = Actor {
        session,
        profile: None,
        context,
        gate,
    };
    tokio::spawn(actor.run(requests));
    Ok(Executor(sender))
}

struct Actor {
    session: Session,
    profile: Option<String>,
    context: watch::Sender<Context>,
    gate: Arc<RwLock<()>>,
}

impl Actor {
    async fn run(mut self, mut requests: mpsc::Receiver<Request>) {
        while let Some(request) = requests.recv().await {
            match request {
                Request::Prepare {
                    profile,
                    prewarm,
                    reply,
                } => {
                    let result = self.ensure_page(&profile, prewarm).await;
                    let _ = reply.send(result);
                }
                Request::Submit {
                    profile,
                    lesson,
                    select,
                    reply,
                } => {
                    let submitted = self.submit(&profile, &lesson, select).await;
                    let _ = reply.send(submitted);
                }
                Request::Maintain { maintenance, reply } => {
                    let gate = self.gate.clone();
                    let _changing = gate.write().await;
                    let result = self.maintain(maintenance).await;
                    self.publish();
                    let _ = reply.send(result);
                }
            }
            if let Err(e) = self.session.persist_cookies() {
                tracing::warn!("保存 Cookie 失败：{e:#}");
            }
        }
    }

    fn publish(&self) {
        let reader = self
            .profile
            .as_deref()
            .filter(|p| self.session.election_ready(p))
            .map(|p| self.session.count_reader(p));
        self.context.send_replace(Context {
            profile: self.profile.clone(),
            reader,
        });
    }

    async fn ensure_page(&mut self, profile: &str, prewarm: bool) -> Result<()> {
        anyhow::ensure!(self.profile.as_deref() == Some(profile), "profile 已改变");
        if prewarm {
            let _ = http::prewarm(&self.session).await;
        }
        if !self.session.election_ready(profile) {
            let gate = self.gate.clone();
            let _changing = gate.write().await;
            let result = self.session.prepare_election(profile).await;
            self.publish();
            result?;
        }
        Ok(())
    }

    async fn submit(&mut self, profile: &str, lesson: &str, select: bool) -> Submitted {
        if let Err(e) = self.ensure_page(profile, false).await {
            return Submitted::NotSent(format!("{e:#}"));
        }
        let result = self.session.submit_ready(profile, lesson, select).await;
        if !self.session.election_ready(profile) {
            self.publish(); // the server invalidated the page
        }
        match result {
            Ok(body) => {
                let message = parser::summarize_selection_response(&body);
                if parser::selection_succeeded(&body) {
                    Submitted::Succeeded(message)
                } else if parser::selection_response_recognized(&body) {
                    Submitted::Rejected(message)
                } else {
                    Submitted::Unknown(format!("未识别的提交响应：{message}"))
                }
            }
            Err(e) => Submitted::Unknown(format!("提交结果未知：{e:#}")),
        }
    }

    async fn maintain(&mut self, maintenance: Maintenance) -> Result<Response> {
        let session = &mut self.session;
        match maintenance {
            Maintenance::Login { username, password } => {
                self.profile = None;
                // Never keep an old login session usable after a failed account change.
                *session = Session::empty()?;
                cache::clear_login_state()?;
                *session = login::login(&username, &password).await?;
                Ok(Response::LoggedIn)
            }
            Maintenance::Logout => {
                self.profile = None;
                *session = Session::empty()?;
                cache::clear_login_state()?;
                Ok(Response::LoggedOut)
            }
            Maintenance::SyncProfiles => Ok(Response::Profiles(
                http::fetch_and_cache_channels(session).await?,
            )),
            Maintenance::UseProfile { id } => {
                self.profile = None;
                session.select_profile(&id).await?;
                self.profile = Some(id.clone());
                Ok(Response::ProfileInUse { profile: id })
            }
            Maintenance::SyncCourses => {
                let profile = self.profile.as_deref().context("未进入 profile")?;
                let data = http::refresh_course_data(session, profile).await?;
                Ok(Response::CoursesSynced {
                    courses: data.mapping.lessons.len(),
                    counts: data.counts.as_ref().map(|c| c.counts.len()),
                    counts_from_cache: data.counts_from_cache,
                })
            }
            Maintenance::SyncSelected => {
                let profile = self.profile.as_deref().context("未进入 profile")?;
                let snapshot = SelectedSnapshot {
                    profile: profile.into(),
                    at_ms: now_ms(),
                    selected: http::fetch_elected_lesson_ids(session, profile).await?,
                };
                cache::save_selected_snapshot(&snapshot)?;
                Ok(Response::Selected(snapshot))
            }
            Maintenance::ExportSchedule { semester } => Ok(Response::Schedule {
                html: http::query_class_schedule_html(session, &semester).await?,
                semester,
            }),
        }
    }
}
