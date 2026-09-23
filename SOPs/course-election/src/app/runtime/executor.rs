//! The only Session owner. Prepares, obtains a grant, submits once, then reports back.
use super::{Context, Event, Work};
use crate::{
    app::{
        cache,
        http::{self, Session},
        intent::{Intent, Outcome, now_ms},
        login, parser,
        protocol::{Maintenance, Mode, Response},
    },
    model::SelectedSnapshot,
};
use anyhow::{Context as _, Result};
use tokio::sync::{mpsc, oneshot};

pub(super) async fn run(mut work: mpsc::Receiver<Work>, events: mpsc::Sender<Event>) -> Result<()> {
    let mut session = match cache::load_saved_cookies() {
        Ok(saved) => Session::new(saved.cookies)?,
        Err(_) => Session::empty()?,
    };
    let mut profile: Option<String> = None;
    while let Some(next) = work.recv().await {
        let event = match next {
            Work::Maintenance(maintenance) => {
                let result = maintain(maintenance, &mut session, &mut profile).await;
                Event::MaintenanceDone {
                    result,
                    context: context(&session, &profile),
                }
            }
            Work::Advance { job, prepare_only } => {
                let outcome = advance(&job, prepare_only, &mut session, &profile, &events).await;
                Event::JobDone {
                    id: job.id,
                    prepare_only,
                    outcome,
                    context: context(&session, &profile),
                }
            }
        };
        if let Err(e) = session.persist_cookies() {
            tracing::warn!("保存 Cookie 失败：{e:#}");
        }
        if events.send(event).await.is_err() {
            break;
        }
    }
    Ok(())
}

fn context(session: &Session, profile: &Option<String>) -> Context {
    Context {
        profile: profile.clone(),
        reader: profile
            .as_deref()
            .filter(|p| session.election_ready(p))
            .map(|p| session.count_reader(p)),
    }
}

async fn advance(
    job: &Intent,
    prepare_only: bool,
    session: &mut Session,
    profile: &Option<String>,
    events: &mpsc::Sender<Event>,
) -> Outcome {
    if profile.as_deref() != Some(&job.profile) {
        return Outcome::BeforeError("profile 已改变".into());
    }
    if prepare_only || !session.election_ready(&job.profile) {
        if prepare_only && job.spec.mode == Mode::Arm {
            let _ = http::prewarm(session).await;
        }
        if let Err(e) = session.prepare_election(&job.profile).await {
            return Outcome::BeforeError(format!("{e:#}"));
        }
        if prepare_only {
            return Outcome::Prepared;
        }
    }
    let (reply, granted) = oneshot::channel();
    let grant = Event::Grant { id: job.id, reply };
    if events.send(grant).await.is_err() || !granted.await.unwrap_or(false) {
        return Outcome::Skipped;
    }
    // Granted: nothing but the POST itself happens from here on.
    let select = job.spec.mode != Mode::Drop;
    match session
        .submit_ready(&job.profile, &job.spec.lesson, select)
        .await
    {
        Ok(body) if parser::selection_response_recognized(&body) => Outcome::Response(body),
        Ok(body) => Outcome::Unknown(format!(
            "未识别的提交响应，禁止自动重试：{}",
            parser::summarize_selection_response(&body)
        )),
        Err(e) => Outcome::Unknown(format!("提交结果未知，禁止自动重试：{e:#}")),
    }
}

async fn maintain(
    maintenance: Maintenance,
    session: &mut Session,
    profile: &mut Option<String>,
) -> Result<Response> {
    match maintenance {
        Maintenance::Reconcile { id } => {
            let profile = profile.as_deref().context("未选择 profile")?;
            let selected = http::fetch_elected_lesson_ids(session, profile).await?;
            Ok(Response::Reconciled { id, selected })
        }
        Maintenance::Profile { id, .. } => {
            *profile = None;
            session.select_profile(&id).await?;
            *profile = Some(id.clone());
            Ok(Response::Profile { profile: id })
        }
        Maintenance::Login { username, password } => {
            *profile = None;
            // Never keep an old login session usable after a failed account change.
            *session = Session::empty()?;
            cache::clear_login_state()?;
            *session = login::login(&username, &password).await?;
            Ok(Response::LoggedIn)
        }
        Maintenance::Logout { .. } => {
            *profile = None;
            *session = Session::empty()?;
            cache::clear_login_state()?;
            Ok(Response::LoggedOut)
        }
        Maintenance::Refresh => {
            let profile = profile.as_deref().context("未选择 profile")?;
            let data = http::refresh_course_data(session, profile).await?;
            Ok(Response::Refreshed {
                mapping: data.mapping.lessons.len(),
                counts: data.counts.as_ref().map(|c| c.counts.len()),
                counts_from_cache: data.counts_from_cache,
            })
        }
        Maintenance::Selected => {
            let profile = profile.as_deref().context("未选择 profile")?;
            let snapshot = SelectedSnapshot {
                profile: profile.into(),
                at_ms: now_ms(),
                selected: http::fetch_elected_lesson_ids(session, profile).await?,
            };
            cache::save_selected_snapshot(&snapshot)?;
            Ok(Response::Selected(snapshot))
        }
        Maintenance::Channels => Ok(Response::Channels(
            http::fetch_and_cache_channels(session).await?,
        )),
        Maintenance::Export { semester } => Ok(Response::Exported {
            html: http::query_class_schedule_html(session, &semester).await?,
            semester,
        }),
        Maintenance::Prepare => {
            let profile = profile.as_deref().context("未选择 profile")?;
            http::prewarm(session).await?;
            session.prepare_election(profile).await?;
            Ok(Response::Prepared)
        }
        Maintenance::ClearCache => {
            cache::clear_derived_caches()?;
            Ok(Response::Cleared)
        }
    }
}
