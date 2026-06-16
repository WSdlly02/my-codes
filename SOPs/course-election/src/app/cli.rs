use clap::{Args, Parser, Subcommand};
use std::time::Duration;

use crate::app::support::parse_duration_arg;

#[derive(Parser)]
#[command(name = "course-election", version, about = "选课 CLI 的 Rust 版本")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) command: Commands,
}

#[derive(Subcommand)]
pub(crate) enum Commands {
    Status,
    Query(QueryArgs),
    Warmup(WarmupArgs),
    FlushState(FlushStateArgs),
    Select(ActionArgs),
    Drop(ActionArgs),
}

#[derive(Args)]
pub(crate) struct QueryArgs {
    #[arg(long)]
    pub(crate) profile: Option<String>,
    #[arg(long)]
    pub(crate) name: Option<String>,
    #[arg(long = "lesson-id")]
    pub(crate) lesson_id: Option<String>,
    #[arg(long)]
    pub(crate) code: Option<String>,
    #[arg(long = "selected-lessons")]
    pub(crate) selected_lessons: bool,
    #[arg(long = "class-schedule")]
    pub(crate) class_schedule: bool,
    #[arg(long = "semester-id")]
    pub(crate) semester_id: Option<String>,
}

#[derive(Args)]
pub(crate) struct WarmupArgs {
    #[arg(long)]
    pub(crate) username: Option<String>,
    #[arg(long)]
    pub(crate) password: Option<String>,
    #[arg(long = "autofill-captcha")]
    pub(crate) autofill_captcha: bool,
}

#[derive(Args)]
pub(crate) struct FlushStateArgs {
    #[arg(long)]
    pub(crate) all: bool,
}

#[derive(Args, Clone)]
pub(crate) struct ActionArgs {
    #[arg(long)]
    pub(crate) profile: Option<String>,
    #[arg(long = "lesson-id")]
    pub(crate) lesson_id: Option<String>,
    #[arg(long = "name")]
    pub(crate) course_name: Option<String>,
    #[arg(long, default_value_t = 1)]
    pub(crate) retry: usize,
    #[arg(long, default_value = "500ms", value_parser = parse_duration_arg)]
    pub(crate) interval: Duration,
    #[arg(long = "skip-session-check")]
    pub(crate) skip_session_check: bool,
}
