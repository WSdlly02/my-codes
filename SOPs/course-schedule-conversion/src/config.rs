use std::path::PathBuf;

use chrono::Local;
use clap::{ArgAction, Parser};

#[derive(Debug, Parser)]
#[command(
    name = "course-schedule-conversion",
    version,
    about = "将课程表 HTML 转换为 ICS 日历文件"
)]
pub struct Config {
    #[arg(
        long,
        default_value = "/home/wsdlly02/Documents/my-codes/SOPs/course-election/course-election",
        help = "course-election 可执行文件路径"
    )]
    pub course_election_bin: PathBuf,

    #[arg(long, help = "直接读取本地 HTML 文件，跳过外部命令查询")]
    pub html_file: Option<PathBuf>,

    #[arg(
        long,
        default_value = "2026-03-09",
        help = "学期第一天，格式 YYYY-MM-DD"
    )]
    pub semester_start: String,

    #[arg(long, default_value = "Asia/Shanghai", help = "IANA 时区名称")]
    pub timezone: String,

    #[arg(long, default_value_t = 20, help = "课程开始前多少分钟提醒")]
    pub alarm_minutes_before: i64,

    #[arg(long, default_value_t = 17, help = "教学周提醒数量")]
    pub teaching_week_count: u32,

    #[arg(long, action = ArgAction::SetTrue, help = "不生成每周教学周提醒事件")]
    pub no_teaching_week_reminders: bool,

    #[arg(long, help = "输出 ICS 文件路径；默认按时间戳生成文件名")]
    pub output: Option<PathBuf>,
}

impl Config {
    pub fn output_path(&self) -> PathBuf {
        self.output.clone().unwrap_or_else(|| {
            let file_name = format!(
                "course_schedule_{}.ics",
                Local::now().format("%Y%m%d_%H%M%S")
            );
            PathBuf::from(file_name)
        })
    }

    pub fn teaching_week_reminders_enabled(&self) -> bool {
        !self.no_teaching_week_reminders
    }
}
