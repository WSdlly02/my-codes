use anyhow::{Context, Result, bail};
use chrono::{Duration, NaiveDate, NaiveTime, TimeZone, Utc};
use chrono_tz::Tz;
use icalendar::{Alarm, Calendar, CalendarDateTime, Component, EventLike};
use uuid::Uuid;

use crate::config::Config;
use crate::model::{ArrangeInfo, Lesson, resolve_unit_range};

pub fn generate_ics(lessons: &[Lesson], config: &Config) -> Result<String> {
    if lessons.is_empty() {
        bail!("课程信息为空");
    }

    let timezone: Tz = config
        .timezone
        .parse()
        .with_context(|| format!("无法解析时区 {}", config.timezone))?;
    let semester_start = NaiveDate::parse_from_str(&config.semester_start, "%Y-%m-%d")
        .with_context(|| format!("无法解析学期开始日期 {}", config.semester_start))?;

    let mut calendar = Calendar::new();
    calendar.name("课程表");
    calendar.timezone(&config.timezone);

    let dt_stamp = Utc::now();
    let mut event_count = 0usize;

    if config.teaching_week_reminders_enabled() {
        event_count += append_teaching_week_reminders(
            &mut calendar,
            semester_start,
            timezone,
            dt_stamp,
            config.teaching_week_count,
        )?;
    }

    for lesson in lessons {
        if lesson.arrange_info.is_empty() {
            continue;
        }

        for arrange in &lesson.arrange_info {
            let Some((start_clock, end_clock)) =
                resolve_unit_range(arrange.start_unit, arrange.end_unit)
            else {
                continue;
            };

            for (week_index, flag) in arrange.week_state.chars().enumerate().skip(1) {
                if flag != '1' {
                    continue;
                }

                let class_date = semester_start
                    + Duration::days(
                        ((week_index - 1) * 7 + usize::from(arrange.week_day - 1)) as i64,
                    );
                let start_at = combine_date_and_clock(class_date, start_clock, timezone)?;
                let end_at = combine_date_and_clock(class_date, end_clock, timezone)?;

                let mut event = icalendar::Event::new();
                event
                    .uid(&build_event_uid(lesson, arrange, week_index as u32))
                    .summary(lesson.name.trim())
                    .starts(CalendarDateTime::WithTimezone {
                        date_time: start_at.naive_local(),
                        tzid: config.timezone.clone(),
                    })
                    .ends(CalendarDateTime::WithTimezone {
                        date_time: end_at.naive_local(),
                        tzid: config.timezone.clone(),
                    })
                    .timestamp(dt_stamp);

                if !arrange.rooms.trim().is_empty() {
                    event.location(arrange.rooms.trim());
                }
                if !lesson.teachers.trim().is_empty() {
                    event.description(&format!("教师: {}", lesson.teachers.trim()));
                }

                event.alarm(Alarm::display(
                    lesson.name.trim(),
                    Duration::minutes(-config.alarm_minutes_before),
                ));
                calendar.push(event.done());
                event_count += 1;
            }
        }
    }

    if event_count == 0 {
        bail!("未生成任何课程事件");
    }

    Ok(calendar.to_string())
}

fn append_teaching_week_reminders(
    calendar: &mut Calendar,
    semester_start: NaiveDate,
    timezone: Tz,
    dt_stamp: chrono::DateTime<Utc>,
    teaching_week_count: u32,
) -> Result<usize> {
    let mut count = 0usize;

    for week in 1..=teaching_week_count {
        let reminder_date = semester_start + Duration::days(i64::from((week - 1) * 7));
        let start_at = combine_date_and_clock(reminder_date, "08:00", timezone)?;
        let end_at = start_at + Duration::minutes(5);
        let summary = format!("第{week}教学周");
        let description = format!("这是第{week}教学周");

        let mut event = icalendar::Event::new();
        event
            .uid(&build_teaching_week_uid(&semester_start.to_string(), week))
            .summary(&summary)
            .description(&description)
            .starts(CalendarDateTime::WithTimezone {
                date_time: start_at.naive_local(),
                tzid: timezone.to_string(),
            })
            .ends(CalendarDateTime::WithTimezone {
                date_time: end_at.naive_local(),
                tzid: timezone.to_string(),
            })
            .timestamp(dt_stamp);
        event.alarm(Alarm::display(&description, Duration::zero()));
        calendar.push(event.done());
        count += 1;
    }

    Ok(count)
}

fn combine_date_and_clock(
    date: NaiveDate,
    clock: &str,
    timezone: Tz,
) -> Result<chrono::DateTime<Tz>> {
    let time = NaiveTime::parse_from_str(clock, "%H:%M")
        .with_context(|| format!("无法解析上课时间 {clock}"))?;
    let naive = date.and_time(time);
    timezone
        .from_local_datetime(&naive)
        .single()
        .with_context(|| format!("无法在时区 {timezone} 下构造时间 {naive}"))
}

fn build_event_uid(lesson: &Lesson, arrange: &ArrangeInfo, week: u32) -> String {
    let raw = format!(
        "{}|{}|{}|{}|{}|{}",
        lesson.no, lesson.name, week, arrange.week_day, arrange.start_unit, arrange.end_unit
    );
    build_stable_uid(&raw)
}

fn build_teaching_week_uid(semester_start: &str, week: u32) -> String {
    let raw = format!("teaching-week|{semester_start}|{week}");
    build_stable_uid(&raw)
}

fn build_stable_uid(value: &str) -> String {
    // 使用 UUID v5 为同一份业务数据生成稳定 UID，这样重复导入或覆盖导出时，
    // 日历客户端能把它识别为同一个事件，而不是新增出重复副本。
    let uuid = Uuid::new_v5(&Uuid::NAMESPACE_URL, value.as_bytes());
    format!("{uuid}@course-schedule-conversion")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{ArrangeInfo, Lesson};

    #[test]
    fn generate_calendar_contains_course_and_week_reminders() {
        let config = Config {
            course_election_bin: "/tmp/unused".into(),
            html_file: None,
            semester_start: "2026-03-09".to_owned(),
            timezone: "Asia/Shanghai".to_owned(),
            alarm_minutes_before: 20,
            teaching_week_count: 2,
            no_teaching_week_reminders: false,
            output: None,
        };

        let lessons = vec![Lesson {
            no: "CS123456_001".to_owned(),
            name: "编译原理".to_owned(),
            teachers: "张三".to_owned(),
            arrange_info: vec![ArrangeInfo {
                week_day: 1,
                week_state: "01100000000000000000000000000000000000000000000000000".to_owned(),
                start_unit: 1,
                end_unit: 2,
                rooms: "一教101".to_owned(),
            }],
            ..Lesson::default()
        }];

        let ics = generate_ics(&lessons, &config).unwrap();
        assert!(ics.contains("SUMMARY:编译原理"));
        assert!(ics.contains("SUMMARY:第1教学周"));
        assert!(ics.contains("SUMMARY:第2教学周"));
        assert!(ics.contains("LOCATION:一教101"));
    }
}
