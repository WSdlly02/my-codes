use crate::model::{ArrangeInfo, Lesson, LessonCount, LessonCountSnapshot, LessonMappingCache};
use std::collections::HashMap;

pub(crate) struct LessonDisplayEntry {
    pub(crate) lesson: Lesson,
    pub(crate) count: Option<LessonCount>,
}

#[derive(Default)]
pub(crate) struct LessonQueryFilter {
    pub(crate) name: Option<String>,
    pub(crate) lesson_id: Option<String>,
    pub(crate) code: Option<String>,
    pub(crate) selected_only: bool,
    pub(crate) selected_lesson_ids: HashMap<String, bool>,
}

pub(crate) fn build_lesson_display_entries(
    mapping: &LessonMappingCache,
    counts: Option<&LessonCountSnapshot>,
    filter: &LessonQueryFilter,
) -> Vec<LessonDisplayEntry> {
    let mut entries = Vec::new();
    for lesson in &mapping.lessons {
        if !match_lesson_filter(lesson, filter) {
            continue;
        }
        let count = counts.and_then(|snapshot| snapshot.counts.get(&lesson.id.to_string()).cloned());
        entries.push(LessonDisplayEntry {
            lesson: lesson.clone(),
            count,
        });
    }
    entries
}

pub(crate) fn format_lesson_display_entry(index: usize, entry: &LessonDisplayEntry) -> String {
    let lesson = &entry.lesson;
    let mut out = String::new();
    out.push_str(&format!("[查询结果{index}]\n"));
    out.push_str(&format!("课程ID: {}\n", lesson.id));
    out.push_str(&format!("课程号: {}\n", lesson.code));
    out.push_str(&format!("课序号: {}\n", lesson.no));
    out.push_str(&format!("课程名称: {}\n", lesson.name));
    out.push_str(&format!("学分: {}\n", lesson.credits));
    out.push_str(&format!("课程类型: {}\n", lesson.course_category_name));
    out.push_str(&format!("开课院系: {}\n", lesson.teach_depart_name));
    out.push_str(&format!("考试方式: {}\n", lesson.exam_mode_name));
    out.push_str(&format!("授课教师: {}\n", lesson.teachers));
    out.push_str(&format!("学时: {}\n", lesson.period));
    out.push_str(&format!("行政班: {}\n", lesson.admin_class));
    if let Some(count) = entry.count.as_ref() {
        out.push_str(&format!("容量: {}/{}", count.selected, count.limit));
        if count.reserved > 0 {
            out.push_str(&format!(" (保留 {})", count.reserved));
        }
        out.push('\n');
    }
    for (idx, arrange) in lesson.arrange_info.iter().enumerate() {
        out.push_str(&format!(
            "上课信息{}: {}\n",
            idx + 1,
            format_arrange_info(arrange)
        ));
    }
    out.push('\n');
    out
}

pub(crate) fn format_week_state(week_state: &str) -> String {
    if week_state.is_empty() {
        return "周次未知".to_string();
    }
    let bytes = week_state.as_bytes();
    let mut segments = Vec::new();
    let mut start = None;
    for week in 1..bytes.len() {
        let has_class = bytes[week] == b'1';
        if has_class && start.is_none() {
            start = Some(week);
        }
        let is_last = week == bytes.len() - 1;
        if (!has_class || is_last) && start.is_some() {
            let s = start.expect("checked");
            let end = if has_class && is_last { week } else { week - 1 };
            if s == end {
                segments.push(format!("{s}"));
            } else {
                segments.push(format!("{s}-{end}"));
            }
            start = None;
        }
    }
    if segments.is_empty() {
        "周次未知".to_string()
    } else {
        format!("{}周", segments.join(" "))
    }
}

fn match_lesson_filter(lesson: &Lesson, filter: &LessonQueryFilter) -> bool {
    if let Some(name) = filter.name.as_ref() {
        if !lesson.name.to_lowercase().contains(&name.to_lowercase()) {
            return false;
        }
    }
    if let Some(lesson_id) = filter.lesson_id.as_ref() {
        if lesson.id.to_string() != *lesson_id {
            return false;
        }
    }
    if let Some(code) = filter.code.as_ref() {
        if !lesson.code.eq_ignore_ascii_case(code) {
            return false;
        }
    }
    if filter.selected_only
        && !filter
            .selected_lesson_ids
            .contains_key(&lesson.id.to_string())
    {
        return false;
    }
    true
}

fn format_arrange_info(arrange: &ArrangeInfo) -> String {
    let day = match arrange.week_day {
        1 => "周一",
        2 => "周二",
        3 => "周三",
        4 => "周四",
        5 => "周五",
        6 => "周六",
        7 => "周日",
        _ => "未知",
    };
    let room = if arrange.rooms.trim().is_empty() {
        "地点未定"
    } else {
        arrange.rooms.trim()
    };
    format!(
        "{day} {}-{}节 {} {room}",
        arrange.start_unit,
        arrange.end_unit,
        format_week_state(&arrange.week_state)
    )
}

#[cfg(test)]
mod tests {
    use super::format_week_state;

    #[test]
    fn format_week_state_compacts_ranges() {
        assert_eq!(format_week_state("01110011"), "1-3 6-7周");
    }
}
