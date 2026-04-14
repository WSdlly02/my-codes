use std::collections::{BTreeMap, HashMap};

use anyhow::{Context, Result, bail};
use once_cell::sync::Lazy;
use regex::Regex;
use scraper::{Html, Selector};

use crate::model::{ArrangeInfo, DEFAULT_COURSE_CATEGORY_NAME, DEFAULT_COURSE_TYPE_NAME, Lesson};

static LESSON_ID_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"lesson\.id=(\d+)").unwrap());
static LESSON_NO_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[A-Z]{2}\d{6}_[0-9]{3}").unwrap());
static COURSE_CODE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[A-Z]{2}\d{6}").unwrap());
static ACTIVITY_DECL_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r#"activity = new TaskActivity\("([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([01]{53})"\);"#,
    )
    .unwrap()
});
static ACTIVITY_INDEX_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"index =(\d+)\*unitCount\+(\d+);").unwrap());
static COURSE_NAME_TAIL_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^(.*)\(([A-Z]{2}\d{6}_[0-9]{3})\)$").unwrap());

static ROW_SELECTOR: Lazy<Selector> =
    Lazy::new(|| Selector::parse("tbody[id$='_data'] tr").unwrap());
static CELL_SELECTOR: Lazy<Selector> = Lazy::new(|| Selector::parse("td").unwrap());
static LINK_SELECTOR: Lazy<Selector> = Lazy::new(|| Selector::parse("a").unwrap());

#[derive(Debug, Clone)]
struct ActivityBlock {
    lesson_no: String,
    name: String,
    teacher: String,
    room: String,
    week_state: String,
    slots: Vec<Slot>,
}

#[derive(Debug, Clone, Copy)]
struct Slot {
    weekday: u8,
    unit: u8,
}

pub fn parse_class_schedule_html(html: &str) -> Result<Vec<Lesson>> {
    let document = Html::parse_document(html);
    let (mut lessons_by_no, order) = parse_class_schedule_rows(&document);
    if lessons_by_no.is_empty() {
        bail!("未在课程表 HTML 中解析到任何课程");
    }

    merge_class_schedule_activities(html, &mut lessons_by_no).context("合并课表活动信息失败")?;

    let mut lessons = Vec::with_capacity(order.len());
    for lesson_no in order {
        if let Some(mut lesson) = lessons_by_no.remove(&lesson_no) {
            if lesson.start_week == 0 || lesson.end_week == 0 {
                let (start_week, end_week) = week_bounds_from_arranges(&lesson.arrange_info);
                lesson.start_week = start_week;
                lesson.end_week = end_week;
            }
            lesson.scheduled = !lesson.arrange_info.is_empty();
            lessons.push(lesson);
        }
    }

    Ok(lessons)
}

fn parse_class_schedule_rows(document: &Html) -> (HashMap<String, Lesson>, Vec<String>) {
    let mut lessons_by_no = HashMap::new();
    let mut order = Vec::new();

    for row in document.select(&ROW_SELECTOR) {
        let cells = row.select(&CELL_SELECTOR).collect::<Vec<_>>();
        if cells.len() < 11 {
            continue;
        }

        let get_text = |index: usize| -> String {
            cells
                .get(index)
                .map(|cell| cell_text(cell))
                .unwrap_or_default()
        };

        let lesson_no = get_text(1);
        if lesson_no.is_empty() {
            continue;
        }

        let id = cells
            .get(1)
            .and_then(|cell| cell.select(&LINK_SELECTOR).next())
            .and_then(|anchor| anchor.value().attr("href"))
            .and_then(|href| {
                LESSON_ID_RE
                    .captures(href)
                    .and_then(|captures| captures.get(1))
                    .and_then(|matched| matched.as_str().parse::<u32>().ok())
            })
            .unwrap_or_default();

        let code = COURSE_CODE_RE
            .find(&get_text(2))
            .map(|matched| matched.as_str().to_owned())
            .unwrap_or_default();

        let lesson = Lesson {
            id,
            no: lesson_no.clone(),
            code,
            name: get_text(3),
            credits: get_text(4).parse::<f64>().ok(),
            teachers: get_text(5),
            teach_depart_name: get_text(6),
            exam_mode_name: get_text(7),
            lang_type: get_text(9),
            remark: get_text(10),
            course_type_name: DEFAULT_COURSE_TYPE_NAME.to_owned(),
            course_category_name: DEFAULT_COURSE_CATEGORY_NAME.to_owned(),
            withdrawable: true,
            ..Lesson::default()
        };

        lessons_by_no.insert(lesson_no.clone(), lesson);
        order.push(lesson_no);
    }

    (lessons_by_no, order)
}

fn merge_class_schedule_activities(
    html: &str,
    lessons_by_no: &mut HashMap<String, Lesson>,
) -> Result<()> {
    let mut current: Option<ActivityBlock> = None;

    for raw_line in html.lines() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        if let Some(captures) = ACTIVITY_DECL_RE.captures(line) {
            flush_current(current.take(), lessons_by_no);
            current = Some(ActivityBlock {
                lesson_no: first_non_empty(&[
                    extract_lesson_no(captures.get(3).map(|m| m.as_str()).unwrap_or_default()),
                    extract_lesson_no(captures.get(4).map(|m| m.as_str()).unwrap_or_default()),
                ]),
                name: extract_course_name(captures.get(4).map(|m| m.as_str()).unwrap_or_default()),
                teacher: captures
                    .get(2)
                    .map(|m| m.as_str().trim().to_owned())
                    .unwrap_or_default(),
                room: captures
                    .get(6)
                    .map(|m| m.as_str().trim().to_owned())
                    .unwrap_or_default(),
                week_state: captures
                    .get(7)
                    .map(|m| m.as_str().to_owned())
                    .unwrap_or_default(),
                slots: Vec::new(),
            });
            continue;
        }

        if let (Some(block), Some(captures)) = (&mut current, ACTIVITY_INDEX_RE.captures(line)) {
            let weekday_base = captures
                .get(1)
                .and_then(|m| m.as_str().parse::<u8>().ok())
                .unwrap_or(0);
            let unit_base = captures
                .get(2)
                .and_then(|m| m.as_str().parse::<u8>().ok())
                .unwrap_or(0);
            block.slots.push(Slot {
                weekday: weekday_base + 1,
                unit: unit_base + 1,
            });
        }
    }

    flush_current(current, lessons_by_no);
    Ok(())
}

fn flush_current(current: Option<ActivityBlock>, lessons_by_no: &mut HashMap<String, Lesson>) {
    let Some(current) = current else {
        return;
    };
    if current.lesson_no.is_empty() || current.slots.is_empty() {
        return;
    }

    let lesson = lessons_by_no
        .entry(current.lesson_no.clone())
        .or_insert_with(|| Lesson {
            no: current.lesson_no.clone(),
            name: current.name.clone(),
            teachers: current.teacher.clone(),
            course_type_name: DEFAULT_COURSE_TYPE_NAME.to_owned(),
            course_category_name: DEFAULT_COURSE_CATEGORY_NAME.to_owned(),
            withdrawable: true,
            ..Lesson::default()
        });

    if lesson.name.trim().is_empty() {
        lesson.name = current.name.clone();
    }
    if lesson.teachers.trim().is_empty() {
        lesson.teachers = current.teacher.clone();
    }

    lesson.arrange_info.extend(build_arrange_infos(
        &current.slots,
        &current.week_state,
        &current.room,
    ));
}

fn build_arrange_infos(slots: &[Slot], week_state: &str, room: &str) -> Vec<ArrangeInfo> {
    let mut by_weekday: BTreeMap<u8, Vec<u8>> = BTreeMap::new();
    for slot in slots {
        by_weekday.entry(slot.weekday).or_default().push(slot.unit);
    }

    let mut arranges = Vec::new();
    for (weekday, mut units) in by_weekday {
        units.sort_unstable();
        let mut start = units[0];
        let mut prev = units[0];

        for &unit in units.iter().skip(1) {
            if unit == prev + 1 {
                prev = unit;
                continue;
            }

            arranges.push(ArrangeInfo {
                week_day: weekday,
                week_state: week_state.to_owned(),
                start_unit: start,
                end_unit: prev,
                rooms: room.to_owned(),
            });
            start = unit;
            prev = unit;
        }

        arranges.push(ArrangeInfo {
            week_day: weekday,
            week_state: week_state.to_owned(),
            start_unit: start,
            end_unit: prev,
            rooms: room.to_owned(),
        });
    }

    arranges.sort_by(|left, right| {
        left.week_day
            .cmp(&right.week_day)
            .then(left.start_unit.cmp(&right.start_unit))
            .then(left.end_unit.cmp(&right.end_unit))
            .then(left.week_state.cmp(&right.week_state))
    });

    arranges
}

fn week_bounds_from_arranges(arranges: &[ArrangeInfo]) -> (u32, u32) {
    let mut start_week = 0;
    let mut end_week = 0;

    for arrange in arranges {
        for (week, flag) in arrange.week_state.chars().enumerate().skip(1) {
            if flag != '1' {
                continue;
            }
            if start_week == 0 {
                start_week = week as u32;
            }
            end_week = week as u32;
        }
    }

    (start_week, end_week)
}

fn extract_lesson_no(value: &str) -> String {
    LESSON_NO_RE
        .find(value)
        .map(|matched| matched.as_str().to_owned())
        .unwrap_or_default()
}

fn extract_course_name(value: &str) -> String {
    let trimmed = value.trim();
    if let Some(captures) = COURSE_NAME_TAIL_RE.captures(trimmed) {
        return captures
            .get(1)
            .map(|m| m.as_str().trim().to_owned())
            .unwrap_or_else(|| trimmed.to_owned());
    }
    trimmed.to_owned()
}

fn first_non_empty(values: &[String]) -> String {
    values
        .iter()
        .find(|value| !value.trim().is_empty())
        .cloned()
        .unwrap_or_default()
}

fn cell_text(cell: &scraper::ElementRef<'_>) -> String {
    cell.text().collect::<Vec<_>>().join("").trim().to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_schedule_rows_and_activities() {
        let html = r#"
        <table>
          <tbody id="demo_data">
            <tr>
              <td>1</td>
              <td><a href="lesson.id=123">CS123456_001</a></td>
              <td>CS123456</td>
              <td>编译原理</td>
              <td>3.0</td>
              <td>张三</td>
              <td>计算机学院</td>
              <td>考试</td>
              <td></td>
              <td>中文</td>
              <td>备注</td>
            </tr>
          </tbody>
        </table>
        <script>
          activity = new TaskActivity("","张三","CS123456_001","编译原理(CS123456_001)","","一教101","01100000000000000000000000000000000000000000000000000");
          index =0*unitCount+0;
          index =0*unitCount+1;
        </script>
        "#;

        let lessons = parse_class_schedule_html(html).unwrap();
        assert_eq!(lessons.len(), 1);
        assert_eq!(lessons[0].name, "编译原理");
        assert_eq!(lessons[0].arrange_info.len(), 1);
        assert_eq!(
            lessons[0].arrange_info[0],
            ArrangeInfo {
                week_day: 1,
                week_state: "01100000000000000000000000000000000000000000000000000".to_owned(),
                start_unit: 1,
                end_unit: 2,
                rooms: "一教101".to_owned(),
            }
        );
        assert_eq!(lessons[0].start_week, 1);
        assert_eq!(lessons[0].end_week, 2);
    }
}
