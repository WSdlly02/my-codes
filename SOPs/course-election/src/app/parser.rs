use anyhow::{Result, bail, Context};
use chrono::TimeZone;
use chrono_tz::Asia::Shanghai;
use regex::Regex;
use std::collections::{BTreeSet, HashMap};

use crate::app::support::{BASE_URL, normalize_index_key, now_fixed, split_teachers};
use crate::model::{
    ChannelEntry, Lesson, LessonCount, LessonCountSnapshot, LessonMappingCache, LessonRef,
};

pub(crate) fn parse_channels(html: &str) -> Result<Vec<ChannelEntry>> {
    let row_re = Regex::new(r"(?s)<tr>(.*?)</tr>").unwrap();
    let cell_re = Regex::new(r"(?s)<td[^>]*>(.*?)</td>").unwrap();
    let profile_re = Regex::new(r"electionProfile\.id=(\d+)").unwrap();
    let begin_re = Regex::new(r"new Date\((\d+),(\d+)-1,(\d+),(\d+),(\d+),(\d+)\)").unwrap();

    let discovered_at = now_fixed();
    let mut channels = Vec::new();
    for row_caps in row_re.captures_iter(html) {
        let row = row_caps.get(1).map(|m| m.as_str()).unwrap_or_default();
        let cells: Vec<_> = cell_re
            .captures_iter(row)
            .filter_map(|caps| caps.get(1).map(|m| m.as_str().to_string()))
            .collect();
        if cells.len() < 5 {
            continue;
        }
        let Some(profile_caps) = profile_re.captures(row) else {
            continue;
        };
        let profile_id = profile_caps.get(1).unwrap().as_str().to_string();
        let begin_at = begin_re.captures(&cells[4]).and_then(|caps| {
            let year = caps.get(1)?.as_str().parse::<i32>().ok()?;
            let month = caps.get(2)?.as_str().parse::<u32>().ok()?;
            let day = caps.get(3)?.as_str().parse::<u32>().ok()?;
            let hour = caps.get(4)?.as_str().parse::<u32>().ok()?;
            let minute = caps.get(5)?.as_str().parse::<u32>().ok()?;
            let second = caps.get(6)?.as_str().parse::<u32>().ok()?;
            Shanghai
                .with_ymd_and_hms(year, month, day, hour, minute, second)
                .single()
                .map(|dt| dt.fixed_offset())
        });

        channels.push(ChannelEntry {
            round_no: clean_html(&cells[0]),
            name: clean_html(&cells[1]),
            open_time: clean_html(&cells[2]),
            notice: clean_html(&cells[3]),
            profile_id,
            opened: !cells[4].contains("setInterval"),
            begin_at,
            discovered_at,
        });
    }

    if channels.is_empty() {
        bail!("未能从 stdElectCourse.action 解析出任何通道");
    }
    Ok(channels)
}

pub(crate) fn parse_lesson_payload(raw: &str) -> Result<Vec<Lesson>> {
    let normalized = normalize_js_literal(raw, r"^\s*var\s+lessonJSONs\s*=")?;
    serde_json::from_str::<Vec<Lesson>>(&normalized).context("解析课程数据失败")
}

pub(crate) fn parse_count_payload(raw: &str) -> Result<HashMap<String, LessonCount>> {
    let normalized = normalize_js_literal(raw, r"^\s*window\.lessonId2Counts\s*=")?;
    serde_json::from_str::<HashMap<String, LessonCount>>(&normalized).context("解析容量数据失败")
}

pub(crate) fn build_lesson_mapping_cache(profile_id: &str, lessons: Vec<Lesson>) -> LessonMappingCache {
    let mut by_lesson_id = HashMap::new();
    let mut by_name = HashMap::<String, Vec<String>>::new();
    let mut by_code = HashMap::<String, Vec<String>>::new();
    let mut by_teacher = HashMap::<String, Vec<String>>::new();

    for lesson in &lessons {
        let id = lesson.id.to_string();
        by_lesson_id.insert(
            id.clone(),
            LessonRef {
                id: lesson.id,
                no: lesson.no.clone(),
                code: lesson.code.clone(),
                name: lesson.name.clone(),
            },
        );
        append_unique(&mut by_name, normalize_index_key(&lesson.name), &id);
        append_unique(&mut by_code, normalize_index_key(&lesson.code), &id);
        for teacher in split_teachers(&lesson.teachers) {
            append_unique(&mut by_teacher, normalize_index_key(&teacher), &id);
        }
    }

    sort_index_values(&mut by_name);
    sort_index_values(&mut by_code);
    sort_index_values(&mut by_teacher);

    LessonMappingCache {
        profile_id: profile_id.to_string(),
        fetched_at: now_fixed(),
        source_url: format!("{BASE_URL}/stdElectCourse!data.action?profileId={profile_id}"),
        lessons,
        by_lesson_id,
        by_name,
        by_code,
        by_teacher,
    }
}

pub(crate) fn build_lesson_count_snapshot(
    profile_id: &str,
    counts: HashMap<String, LessonCount>,
) -> LessonCountSnapshot {
    LessonCountSnapshot {
        profile_id: profile_id.to_string(),
        fetched_at: now_fixed(),
        source_url: format!("{BASE_URL}/stdElectCourse!queryStdCount.action?profileId={profile_id}"),
        counts,
    }
}

pub(crate) fn parse_elected_ids(html: &str) -> HashMap<String, bool> {
    let re = Regex::new(r#"electedIds\["l(\d+)"\]\s*=\s*true"#).unwrap();
    let mut elected = HashMap::new();
    for caps in re.captures_iter(html) {
        if let Some(id) = caps.get(1) {
            elected.insert(id.as_str().to_string(), true);
        }
    }
    elected
}

pub(crate) fn parse_unique_std_id(html: &str) -> Result<String> {
    let re = Regex::new(
        r##"(?s)if\(jQuery\("#courseTableType"\)\.val\(\)=="std"\)\s*\{\s*bg\.form\.addInput\(form,"ids","(\d+)"\);"##,
    )
    .unwrap();
    let unique: BTreeSet<_> = re
        .captures_iter(html)
        .filter_map(|caps| caps.get(1).map(|m| m.as_str().to_string()))
        .collect();
    if unique.len() != 1 {
        bail!("无法从课表入口页提取唯一学号，匹配到 {} 个候选", unique.len());
    }
    Ok(unique.into_iter().next().unwrap())
}

pub(crate) fn summarize_selection_response(body: &str) -> String {
    let trimmed = body.trim();
    if trimmed.is_empty() {
        return "空响应".to_string();
    }
    let re = Regex::new(r#"(?s)margin:auto;">\s*(.*?)\s*</br>"#).unwrap();
    if let Some(caps) = re.captures(trimmed) {
        if let Some(matched) = caps.get(1) {
            let msg = clean_html(matched.as_str());
            if !msg.is_empty() {
                return msg;
            }
        }
    }
    clean_html(trimmed)
}

pub(crate) fn selection_succeeded(body: &str) -> bool {
    summarize_selection_response(body).contains("成功")
}

pub(crate) fn resolve_lesson_id_by_name(mapping: &LessonMappingCache, course_name: &str) -> Result<String> {
    let key = normalize_index_key(course_name);
    match mapping.by_name.get(&key) {
        None => bail!("课程映射缓存中找不到课程名: {course_name}"),
        Some(ids) if ids.len() == 1 => Ok(ids[0].clone()),
        Some(ids) => bail!(
            "课程名 {} 对应多个 lessonID，请改用 --lesson-id，候选: {}",
            course_name,
            ids.join(",")
        ),
    }
}

pub(crate) fn clean_html(input: &str) -> String {
    let tag_re = Regex::new(r"<[^>]+>").unwrap();
    let mut text = input.replace("<br/>", "\n").replace("<br>", "\n");
    text = text.replace("&nbsp;", " ");
    text = tag_re.replace_all(&text, " ").to_string();
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn normalize_js_literal(raw: &str, prefix_pattern: &str) -> Result<String> {
    let trimmed = raw.trim();
    let lower = trimmed.to_ascii_lowercase();
    if lower.starts_with("<!doctype html") || lower.starts_with("<html") {
        bail!("接口返回了 HTML 页面而不是课程数据，通常表示当前 profile 尚未建立上下文、未开放，或服务端返回了错误页");
    }

    let comment_re = Regex::new(r"(?s)/\*.*?\*/").unwrap();
    let prefix_re = Regex::new(prefix_pattern).unwrap();
    let bare_key_re = Regex::new(r#"([\{\[,]\s*)([A-Za-z_][A-Za-z0-9_]*)(\s*:)"#).unwrap();

    let mut text = comment_re.replace_all(trimmed, "").to_string();
    text = prefix_re.replace(&text, "").to_string();
    text = text.trim().trim_end_matches(';').trim().to_string();
    if text.is_empty() {
        bail!("空响应体");
    }

    let mut quoted = js_single_quote_to_json(&text)?;
    loop {
        let next = bare_key_re.replace_all(&quoted, r#"$1"$2"$3"#).to_string();
        if next == quoted {
            break;
        }
        quoted = next;
    }
    Ok(quoted)
}

fn js_single_quote_to_json(input: &str) -> Result<String> {
    let mut out = String::with_capacity(input.len());
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;

    for ch in input.chars() {
        if in_single {
            if escape {
                match ch {
                    '\\' | '\'' => out.push(ch),
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    other => {
                        out.push('\\');
                        out.push(other);
                    }
                }
                escape = false;
                continue;
            }
            match ch {
                '\\' => escape = true,
                '\'' => {
                    in_single = false;
                    out.push('"');
                }
                '"' => out.push_str("\\\""),
                _ => out.push(ch),
            }
            continue;
        }

        if in_double {
            out.push(ch);
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '\\' => escape = true,
                '"' => in_double = false,
                _ => {}
            }
            continue;
        }

        match ch {
            '\'' => {
                in_single = true;
                out.push('"');
            }
            '"' => {
                in_double = true;
                out.push('"');
            }
            _ => out.push(ch),
        }
    }

    if in_single {
        bail!("单引号字符串未闭合");
    }
    Ok(out)
}

fn append_unique(index: &mut HashMap<String, Vec<String>>, key: String, lesson_id: &str) {
    if key.is_empty() {
        return;
    }
    let values = index.entry(key).or_default();
    if !values.iter().any(|id| id == lesson_id) {
        values.push(lesson_id.to_string());
    }
}

fn sort_index_values(index: &mut HashMap<String, Vec<String>>) {
    for values in index.values_mut() {
        values.sort();
    }
}

#[cfg(test)]
mod tests {
    use super::{
        build_lesson_mapping_cache, parse_count_payload, parse_lesson_payload,
        resolve_lesson_id_by_name, summarize_selection_response,
    };
    use crate::model::{Lesson, LessonMappingCache};
    use std::collections::HashMap;

    #[test]
    fn single_quote_js_is_normalized() {
        let raw = "var lessonJSONs = [{id: 1, name: '刑法学', code: 'FX110010'}];";
        let lessons = parse_lesson_payload(raw).expect("parse lessons");
        assert_eq!(lessons.len(), 1);
        assert_eq!(lessons[0].name, "刑法学");
        assert_eq!(lessons[0].code, "FX110010");
    }

    #[test]
    fn count_payload_is_normalized() {
        let raw = "window.lessonId2Counts = {'244758': {sc: 12, lc: 60, wc: 1}};";
        let counts = parse_count_payload(raw).expect("parse counts");
        assert_eq!(counts["244758"].limit, 60);
        assert_eq!(counts["244758"].selected, 12);
    }

    #[test]
    fn mapping_cache_builds_indexes() {
        let cache = build_lesson_mapping_cache(
            "2936",
            vec![Lesson {
                id: 244433,
                name: "刑法学".to_string(),
                code: "FX110010".to_string(),
                teachers: "张宜培".to_string(),
                ..Lesson::default()
            }],
        );
        assert_eq!(cache.profile_id, "2936");
        assert_eq!(cache.by_name["刑法学"], vec!["244433"]);
        assert_eq!(cache.by_code["fx110010"], vec!["244433"]);
        assert_eq!(cache.by_teacher["张宜培"], vec!["244433"]);
    }

    #[test]
    fn resolve_lesson_id_by_name_works() {
        let cache = LessonMappingCache {
            by_name: HashMap::from([("刑法学".to_string(), vec!["244433".to_string()])]),
            ..LessonMappingCache::default()
        };
        let lesson_id = resolve_lesson_id_by_name(&cache, "刑法学").expect("resolve lesson id");
        assert_eq!(lesson_id, "244433");
    }

    #[test]
    fn summarize_selection_response_extracts_message() {
        let body = r#"<html><body><div style="margin:auto;"> 选课成功 </br></div></body></html>"#;
        assert_eq!(summarize_selection_response(body), "选课成功");
    }
}
