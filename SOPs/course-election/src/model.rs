use chrono::{DateTime, FixedOffset};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SavedCookies {
    #[serde(default)]
    pub cookies: Vec<SavedCookie>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SavedCookie {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub value: String,
    #[serde(default)]
    pub domain: String,
    #[serde(default)]
    pub path: String,
    #[serde(default)]
    pub expires: Option<DateTime<FixedOffset>>,
    #[serde(default, rename = "httpOnly")]
    pub http_only: bool,
    #[serde(default)]
    pub secure: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ChannelEntry {
    #[serde(default, rename = "roundNo")]
    pub round_no: String,
    #[serde(default)]
    pub name: String,
    #[serde(default, rename = "openTime")]
    pub open_time: String,
    #[serde(default)]
    pub notice: String,
    #[serde(default, rename = "profileId")]
    pub profile_id: String,
    #[serde(default)]
    pub opened: bool,
    #[serde(default, rename = "beginAt")]
    pub begin_at: Option<DateTime<FixedOffset>>,
    #[serde(default, rename = "discoveredAt")]
    pub discovered_at: DateTime<FixedOffset>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ChannelCache {
    #[serde(default, rename = "fetchedAt")]
    pub fetched_at: DateTime<FixedOffset>,
    #[serde(default, rename = "sourceUrl")]
    pub source_url: String,
    #[serde(default)]
    pub channels: Vec<ChannelEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Lesson {
    #[serde(default)]
    pub id: i64,
    #[serde(default)]
    pub no: String,
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub code: String,
    #[serde(default)]
    pub credits: f64,
    #[serde(default, rename = "courseId")]
    pub course_id: i64,
    #[serde(default, rename = "startWeek")]
    pub start_week: i64,
    #[serde(default, rename = "endWeek")]
    pub end_week: i64,
    #[serde(default, rename = "courseTypeId")]
    pub course_type_id: i64,
    #[serde(default, rename = "courseTypeName")]
    pub course_type_name: String,
    #[serde(default, rename = "courseTypeCode")]
    pub course_type_code: String,
    #[serde(default, rename = "courseCategoryName")]
    pub course_category_name: String,
    #[serde(default, rename = "teachDepartName")]
    pub teach_depart_name: String,
    #[serde(default, rename = "examModeName")]
    pub exam_mode_name: String,
    #[serde(default)]
    pub scheduled: bool,
    #[serde(default, rename = "hasTextBook")]
    pub has_text_book: bool,
    #[serde(default)]
    pub period: i64,
    #[serde(default, rename = "weekHour")]
    pub week_hour: i64,
    #[serde(default)]
    pub withdrawable: bool,
    #[serde(default)]
    pub textbooks: String,
    #[serde(default)]
    pub teachers: String,
    #[serde(default, rename = "campusCode")]
    pub campus_code: String,
    #[serde(default, rename = "langType")]
    pub lang_type: String,
    #[serde(default, rename = "campusName")]
    pub campus_name: String,
    #[serde(default, rename = "adminClass")]
    pub admin_class: String,
    #[serde(default)]
    pub remark: String,
    #[serde(default, rename = "arrangeInfo")]
    pub arrange_info: Vec<ArrangeInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ArrangeInfo {
    #[serde(default, rename = "weekDay")]
    pub week_day: i64,
    #[serde(default, rename = "weekState")]
    pub week_state: String,
    #[serde(default, rename = "startUnit")]
    pub start_unit: i64,
    #[serde(default, rename = "endUnit")]
    pub end_unit: i64,
    #[serde(default)]
    pub rooms: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LessonCount {
    #[serde(default, rename = "sc")]
    pub selected: i64,
    #[serde(default, rename = "lc")]
    pub limit: i64,
    #[serde(default, rename = "wc")]
    pub reserved: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LessonRef {
    #[serde(default)]
    pub id: i64,
    #[serde(default)]
    pub no: String,
    #[serde(default)]
    pub code: String,
    #[serde(default)]
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LessonMappingCache {
    #[serde(default, rename = "profileId")]
    pub profile_id: String,
    #[serde(default, rename = "fetchedAt")]
    pub fetched_at: DateTime<FixedOffset>,
    #[serde(default, rename = "sourceUrl")]
    pub source_url: String,
    #[serde(default)]
    pub lessons: Vec<Lesson>,
    #[serde(default, rename = "byLessonId")]
    pub by_lesson_id: HashMap<String, LessonRef>,
    #[serde(default, rename = "byName")]
    pub by_name: HashMap<String, Vec<String>>,
    #[serde(default, rename = "byCode")]
    pub by_code: HashMap<String, Vec<String>>,
    #[serde(default, rename = "byTeacher")]
    pub by_teacher: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LessonCountSnapshot {
    #[serde(default, rename = "profileId")]
    pub profile_id: String,
    #[serde(default, rename = "fetchedAt")]
    pub fetched_at: DateTime<FixedOffset>,
    #[serde(default, rename = "sourceUrl")]
    pub source_url: String,
    #[serde(default)]
    pub counts: HashMap<String, LessonCount>,
}
