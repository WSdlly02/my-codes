pub const DEFAULT_COURSE_TYPE_NAME: &str = "\u{00a0}";
pub const DEFAULT_COURSE_CATEGORY_NAME: &str = "一般课程";

#[allow(dead_code)]
#[derive(Debug, Clone, Default)]
pub struct Lesson {
    pub id: u32,
    pub no: String,
    pub name: String,
    pub code: String,
    pub credits: Option<f64>,
    pub start_week: u32,
    pub end_week: u32,
    pub course_type_name: String,
    pub course_category_name: String,
    pub teach_depart_name: String,
    pub exam_mode_name: String,
    pub scheduled: bool,
    pub withdrawable: bool,
    pub teachers: String,
    pub lang_type: String,
    pub remark: String,
    pub arrange_info: Vec<ArrangeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrangeInfo {
    pub week_day: u8,
    pub week_state: String,
    pub start_unit: u8,
    pub end_unit: u8,
    pub rooms: String,
}

pub fn resolve_unit_range(start_unit: u8, end_unit: u8) -> Option<(&'static str, &'static str)> {
    let (start, _) = unit_time(start_unit)?;
    let (_, end) = unit_time(end_unit)?;
    Some((start, end))
}

fn unit_time(unit: u8) -> Option<(&'static str, &'static str)> {
    match unit {
        1 => Some(("08:20", "09:05")),
        2 => Some(("09:10", "09:55")),
        3 => Some(("10:15", "11:00")),
        4 => Some(("11:05", "11:50")),
        5 => Some(("11:55", "12:25")),
        6 => Some(("12:30", "13:00")),
        7 => Some(("13:10", "13:55")),
        8 => Some(("14:00", "14:45")),
        9 => Some(("15:05", "15:50")),
        10 => Some(("15:55", "16:40")),
        11 => Some(("18:00", "18:45")),
        12 => Some(("18:50", "19:35")),
        13 => Some(("19:40", "20:25")),
        _ => None,
    }
}
