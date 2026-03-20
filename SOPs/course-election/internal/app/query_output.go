package app

import (
	"fmt"
	"strings"

	"course-election/internal/jwxt"
)

type lessonQueryFilter struct {
	Name              string
	LessonID          string
	Code              string
	SelectedOnly      bool
	SelectedLessonIDs map[string]bool
}

type lessonDisplayEntry struct {
	Lesson jwxt.Lesson
	Count  *jwxt.LessonCount
}

func buildLessonDisplayEntries(mapping *jwxt.LessonMappingCache, counts *jwxt.LessonCountSnapshot, filter lessonQueryFilter) []lessonDisplayEntry {
	entries := make([]lessonDisplayEntry, 0, len(mapping.Lessons))
	for _, lesson := range mapping.Lessons {
		if !matchLessonFilter(lesson, filter) {
			continue
		}

		entry := lessonDisplayEntry{Lesson: lesson}
		if counts != nil {
			if count, ok := counts.Counts[fmt.Sprintf("%d", lesson.ID)]; ok {
				c := count
				entry.Count = &c
			}
		}
		entries = append(entries, entry)
	}
	return entries
}

func matchLessonFilter(lesson jwxt.Lesson, filter lessonQueryFilter) bool {
	if filter.Name != "" && !strings.Contains(strings.ToLower(lesson.Name), strings.ToLower(filter.Name)) {
		return false
	}
	if filter.LessonID != "" && fmt.Sprintf("%d", lesson.ID) != filter.LessonID {
		return false
	}
	if filter.Code != "" && !strings.EqualFold(lesson.Code, filter.Code) {
		return false
	}
	if filter.SelectedOnly && !filter.SelectedLessonIDs[fmt.Sprintf("%d", lesson.ID)] {
		return false
	}
	return true
}

func formatLessonDisplayEntry(index int, entry lessonDisplayEntry) string {
	lesson := entry.Lesson
	var b strings.Builder

	fmt.Fprintf(&b, "[查询结果%d]\n", index)
	fmt.Fprintf(&b, "课程ID: %d\n", lesson.ID)
	fmt.Fprintf(&b, "课程号: %s\n", lesson.Code)
	fmt.Fprintf(&b, "课序号: %s\n", lesson.No)
	fmt.Fprintf(&b, "课程名称: %s\n", lesson.Name)
	fmt.Fprintf(&b, "学分: %g\n", lesson.Credits)
	fmt.Fprintf(&b, "课程类型: %s\n", lesson.CourseCategoryName)
	fmt.Fprintf(&b, "开课院系: %s\n", lesson.TeachDepartName)
	fmt.Fprintf(&b, "考试方式: %s\n", lesson.ExamModeName)
	fmt.Fprintf(&b, "授课教师: %s\n", lesson.Teachers)
	fmt.Fprintf(&b, "学时: %d\n", lesson.Period)
	fmt.Fprintf(&b, "行政班: %s\n", lesson.AdminClass)
	if entry.Count != nil {
		fmt.Fprintf(&b, "容量: %d/%d", entry.Count.Selected, entry.Count.Limit)
		if entry.Count.Reserved > 0 {
			fmt.Fprintf(&b, " (保留 %d)", entry.Count.Reserved)
		}
		b.WriteByte('\n')
	}
	for i, arrange := range lesson.ArrangeInfo {
		fmt.Fprintf(&b, "上课信息%d: %s\n", i+1, formatArrangeInfo(arrange))
	}
	b.WriteByte('\n')
	return b.String()
}

func formatArrangeInfo(arrange jwxt.ArrangeInfo) string {
	weekday := []string{"", "周一", "周二", "周三", "周四", "周五", "周六", "周日"}
	day := "未知"
	if arrange.WeekDay >= 1 && arrange.WeekDay < len(weekday) {
		day = weekday[arrange.WeekDay]
	}
	room := strings.TrimSpace(arrange.Rooms)
	if room == "" {
		room = "地点未定"
	}
	return fmt.Sprintf("%s %d-%d节 %s %s", day, arrange.StartUnit, arrange.EndUnit, formatWeekState(arrange.WeekState), room)
}

func formatWeekState(weekState string) string {
	if weekState == "" {
		return "周次未知"
	}
	segments := make([]string, 0)
	start := -1
	for week := 1; week < len(weekState); week++ {
		hasClass := weekState[week] == '1'
		if hasClass && start == -1 {
			start = week
		}
		if (!hasClass || week == len(weekState)-1) && start != -1 {
			end := week - 1
			if hasClass && week == len(weekState)-1 {
				end = week
			}
			if start == end {
				segments = append(segments, fmt.Sprintf("%d", start))
			} else {
				segments = append(segments, fmt.Sprintf("%d-%d", start, end))
			}
			start = -1
		}
	}
	if len(segments) == 0 {
		return "周次未知"
	}
	return strings.Join(segments, " ") + "周"
}
