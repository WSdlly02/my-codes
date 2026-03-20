package main

import (
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

func generateICSFromCourseInfo(courseInfo json.RawMessage, firstDayOfSemester, timezone string) (string, error) {
	var lessons []Lesson
	if err := json.Unmarshal(courseInfo, &lessons); err != nil {
		return "", fmt.Errorf("解析课程信息 JSON 失败: %w", err)
	}
	if len(lessons) == 0 {
		return "", fmt.Errorf("课程信息为空")
	}

	loc, err := time.LoadLocation(timezone)
	if err != nil {
		return "", fmt.Errorf("加载时区失败: %w", err)
	}
	semesterStart, err := time.ParseInLocation("2006-01-02", firstDayOfSemester, loc)
	if err != nil {
		return "", fmt.Errorf("解析学期开始日期失败: %w", err)
	}

	var b strings.Builder
	writeICSLine(&b, "BEGIN:VCALENDAR")
	writeICSLine(&b, "VERSION:2.0")
	writeICSLine(&b, "PRODID:-//course-schedule-conversion//EN")
	writeICSLine(&b, "CALSCALE:GREGORIAN")
	writeICSLine(&b, "METHOD:PUBLISH")
	writeICSLine(&b, "X-WR-CALNAME:课程表")
	writeICSLine(&b, "X-WR-TIMEZONE:"+timezone)

	dtStamp := time.Now().UTC().Format("20060102T150405Z")
	eventCount := 0
	for _, lesson := range lessons {
		if len(lesson.ArrangeInfo) == 0 {
			continue
		}
		for _, arrange := range lesson.ArrangeInfo {
			startClock, endClock, ok := resolveUnitRange(arrange.StartUnit, arrange.EndUnit)
			if !ok {
				continue
			}

			for week := 1; week < len(arrange.WeekState); week++ {
				if arrange.WeekState[week] != '1' {
					continue
				}

				classDate := semesterStart.AddDate(0, 0, (week-1)*7+(arrange.WeekDay-1))
				startAt, err := combineDateAndClock(classDate, startClock, loc)
				if err != nil {
					return "", err
				}
				endAt, err := combineDateAndClock(classDate, endClock, loc)
				if err != nil {
					return "", err
				}

				writeICSLine(&b, "BEGIN:VEVENT")
				writeICSLine(&b, "UID:"+buildEventUID(lesson, arrange, week))
				writeICSLine(&b, "DTSTAMP:"+dtStamp)
				writeICSLine(&b, "DTSTART;TZID="+timezone+":"+startAt.Format("20060102T150405"))
				writeICSLine(&b, "DTEND;TZID="+timezone+":"+endAt.Format("20060102T150405"))
				writeICSLine(&b, "SUMMARY:"+escapeICSText(strings.TrimSpace(lesson.Name)))
				if room := strings.TrimSpace(arrange.Rooms); room != "" {
					writeICSLine(&b, "LOCATION:"+escapeICSText(room))
				}
				if teachers := strings.TrimSpace(lesson.Teachers); teachers != "" {
					writeICSLine(&b, "DESCRIPTION:"+escapeICSText("教师: "+teachers))
				}
				writeICSLine(&b, "BEGIN:VALARM")
				writeICSLine(&b, "ACTION:DISPLAY")
				writeICSLine(&b, "DESCRIPTION:"+escapeICSText(strings.TrimSpace(lesson.Name)))
				writeICSLine(&b, fmt.Sprintf("TRIGGER:-PT%dM", AlarmMinutesBefore))
				writeICSLine(&b, "END:VALARM")
				writeICSLine(&b, "END:VEVENT")
				eventCount++
			}
		}
	}

	if eventCount == 0 {
		return "", fmt.Errorf("未生成任何课程事件")
	}

	writeICSLine(&b, "END:VCALENDAR")
	return b.String(), nil
}

func resolveUnitRange(startUnit, endUnit int) (string, string, bool) {
	start, ok := UnitToTimes[startUnit]
	if !ok {
		return "", "", false
	}
	end, ok := UnitToTimes[endUnit]
	if !ok {
		return "", "", false
	}
	return start[0], end[1], true
}

func combineDateAndClock(date time.Time, clock string, loc *time.Location) (time.Time, error) {
	datetime := date.Format("2006-01-02") + " " + clock
	value, err := time.ParseInLocation("2006-01-02 15:04", datetime, loc)
	if err != nil {
		return time.Time{}, fmt.Errorf("解析上课时间失败: %w", err)
	}
	return value, nil
}

func buildEventUID(lesson Lesson, arrange ArrangeInfo, week int) string {
	raw := fmt.Sprintf("%s|%s|%d|%d|%d|%d", lesson.No, lesson.Name, week, arrange.WeekDay, arrange.StartUnit, arrange.EndUnit)
	sum := sha1.Sum([]byte(raw))
	return hex.EncodeToString(sum[:]) + "@course-schedule-conversion"
}

func writeICSLine(b *strings.Builder, line string) {
	b.WriteString(line)
	b.WriteString("\r\n")
}

func escapeICSText(s string) string {
	replacer := strings.NewReplacer(
		`\\`, `\\\\`,
		";", `\;`,
		",", `\,`,
		"\r\n", `\n`,
		"\n", `\n`,
		"\r", `\n`,
	)
	return replacer.Replace(s)
}
