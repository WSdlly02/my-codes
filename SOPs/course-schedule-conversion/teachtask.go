package main

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/PuerkitoBio/goquery"
)

// ==================== 解析安排字符串 ====================

// 匹配 "杨罡 星期3 3-4 [1-16] 教学3D106" 或 "杨罡 星期3 3-4 [1-16]"
var arrangeRe = regexp.MustCompile(
	`星期(\d)\s+(\d+)-(\d+)\s+\[(\d+)-(\d+)\]\s*([^\s]*)`)

func parseArrangeLines(raw string) []ArrangeInfo {
	var result []ArrangeInfo
	for _, line := range strings.Split(raw, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		m := arrangeRe.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		weekDay, _ := strconv.Atoi(m[1])
		startUnit, _ := strconv.Atoi(m[2])
		endUnit, _ := strconv.Atoi(m[3])
		startWeek, _ := strconv.Atoi(m[4])
		endWeek, _ := strconv.Atoi(m[5])
		rooms := strings.TrimSpace(m[6])

		result = append(result, ArrangeInfo{
			WeekDay:   weekDay,
			WeekState: buildWeekState(startWeek, endWeek),
			StartUnit: startUnit,
			EndUnit:   endUnit,
			Rooms:     rooms,
		})
	}
	return result
}

func buildWeekState(start, end int) string {
	bs := make([]byte, 53)
	for i := range bs {
		bs[i] = '0'
	}
	for w := start; w <= end && w < 53; w++ {
		bs[w] = '1'
	}
	return string(bs)
}

// ==================== 解析单个 HTML 文件 ====================

func ParseTeachTaskHTML(htmlStr string) ([]Lesson, error) {
	doc, err := goquery.NewDocumentFromReader(strings.NewReader(htmlStr))
	if err != nil {
		return nil, err
	}

	var lessons []Lesson

	doc.Find("tbody tr").Each(func(_ int, row *goquery.Selection) {
		tds := row.Find("td")
		if tds.Length() < 13 {
			return
		}

		getText := func(i int) string {
			return strings.TrimSpace(tds.Eq(i).Text())
		}

		id, _ := strconv.Atoi(func() string {
			v, _ := tds.Eq(0).Find("input[name='lesson.id']").Attr("value")
			return v
		}())

		// td[5]: 行政班，取 span 的 title 属性（完整班级列表）
		adminClass, exists := tds.Eq(5).Find("span[title]").Attr("title")
		if !exists {
			adminClass = getText(5)
		}

		credits, _ := strconv.ParseFloat(getText(10), 64)
		period, _ := strconv.Atoi(getText(9))
		startWeek, _ := strconv.Atoi(getText(12))
		endWeek, _ := strconv.Atoi(getText(13))

		// 计算 weekHour = period / (endWeek - startWeek + 1)
		weekHour := 0
		if weeks := endWeek - startWeek + 1; weeks > 0 {
			weekHour = period / weeks
		}

		lesson := Lesson{
			ID:           id,
			No:           getText(1),
			Code:         getText(2),
			Name:         getText(3),
			Credits:      credits,
			Period:       period,
			WeekHour:     weekHour,
			StartWeek:    startWeek,
			EndWeek:      endWeek,
			ExamModeName: getText(11),
			Teachers:     getText(6),
			AdminClass:   strings.TrimSpace(adminClass),
			ArrangeInfo:  parseArrangeLines(getText(4)),
			// 缓存中有但 HTML 没有的字段，保留默认值
			CourseTypeName:     "&nbsp;",
			CourseCategoryName: "一般课程",
			Scheduled:          true,
			Withdrawable:       true,
			LangType:           "中文",
			Textbooks:          "",
			Remark:             "",
		}
		lessons = append(lessons, lesson)
	})

	return lessons, nil
}
