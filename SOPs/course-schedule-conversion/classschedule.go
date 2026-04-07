package main

import (
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/PuerkitoBio/goquery"
)

var (
	lessonIDRe       = regexp.MustCompile(`lesson\.id=(\d+)`)
	lessonNoRe       = regexp.MustCompile(`[A-Z]{2}\d{6}_[0-9]{3}`)
	courseCodeRe     = regexp.MustCompile(`[A-Z]{2}\d{6}`)
	activityDeclRe   = regexp.MustCompile(`activity = new TaskActivity\("([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([01]{53})"\);`)
	activityIndexRe  = regexp.MustCompile(`index =(\d+)\*unitCount\+(\d+);`)
	courseNameTailRe = regexp.MustCompile(`^(.*)\(([A-Z]{2}\d{6}_[0-9]{3})\)$`)
)

type activityBlock struct {
	LessonNo  string
	Name      string
	Teacher   string
	Room      string
	WeekState string
	Slots     []slot
}

type slot struct {
	weekday int
	unit    int
}

func ParseClassScheduleHTML(htmlStr string) ([]Lesson, error) {
	doc, err := goquery.NewDocumentFromReader(strings.NewReader(htmlStr))
	if err != nil {
		return nil, err
	}

	lessonsByNo, order := parseClassScheduleRows(doc)
	if len(lessonsByNo) == 0 {
		return nil, fmt.Errorf("未在课程表 HTML 中解析到任何课程")
	}

	if err := mergeClassScheduleActivities(htmlStr, lessonsByNo, &order); err != nil {
		return nil, err
	}

	lessons := make([]Lesson, 0, len(order))
	for _, no := range order {
		lesson := lessonsByNo[no]
		if lesson.StartWeek == 0 || lesson.EndWeek == 0 {
			lesson.StartWeek, lesson.EndWeek = weekBoundsFromArranges(lesson.ArrangeInfo)
		}
		lesson.Scheduled = len(lesson.ArrangeInfo) > 0
		lessons = append(lessons, lesson)
	}
	return lessons, nil
}

func parseClassScheduleRows(doc *goquery.Document) (map[string]Lesson, []string) {
	lessonsByNo := make(map[string]Lesson)
	order := make([]string, 0)

	doc.Find("tbody[id$='_data'] tr").Each(func(_ int, row *goquery.Selection) {
		tds := row.Find("td")
		if tds.Length() < 11 {
			return
		}

		getText := func(i int) string {
			return strings.TrimSpace(tds.Eq(i).Text())
		}

		no := getText(1)
		if no == "" {
			return
		}

		id := 0
		if href, ok := tds.Eq(1).Find("a").Attr("href"); ok {
			if m := lessonIDRe.FindStringSubmatch(href); m != nil {
				id, _ = strconv.Atoi(m[1])
			}
		}

		code := ""
		if m := courseCodeRe.FindString(getText(2)); m != "" {
			code = m
		}

		credits, _ := strconv.ParseFloat(getText(4), 64)
		lesson := Lesson{
			ID:                 id,
			No:                 no,
			Code:               code,
			Name:               getText(3),
			Credits:            credits,
			Teachers:           getText(5),
			TeachDepartName:    getText(6),
			ExamModeName:       getText(7),
			LangType:           getText(9),
			Remark:             getText(10),
			CourseTypeName:     "&nbsp;",
			CourseCategoryName: "一般课程",
			Withdrawable:       true,
		}

		lessonsByNo[no] = lesson
		order = append(order, no)
	})

	return lessonsByNo, order
}

func mergeClassScheduleActivities(htmlStr string, lessonsByNo map[string]Lesson, order *[]string) error {
	lines := strings.Split(htmlStr, "\n")
	var current *activityBlock

	flush := func() {
		if current == nil || current.LessonNo == "" || len(current.Slots) == 0 {
			return
		}

		lesson, exists := lessonsByNo[current.LessonNo]
		if !exists {
			lesson = Lesson{
				No:                 current.LessonNo,
				Name:               current.Name,
				Teachers:           current.Teacher,
				CourseTypeName:     "&nbsp;",
				CourseCategoryName: "一般课程",
				Withdrawable:       true,
			}
			lessonsByNo[current.LessonNo] = lesson
			*order = append(*order, current.LessonNo)
		}

		if lesson.Name == "" {
			lesson.Name = current.Name
		}
		if lesson.Teachers == "" {
			lesson.Teachers = current.Teacher
		}

		for _, arrange := range buildArrangeInfos(current.Slots, current.WeekState, current.Room) {
			lesson.ArrangeInfo = append(lesson.ArrangeInfo, arrange)
		}
		lessonsByNo[current.LessonNo] = lesson
	}

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		if m := activityDeclRe.FindStringSubmatch(line); m != nil {
			flush()
			current = &activityBlock{
				LessonNo:  firstNonEmpty(extractLessonNo(m[3]), extractLessonNo(m[4])),
				Name:      extractCourseName(m[4]),
				Teacher:   strings.TrimSpace(m[2]),
				Room:      strings.TrimSpace(m[6]),
				WeekState: m[7],
			}
			continue
		}

		if current == nil {
			continue
		}
		if m := activityIndexRe.FindStringSubmatch(line); m != nil {
			weekdayBase, _ := strconv.Atoi(m[1])
			unitBase, _ := strconv.Atoi(m[2])
			current.Slots = append(current.Slots, slot{
				weekday: weekdayBase + 1,
				unit:    unitBase + 1,
			})
		}
	}
	flush()

	return nil
}

func buildArrangeInfos(slots []slot, weekState, room string) []ArrangeInfo {
	byWeekday := make(map[int][]int)
	for _, s := range slots {
		byWeekday[s.weekday] = append(byWeekday[s.weekday], s.unit)
	}

	arranges := make([]ArrangeInfo, 0)
	for weekday, units := range byWeekday {
		sort.Ints(units)
		start := units[0]
		prev := units[0]
		for i := 1; i < len(units); i++ {
			if units[i] == prev+1 {
				prev = units[i]
				continue
			}
			arranges = append(arranges, ArrangeInfo{
				WeekDay:   weekday,
				WeekState: weekState,
				StartUnit: start,
				EndUnit:   prev,
				Rooms:     room,
			})
			start = units[i]
			prev = units[i]
		}
		arranges = append(arranges, ArrangeInfo{
			WeekDay:   weekday,
			WeekState: weekState,
			StartUnit: start,
			EndUnit:   prev,
			Rooms:     room,
		})
	}

	sort.Slice(arranges, func(i, j int) bool {
		if arranges[i].WeekDay != arranges[j].WeekDay {
			return arranges[i].WeekDay < arranges[j].WeekDay
		}
		if arranges[i].StartUnit != arranges[j].StartUnit {
			return arranges[i].StartUnit < arranges[j].StartUnit
		}
		if arranges[i].EndUnit != arranges[j].EndUnit {
			return arranges[i].EndUnit < arranges[j].EndUnit
		}
		return arranges[i].WeekState < arranges[j].WeekState
	})

	return arranges
}

func weekBoundsFromArranges(arranges []ArrangeInfo) (int, int) {
	startWeek := 0
	endWeek := 0
	for _, arrange := range arranges {
		for week := 1; week < len(arrange.WeekState); week++ {
			if arrange.WeekState[week] != '1' {
				continue
			}
			if startWeek == 0 {
				startWeek = week
			}
			endWeek = week
		}
	}
	return startWeek, endWeek
}

func extractLessonNo(s string) string {
	return lessonNoRe.FindString(s)
}

func extractCourseName(s string) string {
	s = strings.TrimSpace(s)
	if m := courseNameTailRe.FindStringSubmatch(s); m != nil {
		return strings.TrimSpace(m[1])
	}
	return s
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if strings.TrimSpace(value) != "" {
			return value
		}
	}
	return ""
}
