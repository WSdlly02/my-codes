package jwxt

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
)

func QueryTeachTaskHTML(client *http.Client, lessonNo string) (string, error) {
	form := url.Values{
		"lesson.semester.id":           {semesterID},
		"lesson.project.id":            {projectID},
		"lesson.no":                    {lessonNo},
		"lesson.course.code":           {""},
		"lesson.course.name":           {""},
		"lesson.course.category.name":  {""},
		"lesson.course.courseType.id":  {""},
		"fake.adminclass.name":         {""},
		"lesson.teachClass.depart.id":  {""},
		"lesson.teachDepart.id":        {""},
		"fake.crossdepart":             {""},
		"teacher.name":                 {""},
		"fake.teacher.null":            {""},
		"fake.teacher.department.id":   {""},
		"lesson.teachClass.grade":      {""},
		"fake.stdCount.start":          {""},
		"fake.stdCount.end":            {""},
		"lesson.courseSchedule.status": {""},
		"fake.time.weekday":            {""},
		"fake.time.unit":               {""},
		"fake.week.start":              {""},
		"fake.week.end":                {""},
		"fake.limitCount.start":        {""},
		"fake.limitCount.end":          {""},
	}

	req, err := http.NewRequest(
		http.MethodPost,
		baseURL+"/teachTaskSearch!arrangeInfoList.action",
		strings.NewReader(form.Encode()),
	)
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
	req.Header.Set("X-Requested-With", "XMLHttpRequest")
	req.Header.Set("Referer", baseURL+"/teachTaskSearch.action")

	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusFound {
		return "", fmt.Errorf("teachTaskSearch 被重定向，登录态可能已失效")
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	return string(body), nil
}
