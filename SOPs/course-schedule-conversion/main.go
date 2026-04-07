package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/bytedance/sonic"
)

const (
	Timezone           = "Asia/Shanghai"
	FirstDayOfSemester = "2026-03-09" // 2026年春季学期第一天
	AlarmMinutesBefore = 20
	TeachingWeekCount  = 17
	CourseElectionBin  = "/home/wsdlly02/Documents/my-codes/SOPs/course-election/course-election"
)

func main() {
	html, err := queryClassScheduleHTML()
	if err != nil {
		log.Fatalf("查询课程表 HTML 失败: %v", err)
	}

	lessons, err := ParseClassScheduleHTML(html)
	if err != nil {
		log.Fatalf("解析课程表 HTML 失败: %v", err)
	}

	courseInfo, err := sonic.Marshal(lessons)
	if err != nil {
		log.Fatalf("序列化课程信息失败: %v", err)
	}

	icsContent, err := generateICSFromCourseInfo(json.RawMessage(courseInfo), FirstDayOfSemester, Timezone)
	if err != nil {
		log.Fatalf("生成 ICS 文件失败: %v", err)
	}

	fileName := fmt.Sprintf("course_schedule_%s.ics", time.Now().Format("20060102_150405"))
	if err := os.WriteFile(fileName, []byte(icsContent), 0644); err != nil {
		log.Fatalf("写入 ICS 文件失败: %v", err)
	}

	log.Println("课程表已成功转换为", fileName)
}

func queryClassScheduleHTML() (string, error) {
	cmd := exec.Command(CourseElectionBin, "query", "--class-schedule")
	cmd.Dir = strings.TrimSuffix(CourseElectionBin, "/course-election")
	output, err := cmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return "", fmt.Errorf("%w: %s", err, strings.TrimSpace(string(exitErr.Stderr)))
		}
		return "", err
	}
	return string(output), nil
}
