package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"
)

const (
	Timezone           = "Asia/Shanghai"
	FirstDayOfSemester = "2026-03-09" // 2026年春季学期第一天
	DefaultOCRModel    = "qwen3-vl:8b-instruct"
	DefaultOllamaURL   = "http://localhost:11434/api/generate"
	AlarmMinutesBefore = 20
	CourseElectionBin  = "/home/wsdlly02/Documents/my-codes/SOPs/course-election/course-election"
)

var CacheFilePaths = []string{
	"/home/wsdlly02/Documents/my-codes/SOPs/course-election/cache/mapping_2947.json",
	"/home/wsdlly02/Documents/my-codes/SOPs/course-election/cache/mapping_2948.json",
	"/home/wsdlly02/Documents/my-codes/SOPs/course-election/cache/mapping_2938.json",
	"/home/wsdlly02/Documents/my-codes/SOPs/course-election/cache/mapping_2988.json",
	"/home/wsdlly02/Documents/my-codes/SOPs/course-election/cache/mapping_2969.json",
}

func main() {
	// step1: 读取课程表图片，OCR 识别课程信息-> 返回json 数组，元素为课序号
	// step2: 使用课序号查询课程信息
	// step3: 将课程信息翻译为ics，写入文件
	if len(os.Args) < 2 {
		log.Fatalf("用法: %s <课程表图片路径>", os.Args[0])
	}
	imagePath := os.Args[1]

	img, err := os.Open(imagePath)
	if err != nil {
		log.Fatalf("无法打开图片文件: %v", err)
	}
	defer img.Close()

	// 使用 OCR 识别课程信息
	courseNos, err := ocrRecognizeCourseNos(img)
	if err != nil {
		log.Fatalf("OCR 识别失败: %v", err)
	}
	// 从缓存文件中查询课程信息
	courseInfo, missingCourseNos, err := queryCourseInfoFromCache(courseNos, CacheFilePaths)
	if err != nil {
		log.Fatalf("查询课程信息失败: %v", err)
	}
	if string(missingCourseNos) != "[]" {
		log.Printf("存在未命中的课序号，待后续处理: %s", string(missingCourseNos))
		extraCourseInfo, unresolvedCourseNos, err := queryMissingCourseInfoFromTeachTask(missingCourseNos)
		if err != nil {
			log.Fatalf("补查缺失课序号失败: %v", err)
		}
		queriedCourseNos, err := extractCourseNosFromCourseInfo(extraCourseInfo)
		if err != nil {
			log.Fatalf("提取补查成功的课序号失败: %v", err)
		}
		if string(queriedCourseNos) != "[]" {
			log.Printf("已查询到未命中的课序号：%s", string(queriedCourseNos))
		}
		courseInfo, err = mergeCourseInfo(courseInfo, extraCourseInfo)
		if err != nil {
			log.Fatalf("合并课程信息失败: %v", err)
		}
		if string(unresolvedCourseNos) != "[]" {
			log.Printf("警告: 这些课序号进一步查询后仍未解析到课程信息: %s", string(unresolvedCourseNos))
		}
	}
	// 将课程信息转换为 ICS 格式
	icsContent, err := generateICSFromCourseInfo(courseInfo, FirstDayOfSemester, Timezone)
	if err != nil {
		log.Fatalf("生成 ICS 文件失败: %v", err)
	}

	// 将 ICS 内容写入文件
	fileName := fmt.Sprintf("course_schedule_%s.ics", time.Now().Format("20060102_150405"))
	if err := os.WriteFile(fileName, []byte(icsContent), 0644); err != nil {
		log.Fatalf("写入 ICS 文件失败: %v", err)
	}

	log.Println("课程表已成功转换为", fileName)
}

func ocrRecognizeCourseNos(img *os.File) (json.RawMessage, error) {
	imageBytes, err := io.ReadAll(img)
	if err != nil {
		return nil, fmt.Errorf("读取课程表图片失败: %w", err)
	}
	base64Image := base64.StdEncoding.EncodeToString(imageBytes)
	raw, err := callLocalOCR(base64Image, courseNoOCRPrompt(), strictOCRModePrompt())
	if err != nil {
		return nil, err
	}

	courseNosJSON, err := normalizeCourseNoJSON(raw)
	if err != nil {
		return nil, fmt.Errorf("解析 OCR 返回结果失败: %w; 原始输出: %q", err, raw)
	}
	if string(courseNosJSON) == "[]" {
		return nil, fmt.Errorf("OCR 未识别出任何课序号")
	}
	return courseNosJSON, nil
}

func courseNoOCRPrompt() string {
	return strings.TrimSpace(`
You are reading a university course table screenshot.
Only extract the text values under the column titled "课序号".

Rules:
- Ignore every other column, including "序号", "课程号", "课程名", "学分", and links like "查看" or "下载".
- Keep each recognized course number exactly as shown.
- Return a valid JSON array of strings only.
- Do not add explanations, markdown, or code fences.
- If nothing is legible, return [].
Example:
["JG120120_001","JY310430_001"]
`)
}

func strictOCRModePrompt() string {
	return strings.TrimSpace(`
You are an OCR engine. Transcribe content from the image faithfully.
Rules:
- Do NOT invent, guess, or auto-correct.
- Focus only on the requested content.
- Output ONLY the requested content. No explanations. No code fences.
`)
}

func callLocalOCR(base64Image, prompt, system string) (string, error) {
	payload := map[string]any{
		"model":  DefaultOCRModel,
		"system": system,
		"prompt": prompt,
		"images": []string{base64Image},
		"think":  false,
		"stream": false,
		"options": map[string]any{
			"temperature":    0.1,
			"num_ctx":        8192,
			"num_predict":    1024,
			"repeat_penalty": 1.1,
			"top_k":          20,
			"top_p":          0.8,
			"min_p":          0,
		},
	}

	body, err := json.Marshal(payload)
	if err != nil {
		return "", fmt.Errorf("编码 OCR 请求失败: %w", err)
	}

	resp, err := http.Post(DefaultOllamaURL, "application/json", bytes.NewReader(body))
	if err != nil {
		return "", fmt.Errorf("请求本地 Ollama 失败: %w", err)
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("读取 OCR 响应失败: %w", err)
	}
	if resp.StatusCode >= http.StatusBadRequest {
		return "", fmt.Errorf("Ollama 返回错误状态 %d: %s", resp.StatusCode, strings.TrimSpace(string(respBody)))
	}

	var result struct {
		Response string `json:"response"`
		Error    string `json:"error"`
	}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return "", fmt.Errorf("解析 OCR 响应 JSON 失败: %w", err)
	}
	if result.Error != "" {
		return "", fmt.Errorf("Ollama 返回错误: %s", result.Error)
	}
	return strings.TrimSpace(result.Response), nil
}

func normalizeCourseNoJSON(raw string) (json.RawMessage, error) {
	trimmed := strings.TrimSpace(raw)
	trimmed = strings.TrimPrefix(trimmed, "```json")
	trimmed = strings.TrimPrefix(trimmed, "```")
	trimmed = strings.TrimSuffix(trimmed, "```")
	trimmed = strings.TrimSpace(trimmed)

	var items []string
	if err := json.Unmarshal([]byte(trimmed), &items); err == nil {
		normalized, err := json.Marshal(normalizeCourseNos(items))
		if err != nil {
			return nil, fmt.Errorf("序列化规范化课序号失败: %w", err)
		}
		return json.RawMessage(normalized), nil
	}

	re := regexp.MustCompile(`[A-Z]{2}\d{6}_[0-9]{3}`)
	matches := re.FindAllString(trimmed, -1)
	if len(matches) == 0 {
		return nil, fmt.Errorf("响应不是有效 JSON 数组，且未匹配到课序号模式")
	}
	normalized, err := json.Marshal(normalizeCourseNos(matches))
	if err != nil {
		return nil, fmt.Errorf("序列化规范化课序号失败: %w", err)
	}
	return json.RawMessage(normalized), nil
}

func normalizeCourseNos(items []string) []string {
	seen := make(map[string]struct{}, len(items))
	normalized := make([]string, 0, len(items))
	for _, item := range items {
		value := strings.TrimSpace(item)
		if value == "" {
			continue
		}
		if _, ok := seen[value]; ok {
			continue
		}
		seen[value] = struct{}{}
		normalized = append(normalized, value)
	}
	return normalized
}

func queryCourseInfoFromCache(courseNos json.RawMessage, mappingPaths []string) (json.RawMessage, json.RawMessage, error) {
	var nos []string
	if err := json.Unmarshal(courseNos, &nos); err != nil {
		return nil, nil, fmt.Errorf("解析课序号 JSON 失败: %w", err)
	}
	nos = normalizeCourseNos(nos)
	if len(nos) == 0 {
		return nil, nil, fmt.Errorf("课序号列表为空")
	}
	if len(mappingPaths) == 0 {
		return nil, nil, fmt.Errorf("课程缓存路径列表为空")
	}

	byNo := make(map[string]Lesson)
	duplicateNos := make([]string, 0)
	for _, mappingPath := range mappingPaths {
		mapping, err := loadLessonMappingCache(mappingPath)
		if err != nil {
			return nil, nil, err
		}
		for _, item := range mapping.Lessons {
			if _, exists := byNo[item.No]; exists {
				duplicateNos = append(duplicateNos, item.No)
				continue
			}
			byNo[item.No] = item
		}
	}

	if len(duplicateNos) > 0 {
		log.Printf("警告: 多个缓存文件中存在重复课序号，已使用首次命中的记录: %s", strings.Join(normalizeCourseNos(duplicateNos), ", "))
	}

	matched := make([]Lesson, 0, len(nos))
	missing := make([]string, 0)
	for _, no := range nos {
		item, ok := byNo[no]
		if !ok {
			missing = append(missing, no)
			continue
		}
		matched = append(matched, item)
	}
	if len(missing) > 0 {
		log.Printf("警告: 缓存中未找到这些课序号，已跳过: %s", strings.Join(missing, ", "))
	}

	matchedJSON, err := json.Marshal(matched)
	if err != nil {
		return nil, nil, fmt.Errorf("序列化课程信息失败: %w", err)
	}
	missingJSON, err := json.Marshal(missing)
	if err != nil {
		return nil, nil, fmt.Errorf("序列化缺失课序号失败: %w", err)
	}
	if len(matched) == 0 {
		return nil, json.RawMessage(missingJSON), fmt.Errorf("缓存中未找到任何课序号: %s", strings.Join(missing, ", "))
	}

	return json.RawMessage(matchedJSON), json.RawMessage(missingJSON), nil
}

func loadLessonMappingCache(path string) (*LessonMappingCache, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("读取课程缓存失败: %w", err)
	}

	var mapping LessonMappingCache
	if err := json.Unmarshal(data, &mapping); err != nil {
		return nil, fmt.Errorf("解析课程缓存失败: %w", err)
	}
	if len(mapping.Lessons) == 0 {
		return nil, fmt.Errorf("课程缓存为空: %s", path)
	}
	return &mapping, nil
}

func queryMissingCourseInfoFromTeachTask(courseNos json.RawMessage) (json.RawMessage, json.RawMessage, error) {
	var nos []string
	if err := json.Unmarshal(courseNos, &nos); err != nil {
		return nil, nil, fmt.Errorf("解析待补查课序号失败: %w", err)
	}
	nos = normalizeCourseNos(nos)
	if len(nos) == 0 {
		empty := json.RawMessage("[]")
		return empty, empty, nil
	}

	extraLessons := make([]Lesson, 0)
	unresolved := make([]string, 0)
	for _, no := range nos {
		html, err := queryTeachTaskHTML(no)
		if err != nil {
			log.Printf("警告: 查询课序号 %s 的 teach-task HTML 失败: %v", no, err)
			unresolved = append(unresolved, no)
			continue
		}

		parsed, err := ParseTeachTaskHTML(html)
		if err != nil {
			log.Printf("警告: 解析课序号 %s 的 teach-task HTML 失败: %v", no, err)
			unresolved = append(unresolved, no)
			continue
		}
		if len(parsed) == 0 {
			log.Printf("警告: 课序号 %s 的 teach-task 查询未返回可解析课程", no)
			unresolved = append(unresolved, no)
			continue
		}

		extraLessons = append(extraLessons, convertQueryLessons(parsed)...)
	}

	extraJSON, err := json.Marshal(extraLessons)
	if err != nil {
		return nil, nil, fmt.Errorf("序列化补查课程信息失败: %w", err)
	}
	unresolvedJSON, err := json.Marshal(normalizeCourseNos(unresolved))
	if err != nil {
		return nil, nil, fmt.Errorf("序列化未解决课序号失败: %w", err)
	}
	return json.RawMessage(extraJSON), json.RawMessage(unresolvedJSON), nil
}

func queryTeachTaskHTML(lessonNo string) (string, error) {
	cmd := exec.Command(CourseElectionBin, "query", "--teach-task", "--lesson-no", lessonNo)
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

func convertQueryLessons(items []Lesson) []Lesson {
	out := make([]Lesson, 0, len(items))
	for _, item := range items {
		arrangeInfo := make([]ArrangeInfo, 0, len(item.ArrangeInfo))
		for _, arrange := range item.ArrangeInfo {
			arrangeInfo = append(arrangeInfo, ArrangeInfo{
				WeekDay:   arrange.WeekDay,
				WeekState: arrange.WeekState,
				StartUnit: arrange.StartUnit,
				EndUnit:   arrange.EndUnit,
				Rooms:     arrange.Rooms,
			})
		}
		out = append(out, Lesson{
			ID:                 item.ID,
			No:                 item.No,
			Name:               item.Name,
			Code:               item.Code,
			Credits:            item.Credits,
			CourseID:           item.CourseID,
			StartWeek:          item.StartWeek,
			EndWeek:            item.EndWeek,
			CourseTypeID:       item.CourseTypeID,
			CourseTypeName:     item.CourseTypeName,
			CourseTypeCode:     item.CourseTypeCode,
			CourseCategoryName: item.CourseCategoryName,
			TeachDepartName:    item.TeachDepartName,
			ExamModeName:       item.ExamModeName,
			Scheduled:          item.Scheduled,
			HasTextBook:        item.HasTextBook,
			Period:             item.Period,
			WeekHour:           item.WeekHour,
			Withdrawable:       item.Withdrawable,
			Textbooks:          item.Textbooks,
			Teachers:           item.Teachers,
			CampusCode:         item.CampusCode,
			LangType:           item.LangType,
			CampusName:         item.CampusName,
			AdminClass:         item.AdminClass,
			Remark:             item.Remark,
			ArrangeInfo:        arrangeInfo,
		})
	}
	return out
}

func mergeCourseInfo(baseCourseInfo, extraCourseInfo json.RawMessage) (json.RawMessage, error) {
	var baseLessons []Lesson
	if err := json.Unmarshal(baseCourseInfo, &baseLessons); err != nil {
		return nil, fmt.Errorf("解析已有课程信息失败: %w", err)
	}

	var extraLessons []Lesson
	if err := json.Unmarshal(extraCourseInfo, &extraLessons); err != nil {
		return nil, fmt.Errorf("解析补查课程信息失败: %w", err)
	}

	merged := make([]Lesson, 0, len(baseLessons)+len(extraLessons))
	seen := make(map[string]struct{}, len(baseLessons)+len(extraLessons))
	for _, lesson := range append(baseLessons, extraLessons...) {
		if _, ok := seen[lesson.No]; ok {
			continue
		}
		seen[lesson.No] = struct{}{}
		merged = append(merged, lesson)
	}

	result, err := json.Marshal(merged)
	if err != nil {
		return nil, fmt.Errorf("序列化合并后的课程信息失败: %w", err)
	}
	return json.RawMessage(result), nil
}

func extractCourseNosFromCourseInfo(courseInfo json.RawMessage) (json.RawMessage, error) {
	var lessons []Lesson
	if err := json.Unmarshal(courseInfo, &lessons); err != nil {
		return nil, fmt.Errorf("解析课程信息失败: %w", err)
	}

	nos := make([]string, 0, len(lessons))
	for _, lesson := range lessons {
		if strings.TrimSpace(lesson.No) == "" {
			continue
		}
		nos = append(nos, lesson.No)
	}

	result, err := json.Marshal(normalizeCourseNos(nos))
	if err != nil {
		return nil, fmt.Errorf("序列化课序号失败: %w", err)
	}
	return json.RawMessage(result), nil
}
