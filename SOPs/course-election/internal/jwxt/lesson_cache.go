package jwxt

import (
	"fmt"
	"io"
	"net/http"
	"sort"
	"strings"
	"sync"
)

type LessonQueryInfo struct {
	LessonID string
	Name     string
	Code     string
	No       string
	Teachers string
	HasCount bool
	Selected int
	Limit    int
	Reserved int
}

func buildLessonMappingCache(profileID string, lessons []Lesson) LessonMappingCache {
	cache := LessonMappingCache{
		ProfileID:  profileID,
		FetchedAt:  now(),
		SourceURL:  fmt.Sprintf("%s/stdElectCourse!data.action?profileId=%s", baseURL, profileID),
		Lessons:    lessons,
		ByLessonID: make(map[string]LessonRef, len(lessons)),
		ByName:     map[string][]string{},
		ByCode:     map[string][]string{},
		ByTeacher:  map[string][]string{},
	}

	for _, lesson := range lessons {
		id := fmt.Sprintf("%d", lesson.ID)
		ref := LessonRef{
			ID:   lesson.ID,
			No:   lesson.No,
			Code: lesson.Code,
			Name: lesson.Name,
		}
		cache.ByLessonID[id] = ref
		appendUnique(cache.ByName, normalizeIndexKey(lesson.Name), id)
		appendUnique(cache.ByCode, normalizeIndexKey(lesson.Code), id)

		for _, teacher := range splitTeachers(lesson.Teachers) {
			appendUnique(cache.ByTeacher, normalizeIndexKey(teacher), id)
		}
	}

	for key := range cache.ByName {
		sort.Strings(cache.ByName[key])
	}
	for key := range cache.ByCode {
		sort.Strings(cache.ByCode[key])
	}
	for key := range cache.ByTeacher {
		sort.Strings(cache.ByTeacher[key])
	}
	return cache
}

func buildLessonCountSnapshot(profileID string, counts map[string]LessonCount) LessonCountSnapshot {
	return LessonCountSnapshot{
		ProfileID: profileID,
		FetchedAt: now(),
		SourceURL: fmt.Sprintf("%s/stdElectCourse!queryStdCount.action?profileId=%s", baseURL, profileID),
		Counts:    counts,
	}
}

func fetchLessonMapping(client *http.Client, profileID string) ([]Lesson, error) {
	endpoint := fmt.Sprintf("%s/stdElectCourse!data.action?profileId=%s", baseURL, profileID)
	raw, err := fetchPayload(client, endpoint)
	if err != nil {
		return nil, err
	}
	return parseLessonPayload(raw)
}

func fetchLessonCounts(client *http.Client, profileID string) (map[string]LessonCount, error) {
	endpoint := fmt.Sprintf("%s/stdElectCourse!queryStdCount.action?profileId=%s", baseURL, profileID)
	raw, err := fetchPayload(client, endpoint)
	if err != nil {
		return nil, err
	}
	return parseCountPayload(raw)
}

func fetchPayload(client *http.Client, endpoint string) ([]byte, error) {
	resp, err := getWithRetry(client, endpoint)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusFound {
		return nil, fmt.Errorf("请求被重定向，登录态可能已失效: %s", endpoint)
	}
	return io.ReadAll(resp.Body)
}

func appendUnique(index map[string][]string, key, lessonID string) {
	if key == "" {
		return
	}
	for _, existing := range index[key] {
		if existing == lessonID {
			return
		}
	}
	index[key] = append(index[key], lessonID)
}

func normalizeIndexKey(s string) string {
	return strings.TrimSpace(strings.ToLower(s))
}

func splitTeachers(s string) []string {
	if s == "" {
		return nil
	}
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == ',' || r == '，'
	})
	out := make([]string, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part != "" {
			out = append(out, part)
		}
	}
	return out
}

func RefreshAndLoadCourseData(client *http.Client, profileID string) (*LessonMappingCache, *LessonCountSnapshot, error) {
	resp, err := fetchDefaultPage(client, profileID)
	if err != nil {
		return nil, nil, err
	}
	resp.Body.Close()

	lessons, err := fetchLessonMapping(client, profileID)
	if err != nil {
		return nil, nil, err
	}
	mapping := buildLessonMappingCache(profileID, lessons)
	if err := saveLessonMappingCache(profileID, mapping); err != nil {
		return nil, nil, err
	}

	counts, err := fetchLessonCounts(client, profileID)
	if err != nil {
		return &mapping, nil, err
	}
	snapshot := buildLessonCountSnapshot(profileID, counts)
	if err := saveLessonCountSnapshot(profileID, snapshot); err != nil {
		return nil, nil, err
	}
	return &mapping, &snapshot, nil
}

func LoadOrFetchLessonMapping(client *http.Client, profileID string) (*LessonMappingCache, error) {
	if cache, err := LoadLessonMappingCache(profileID); err == nil && len(cache.Lessons) > 0 {
		return cache, nil
	}
	if client == nil {
		return nil, fmt.Errorf("课程映射缓存不存在，且当前无法在线获取 profile=%s", profileID)
	}

	resp, err := fetchDefaultPage(client, profileID)
	if err != nil {
		return nil, err
	}
	resp.Body.Close()

	lessons, err := fetchLessonMapping(client, profileID)
	if err != nil {
		return nil, err
	}
	cache := buildLessonMappingCache(profileID, lessons)
	if err := saveLessonMappingCache(profileID, cache); err != nil {
		return nil, err
	}
	return &cache, nil
}

func RefreshOrLoadLessonCounts(client *http.Client, profileID string) (*LessonCountSnapshot, bool, error) {
	if client != nil {
		resp, err := fetchDefaultPage(client, profileID)
		if err == nil {
			resp.Body.Close()
			counts, err := fetchLessonCounts(client, profileID)
			if err == nil {
				snapshot := buildLessonCountSnapshot(profileID, counts)
				if err := saveLessonCountSnapshot(profileID, snapshot); err == nil {
					return &snapshot, false, nil
				}
			}
		}
	}
	snapshot, err := LoadLessonCountSnapshot(profileID)
	if err != nil {
		return nil, false, err
	}
	return snapshot, true, nil
}

func QueryCourseData(client *http.Client, profileID string) (*LessonMappingCache, *LessonCountSnapshot, bool, error) {
	if mapping, err := LoadLessonMappingCache(profileID); err == nil && len(mapping.Lessons) > 0 {
		counts, countsFromCache, _ := RefreshOrLoadLessonCounts(client, profileID)
		return mapping, counts, countsFromCache, nil
	}
	if client == nil {
		return nil, nil, false, fmt.Errorf("课程映射缓存不存在，且当前无法在线获取 profile=%s", profileID)
	}

	resp, err := fetchDefaultPage(client, profileID)
	if err != nil {
		return nil, nil, false, err
	}
	resp.Body.Close()

	var (
		lessons         []Lesson
		counts          map[string]LessonCount
		mappingErr      error
		countsErr       error
		wg              sync.WaitGroup
		countsSnapshot  *LessonCountSnapshot
		countsFromCache bool
	)

	wg.Add(2)
	go func() {
		defer wg.Done()
		lessons, mappingErr = fetchLessonMapping(client, profileID)
	}()
	go func() {
		defer wg.Done()
		counts, countsErr = fetchLessonCounts(client, profileID)
	}()
	wg.Wait()

	if mappingErr != nil {
		return nil, nil, false, mappingErr
	}

	mapping := buildLessonMappingCache(profileID, lessons)
	if err := saveLessonMappingCache(profileID, mapping); err != nil {
		return nil, nil, false, err
	}

	if countsErr == nil {
		snapshot := buildLessonCountSnapshot(profileID, counts)
		if err := saveLessonCountSnapshot(profileID, snapshot); err == nil {
			countsSnapshot = &snapshot
		}
	}
	if countsSnapshot == nil {
		countsSnapshot, countsFromCache, _ = RefreshOrLoadLessonCounts(nil, profileID)
	}

	return &mapping, countsSnapshot, countsFromCache, nil
}

func BuildLessonQueryInfos(mapping *LessonMappingCache, counts *LessonCountSnapshot) []LessonQueryInfo {
	infos := make([]LessonQueryInfo, 0, len(mapping.Lessons))
	for _, lesson := range mapping.Lessons {
		info := LessonQueryInfo{
			LessonID: fmt.Sprintf("%d", lesson.ID),
			Name:     lesson.Name,
			Code:     lesson.Code,
			No:       lesson.No,
			Teachers: lesson.Teachers,
		}
		if counts != nil {
			if count, ok := counts.Counts[info.LessonID]; ok {
				info.HasCount = true
				info.Selected = count.Selected
				info.Limit = count.Limit
				info.Reserved = count.Reserved
			}
		}
		infos = append(infos, info)
	}

	sort.Slice(infos, func(i, j int) bool {
		if infos[i].Name != infos[j].Name {
			return infos[i].Name < infos[j].Name
		}
		return infos[i].LessonID < infos[j].LessonID
	})
	return infos
}

func MatchLessonQueryInfo(info LessonQueryInfo, keyword string) bool {
	key := normalizeIndexKey(keyword)
	return strings.Contains(normalizeIndexKey(info.Name), key) ||
		strings.Contains(normalizeIndexKey(info.Code), key) ||
		strings.Contains(normalizeIndexKey(info.No), key) ||
		strings.Contains(normalizeIndexKey(info.Teachers), key)
}
