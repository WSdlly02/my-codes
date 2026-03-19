package app

import (
	"fmt"
	"io"
	"net/http"
	"sort"
	"strings"
)

func buildLessonMappingCache(profileID string, lessons []lesson) lessonMappingCache {
	cache := lessonMappingCache{
		ProfileID:  profileID,
		FetchedAt:  now(),
		SourceURL:  fmt.Sprintf("%s/stdElectCourse!data.action?profileId=%s", baseURL, profileID),
		Lessons:    lessons,
		ByLessonID: make(map[string]lessonRef, len(lessons)),
		ByName:     map[string][]string{},
		ByCode:     map[string][]string{},
		ByTeacher:  map[string][]string{},
	}

	for _, lesson := range lessons {
		id := fmt.Sprintf("%d", lesson.ID)
		ref := lessonRef{
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

func buildLessonCountSnapshot(profileID string, counts map[string]lessonCount) lessonCountSnapshot {
	return lessonCountSnapshot{
		ProfileID: profileID,
		FetchedAt: now(),
		SourceURL: fmt.Sprintf("%s/stdElectCourse!queryStdCount.action?profileId=%s", baseURL, profileID),
		Counts:    counts,
	}
}

func fetchLessonMapping(client *http.Client, profileID string) ([]lesson, error) {
	endpoint := fmt.Sprintf("%s/stdElectCourse!data.action?profileId=%s", baseURL, profileID)
	raw, err := fetchPayload(client, endpoint)
	if err != nil {
		return nil, err
	}
	return parseLessonPayload(raw)
}

func fetchLessonCounts(client *http.Client, profileID string) (map[string]lessonCount, error) {
	endpoint := fmt.Sprintf("%s/stdElectCourse!queryStdCount.action?profileId=%s", baseURL, profileID)
	raw, err := fetchPayload(client, endpoint)
	if err != nil {
		return nil, err
	}
	return parseCountPayload(raw)
}

func fetchPayload(client *http.Client, endpoint string) ([]byte, error) {
	resp, err := client.Get(endpoint)
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
