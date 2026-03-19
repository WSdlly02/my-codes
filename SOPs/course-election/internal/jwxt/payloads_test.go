package jwxt

import (
	"os"
	"path/filepath"
	"testing"
)

func fixturePath(name string) string {
	return filepath.Join("..", "..", name)
}

func TestParseLessonPayload(t *testing.T) {
	raw, err := os.ReadFile(fixturePath("stdElectCourse!data.action.json"))
	if err != nil {
		t.Fatalf("read payload: %v", err)
	}

	lessons, err := parseLessonPayload(raw)
	if err != nil {
		t.Fatalf("parse lessons: %v", err)
	}
	if len(lessons) == 0 {
		t.Fatal("expected parsed lessons")
	}
	if lessons[0].ID == 0 || lessons[0].Name == "" {
		t.Fatalf("unexpected first lesson: %+v", lessons[0])
	}
}

func TestParseCountPayload(t *testing.T) {
	raw, err := os.ReadFile(fixturePath("stdElectCourse!queryStdCount.action.json"))
	if err != nil {
		t.Fatalf("read payload: %v", err)
	}

	counts, err := parseCountPayload(raw)
	if err != nil {
		t.Fatalf("parse counts: %v", err)
	}
	if len(counts) == 0 {
		t.Fatal("expected parsed counts")
	}
	if counts["244758"].Limit == 0 {
		t.Fatalf("unexpected count entry: %+v", counts["244758"])
	}
}

func TestBuildLessonMappingCache(t *testing.T) {
	raw, err := os.ReadFile(fixturePath("stdElectCourse!data.action.json"))
	if err != nil {
		t.Fatalf("read payload: %v", err)
	}

	lessons, err := parseLessonPayload(raw)
	if err != nil {
		t.Fatalf("parse lessons: %v", err)
	}

	cache := buildLessonMappingCache("2936", lessons)
	if cache.ProfileID != "2936" {
		t.Fatalf("unexpected profileID: %s", cache.ProfileID)
	}
	if len(cache.ByLessonID) != len(lessons) {
		t.Fatalf("byLessonID size mismatch: got %d want %d", len(cache.ByLessonID), len(lessons))
	}
	if len(cache.ByName["刑法学"]) == 0 {
		t.Fatal("expected name index for 刑法学")
	}
	if len(cache.ByCode["fx110010"]) == 0 {
		t.Fatal("expected code index for fx110010")
	}
	if len(cache.ByTeacher["张宜培"]) == 0 {
		t.Fatal("expected teacher index for 张宜培")
	}
}

func TestBuildLessonCountSnapshot(t *testing.T) {
	raw, err := os.ReadFile(fixturePath("stdElectCourse!queryStdCount.action.json"))
	if err != nil {
		t.Fatalf("read payload: %v", err)
	}

	counts, err := parseCountPayload(raw)
	if err != nil {
		t.Fatalf("parse counts: %v", err)
	}

	snapshot := buildLessonCountSnapshot("2936", counts)
	if snapshot.ProfileID != "2936" {
		t.Fatalf("unexpected profileID: %s", snapshot.ProfileID)
	}
	if snapshot.Counts["244758"].Selected == 0 {
		t.Fatalf("unexpected snapshot count: %+v", snapshot.Counts["244758"])
	}
}
