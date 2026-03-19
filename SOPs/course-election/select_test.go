package main

import "testing"

func TestResolveLessonIDByName(t *testing.T) {
	cache := &lessonMappingCache{
		ByName: map[string][]string{
			"刑法学": {"244433"},
		},
	}

	id, err := resolveLessonIDByName(cache, "刑法学")
	if err != nil {
		t.Fatalf("resolve lesson id: %v", err)
	}
	if id != "244433" {
		t.Fatalf("unexpected lesson id: %s", id)
	}
}

func TestSummarizeSelectionResponse(t *testing.T) {
	body := `<html><body><div style="margin:auto;"> 选课成功 </br></div></body></html>`
	msg := summarizeSelectionResponse(body)
	if msg != "选课成功" {
		t.Fatalf("unexpected message: %q", msg)
	}
}
