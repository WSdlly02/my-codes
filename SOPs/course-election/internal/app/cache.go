package app

import (
	"fmt"
	"net/http"
	"os"
	"path/filepath"

	"github.com/bytedance/sonic"
)

func ensureCacheDir() error {
	return os.MkdirAll(cacheDir, 0o755)
}

func channelsCachePath() string {
	return filepath.Join(cacheDir, "channels.json")
}

func mappingCachePath(profileID string) string {
	return filepath.Join(cacheDir, "mapping_"+profileID+".json")
}

func countsCachePath(profileID string) string {
	return filepath.Join(cacheDir, "counts_"+profileID+".json")
}

func saveCookies(cookies []*http.Cookie) error {
	sc := savedCookies{}
	for _, c := range cookies {
		item := savedCookie{
			Name:     c.Name,
			Value:    c.Value,
			Domain:   c.Domain,
			Path:     c.Path,
			HttpOnly: c.HttpOnly,
			Secure:   c.Secure,
		}
		if !c.Expires.IsZero() {
			expires := c.Expires
			item.Expires = &expires
		}
		sc.Cookies = append(sc.Cookies, item)
	}

	data, err := sonic.MarshalIndent(sc, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(cookieFile, data, 0o600)
}

func loadCookies() []*http.Cookie {
	data, err := os.ReadFile(cookieFile)
	if err != nil {
		data, err = os.ReadFile(legacyCookieFile)
	}
	if err != nil {
		return nil
	}

	var sc savedCookies
	if err := sonic.Unmarshal(data, &sc); err != nil {
		return nil
	}

	cookies := make([]*http.Cookie, 0, len(sc.Cookies))
	for _, c := range sc.Cookies {
		cookie := &http.Cookie{
			Name:     c.Name,
			Value:    c.Value,
			Domain:   c.Domain,
			Path:     c.Path,
			HttpOnly: c.HttpOnly,
			Secure:   c.Secure,
		}
		if c.Expires != nil {
			cookie.Expires = *c.Expires
		}
		cookies = append(cookies, cookie)
	}
	return cookies
}

func saveChannelCache(channels []channelEntry) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}

	payload, err := sonic.MarshalIndent(channelCache{
		FetchedAt: now(),
		SourceURL: baseURL + "/stdElectCourse.action",
		Channels:  channels,
	}, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(channelsCachePath(), payload, 0o644)
}

func saveLessonMappingCache(profileID string, data lessonMappingCache) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}
	payload, err := sonic.MarshalIndent(data, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(mappingCachePath(profileID), payload, 0o644)
}

func saveLessonCountSnapshot(profileID string, data lessonCountSnapshot) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}
	payload, err := sonic.MarshalIndent(data, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(countsCachePath(profileID), payload, 0o644)
}

func loadLessonMappingCache(profileID string) (*lessonMappingCache, error) {
	data, err := os.ReadFile(mappingCachePath(profileID))
	if err != nil {
		return nil, err
	}
	var cache lessonMappingCache
	if err := sonic.Unmarshal(data, &cache); err != nil {
		return nil, err
	}
	return &cache, nil
}

func loadLessonCountSnapshot(profileID string) (*lessonCountSnapshot, error) {
	data, err := os.ReadFile(countsCachePath(profileID))
	if err != nil {
		return nil, err
	}
	var snap lessonCountSnapshot
	if err := sonic.Unmarshal(data, &snap); err != nil {
		return nil, err
	}
	return &snap, nil
}

func cacheExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func cookieSummary(cookies []*http.Cookie) []string {
	lines := make([]string, 0, len(cookies))
	for _, c := range cookies {
		if c.Expires.IsZero() {
			lines = append(lines, fmt.Sprintf("%s: session cookie", c.Name))
			continue
		}
		lines = append(lines, fmt.Sprintf("%s: expires at %s", c.Name, c.Expires.In(localTZ()).Format(timeLayout)))
	}
	return lines
}
