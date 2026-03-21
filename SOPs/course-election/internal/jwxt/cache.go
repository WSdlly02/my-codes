package jwxt

import (
	"errors"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bytedance/sonic"
)

type LessonMappingCacheStatus struct {
	ProfileID   string
	LessonCount int
	FetchedAt   string
}

type LessonCountCacheStatus struct {
	ProfileID  string
	CountCount int
	FetchedAt  string
}

func ensureCacheDir() error {
	return os.MkdirAll(cacheDir, 0o755)
}

func ChannelsCachePath() string {
	return filepath.Join(cacheDir, "channels.json")
}

func MappingCachePath(profileID string) string {
	return filepath.Join(cacheDir, "mapping_"+profileID+".json")
}

func CountsCachePath(profileID string) string {
	return filepath.Join(cacheDir, "counts_"+profileID+".json")
}

func FlushLoginState() error {
	if err := os.Remove(cookieFile); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return nil
}

func FlushDerivedCaches() error {
	patterns := []string{
		filepath.Join(cacheDir, "mapping_*.json"),
		filepath.Join(cacheDir, "counts_*.json"),
	}

	for _, pattern := range patterns {
		paths, err := filepath.Glob(pattern)
		if err != nil {
			return err
		}
		for _, path := range paths {
			if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
				return err
			}
		}
	}
	return nil
}

func saveCookies(cookies []*http.Cookie) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}

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

func saveChannelCache(channels []ChannelEntry) error {
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
	return os.WriteFile(ChannelsCachePath(), payload, 0o644)
}

func saveLessonMappingCache(profileID string, data LessonMappingCache) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}
	payload, err := sonic.MarshalIndent(data, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(MappingCachePath(profileID), payload, 0o644)
}

func saveLessonCountSnapshot(profileID string, data LessonCountSnapshot) error {
	if err := ensureCacheDir(); err != nil {
		return err
	}
	payload, err := sonic.MarshalIndent(data, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(CountsCachePath(profileID), payload, 0o644)
}

func LoadLessonMappingCache(profileID string) (*LessonMappingCache, error) {
	data, err := os.ReadFile(MappingCachePath(profileID))
	if err != nil {
		return nil, err
	}
	var cache LessonMappingCache
	if err := sonic.Unmarshal(data, &cache); err != nil {
		return nil, err
	}
	return &cache, nil
}

func LoadLessonCountSnapshot(profileID string) (*LessonCountSnapshot, error) {
	data, err := os.ReadFile(CountsCachePath(profileID))
	if err != nil {
		return nil, err
	}
	var snap LessonCountSnapshot
	if err := sonic.Unmarshal(data, &snap); err != nil {
		return nil, err
	}
	return &snap, nil
}

func LoadChannelCache() (*channelCache, error) {
	data, err := os.ReadFile(ChannelsCachePath())
	if err != nil {
		return nil, err
	}
	var cache channelCache
	if err := sonic.Unmarshal(data, &cache); err != nil {
		return nil, err
	}
	return &cache, nil
}

func CacheExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func ListLessonMappingCacheStatuses() ([]LessonMappingCacheStatus, error) {
	paths, err := filepath.Glob(filepath.Join(cacheDir, "mapping_*.json"))
	if err != nil {
		return nil, err
	}

	statuses := make([]LessonMappingCacheStatus, 0, len(paths))
	for _, path := range paths {
		data, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}

		var cache LessonMappingCache
		if err := sonic.Unmarshal(data, &cache); err != nil {
			return nil, err
		}

		statuses = append(statuses, LessonMappingCacheStatus{
			ProfileID:   firstNonEmpty(cache.ProfileID, profileIDFromPath(path, "mapping_")),
			LessonCount: len(cache.Lessons),
			FetchedAt:   cache.FetchedAt.In(localTZ()).Format(timeLayout),
		})
	}

	sort.Slice(statuses, func(i, j int) bool {
		return statuses[i].ProfileID < statuses[j].ProfileID
	})
	return statuses, nil
}

func ListLessonCountCacheStatuses() ([]LessonCountCacheStatus, error) {
	paths, err := filepath.Glob(filepath.Join(cacheDir, "counts_*.json"))
	if err != nil {
		return nil, err
	}

	statuses := make([]LessonCountCacheStatus, 0, len(paths))
	for _, path := range paths {
		data, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}

		var cache LessonCountSnapshot
		if err := sonic.Unmarshal(data, &cache); err != nil {
			return nil, err
		}

		statuses = append(statuses, LessonCountCacheStatus{
			ProfileID:  firstNonEmpty(cache.ProfileID, profileIDFromPath(path, "counts_")),
			CountCount: len(cache.Counts),
			FetchedAt:  cache.FetchedAt.In(localTZ()).Format(timeLayout),
		})
	}

	sort.Slice(statuses, func(i, j int) bool {
		return statuses[i].ProfileID < statuses[j].ProfileID
	})
	return statuses, nil
}

func CookieSummary(cookies []*http.Cookie) []string {
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

func profileIDFromPath(path, prefix string) string {
	base := filepath.Base(path)
	name := strings.TrimSuffix(base, filepath.Ext(base))
	return strings.TrimPrefix(name, prefix)
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if value != "" {
			return value
		}
	}
	return ""
}
