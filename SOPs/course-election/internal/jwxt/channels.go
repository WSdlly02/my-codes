package jwxt

import (
	"errors"
	"io"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"time"
)

var (
	reRow       = regexp.MustCompile(`(?s)<tr>(.*?)</tr>`)
	reCell      = regexp.MustCompile(`(?s)<td[^>]*>(.*?)</td>`)
	reProfileID = regexp.MustCompile(`electionProfile\.id=(\d+)`)
	reBeginAt   = regexp.MustCompile(`new Date\((\d+),(\d+)-1,(\d+),(\d+),(\d+),(\d+)\)`)
	reTag       = regexp.MustCompile(`<[^>]+>`)
)

func FetchAndCacheChannels(client *http.Client) ([]ChannelEntry, error) {
	endpoint := baseURL + "/stdElectCourse.action"
	resp, err := client.Get(endpoint)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusFound {
		return nil, errors.New("请求被重定向，登录态可能已失效")
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	channels := parseChannels(string(body))
	if len(channels) == 0 {
		return nil, errors.New("未能从 stdElectCourse.action 解析出任何通道")
	}
	if err := saveChannelCache(channels); err != nil {
		return nil, err
	}
	return channels, nil
}

func parseChannels(html string) []ChannelEntry {
	matches := reRow.FindAllStringSubmatch(html, -1)
	channels := make([]ChannelEntry, 0, len(matches))
	discoveredAt := now()

	for _, match := range matches {
		row := match[1]
		cells := reCell.FindAllStringSubmatch(row, -1)
		if len(cells) < 5 {
			continue
		}

		profileID := firstSubmatch(reProfileID, row)
		if profileID == "" {
			continue
		}

		entry := ChannelEntry{
			RoundNo:      cleanHTML(cells[0][1]),
			Name:         cleanHTML(cells[1][1]),
			OpenTime:     cleanHTML(cells[2][1]),
			Notice:       cleanHTML(cells[3][1]),
			ProfileID:    profileID,
			Opened:       !strings.Contains(cells[4][1], "setInterval"),
			DiscoveredAt: discoveredAt,
		}
		if beginAt := parseBeginAt(cells[4][1]); !beginAt.IsZero() {
			entry.BeginAt = &beginAt
		}
		channels = append(channels, entry)
	}
	return channels
}

func parseBeginAt(actionCell string) time.Time {
	m := reBeginAt.FindStringSubmatch(actionCell)
	if len(m) != 7 {
		return time.Time{}
	}

	parts := make([]int, 0, 6)
	for i := 1; i <= 6; i++ {
		v, err := strconv.Atoi(m[i])
		if err != nil {
			return time.Time{}
		}
		parts = append(parts, v)
	}
	return time.Date(parts[0], time.Month(parts[1]), parts[2], parts[3], parts[4], parts[5], 0, localTZ())
}

func firstSubmatch(re *regexp.Regexp, s string) string {
	m := re.FindStringSubmatch(s)
	if len(m) < 2 {
		return ""
	}
	return m[1]
}

func cleanHTML(s string) string {
	s = strings.ReplaceAll(s, "<br/>", "\n")
	s = strings.ReplaceAll(s, "<br>", "\n")
	s = strings.ReplaceAll(s, "&nbsp;", " ")
	s = reTag.ReplaceAllString(s, " ")
	return strings.Join(strings.Fields(s), " ")
}
