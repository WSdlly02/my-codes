package jwxt

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"regexp"
	"strings"
)

var reClassScheduleStdIDs = regexp.MustCompile(`(?s)if\(jQuery\("#courseTableType"\)\.val\(\)=="std"\)\s*\{\s*bg\.form\.addInput\(form,"ids","(\d+)"\);`)

func QueryClassScheduleHTML(client *http.Client) (string, error) {
	entryHTML, err := fetchClassScheduleEntryHTML(client)
	if err != nil {
		return "", err
	}

	stdID, err := parseUniqueStdID(entryHTML)
	if err != nil {
		return "", err
	}

	return fetchClassScheduleTableHTML(client, stdID)
}

func fetchClassScheduleEntryHTML(client *http.Client) (string, error) {
	u, err := url.Parse(baseURL)
	if err != nil {
		return "", err
	}
	if client.Jar != nil {
		client.Jar.SetCookies(u, []*http.Cookie{
			{Name: "semester.id", Value: semesterID, Path: "/"},
		})
	}

	req, err := http.NewRequest(http.MethodGet, baseURL+"/courseTableForStd.action", nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("Accept", "text/html, */*; q=0.01")
	req.Header.Set("Accept-Language", "en,zh-CN;q=0.9,zh;q=0.8")
	req.Header.Set("X-Requested-With", "XMLHttpRequest")
	req.Header.Set("Referer", baseURL+"/home!childmenus.action?menu.id=10841&security.categoryId=1")
	req.Header.Set("Sec-Fetch-Dest", "empty")
	req.Header.Set("Sec-Fetch-Mode", "cors")
	req.Header.Set("Sec-Fetch-Site", "same-origin")
	req.Header.Set("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36")
	req.Header.Set("sec-ch-ua", `"Chromium";v="146", "Not-A.Brand";v="24", "Google Chrome";v="146"`)
	req.Header.Set("sec-ch-ua-mobile", "?0")
	req.Header.Set("sec-ch-ua-platform", `"Linux"`)

	resp, err := doRequestWithRetry(client, req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusFound {
		return "", fmt.Errorf("courseTableForStd 被重定向，登录态可能已失效")
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	return string(body), nil
}

func parseUniqueStdID(html string) (string, error) {
	matches := reClassScheduleStdIDs.FindAllStringSubmatch(html, -1)
	unique := map[string]struct{}{}
	for _, match := range matches {
		if len(match) < 2 {
			continue
		}
		unique[match[1]] = struct{}{}
	}

	if len(unique) != 1 {
		return "", fmt.Errorf("无法从课表入口页提取唯一学号，匹配到 %d 个候选", len(unique))
	}
	for id := range unique {
		return id, nil
	}
	return "", fmt.Errorf("无法从课表入口页提取学号")
}

func fetchClassScheduleTableHTML(client *http.Client, stdID string) (string, error) {
	form := url.Values{
		"ignoreHead":   {"1"},
		"setting.kind": {"std"},
		"startWeek":    {"1"},
		"semester.id":  {semesterID},
		"ids":          {stdID},
	}

	req, err := http.NewRequest(http.MethodPost, baseURL+"/courseTableForStd!courseTable.action", strings.NewReader(form.Encode()))
	if err != nil {
		return "", err
	}
	req.Header.Set("Accept", "*/*")
	req.Header.Set("Accept-Language", "en,zh-CN;q=0.9,zh;q=0.8")
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("Origin", "https://jwxt.shmtu.edu.cn")
	req.Header.Set("Referer", baseURL+"/courseTableForStd.action")
	req.Header.Set("Sec-Fetch-Dest", "empty")
	req.Header.Set("Sec-Fetch-Mode", "cors")
	req.Header.Set("Sec-Fetch-Site", "same-origin")
	req.Header.Set("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36")
	req.Header.Set("X-Requested-With", "XMLHttpRequest")
	req.Header.Set("sec-ch-ua", `"Chromium";v="146", "Not-A.Brand";v="24", "Google Chrome";v="146"`)
	req.Header.Set("sec-ch-ua-mobile", "?0")
	req.Header.Set("sec-ch-ua-platform", `"Linux"`)

	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusFound {
		return "", fmt.Errorf("courseTableForStd!courseTable 被重定向，登录态可能已失效")
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	return string(body), nil
}
