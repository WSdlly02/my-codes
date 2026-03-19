package jwxt

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"regexp"
	"strings"
)

var reBodyMessage = regexp.MustCompile(`(?s)margin:auto;">\s*(.*?)\s*</br>`)

func SelectLesson(client *http.Client, profileID, lessonID string) (string, error) {
	elecSessionTime, err := fetchElecSessionTime(client, profileID)
	if err != nil {
		return "", err
	}
	return batchOperate(client, profileID, lessonID, elecSessionTime, true)
}

func DropLesson(client *http.Client, profileID, lessonID string) (string, error) {
	return batchOperate(client, profileID, lessonID, "undefined", false)
}

func batchOperate(client *http.Client, profileID, lessonID, elecSessionTime string, selectMode bool) (string, error) {
	operatorValue := lessonID + ":false"
	if selectMode {
		operatorValue = lessonID + ":true:0"
	}

	form := url.Values{
		"operator0": {operatorValue},
	}

	endpoint := fmt.Sprintf(
		"%s/stdElectCourse!batchOperator.action?profileId=%s&elecSessionTime=%s",
		baseURL,
		url.QueryEscape(profileID),
		url.QueryEscape(elecSessionTime),
	)

	req, err := http.NewRequest(http.MethodPost, endpoint, strings.NewReader(form.Encode()))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("X-Requested-With", "XMLHttpRequest")
	req.Header.Set("Referer", fmt.Sprintf("%s/stdElectCourse!defaultPage.action?electionProfile.id=%s", baseURL, url.QueryEscape(profileID)))

	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	return string(body), nil
}

func fetchElecSessionTime(client *http.Client, profileID string) (string, error) {
	resp, err := fetchDefaultPage(client, profileID)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	dateHeader := resp.Header.Get("Date")
	if dateHeader == "" {
		return "", errors.New("defaultPage 响应头缺少 Date")
	}

	serverTime, err := http.ParseTime(dateHeader)
	if err != nil {
		return "", fmt.Errorf("解析 Date 失败: %w", err)
	}
	return serverTime.In(localTZ()).Format("20060102150405"), nil
}

func fetchDefaultPage(client *http.Client, profileID string) (*http.Response, error) {
	endpoint := fmt.Sprintf("%s/stdElectCourse!defaultPage.action?electionProfile.id=%s", baseURL, url.QueryEscape(profileID))
	resp, err := client.Get(endpoint)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode == http.StatusFound {
		resp.Body.Close()
		return nil, errors.New("defaultPage 被重定向，登录态或通道状态可能无效")
	}
	return resp, nil
}

func ResolveLessonIDByName(mapping *LessonMappingCache, courseName string) (string, error) {
	key := normalizeIndexKey(courseName)
	ids := mapping.ByName[key]
	switch len(ids) {
	case 0:
		return "", fmt.Errorf("课程映射缓存中找不到课程名: %s", courseName)
	case 1:
		return ids[0], nil
	default:
		return "", fmt.Errorf("课程名 %s 对应多个 lessonID，请改用 --lesson，候选: %s", courseName, strings.Join(ids, ","))
	}
}

func SummarizeSelectionResponse(body string) string {
	body = strings.TrimSpace(body)
	if body == "" {
		return "空响应"
	}
	if match := reBodyMessage.FindStringSubmatch(body); len(match) > 1 {
		msg := cleanHTML(match[1])
		if msg != "" {
			return msg
		}
	}
	return cleanHTML(body)
}

func SelectionSucceeded(body string) bool {
	return strings.Contains(SummarizeSelectionResponse(body), "成功")
}
