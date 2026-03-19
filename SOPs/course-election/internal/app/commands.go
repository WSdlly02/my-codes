package app

import (
	"errors"
	"fmt"
	"time"

	"course-election/internal/jwxt"
)

func runStatus(args []string) error {
	fs := newFlagSet("status")
	if err := fs.Parse(args); err != nil {
		return err
	}

	client, cookies, err := jwxt.ClientFromSavedLogin()
	if err != nil {
		fmt.Println("cookieFile: missing")
		return nil
	}

	fmt.Printf("cookieFile: present (%d cookies)\n", len(cookies))
	for _, line := range jwxt.CookieSummary(cookies) {
		fmt.Println(line)
	}
	fmt.Printf("sessionValid: %t\n", jwxt.IsSessionValid(client))
	fmt.Printf("channelsCache: %t\n", jwxt.CacheExists(jwxt.ChannelsCachePath()))
	return nil
}

func runQuery(args []string) error {
	fs := newFlagSet("query")
	profileID := fs.String("profile", "", "选课通道 profileID")
	nameFilter := fs.String("name", "", "课程名称关键词")
	lessonIDFilter := fs.String("lesson-id", "", "课程ID")
	codeFilter := fs.String("code", "", "课程号")
	if err := fs.Parse(args); err != nil {
		return err
	}

	client, _, err := jwxt.ClientFromSavedLogin()
	sessionValid := false
	if err == nil && jwxt.IsSessionValid(client) {
		sessionValid = true
	} else {
		client = nil
	}

	if *profileID == "" {
		var channels []jwxt.ChannelEntry
		if sessionValid {
			channels, err = jwxt.LoadOrFetchChannels(client)
		} else {
			cache, cacheErr := jwxt.LoadChannelCache()
			if cacheErr != nil {
				return errors.New("当前 Cookie 无效，且本地没有通道缓存，请先执行 warmup")
			}
			channels = cache.Channels
		}
		if err != nil {
			return err
		}
		for _, ch := range channels {
			state := "未开放"
			if ch.Opened {
				state = "已开放"
			}
			line := fmt.Sprintf("[%s] %s | profile=%s | %s", ch.RoundNo, ch.Name, ch.ProfileID, state)
			if ch.OpenTime != "" {
				line += " | " + ch.OpenTime
			}
			fmt.Println(line)
		}
		return nil
	}

	mapping, counts, countsFromCache, err := jwxt.QueryCourseData(client, *profileID)
	if err != nil {
		return err
	}

	filter := lessonQueryFilter{
		Name:     *nameFilter,
		LessonID: *lessonIDFilter,
		Code:     *codeFilter,
	}
	entries := buildLessonDisplayEntries(mapping, counts, filter)
	for i, entry := range entries {
		fmt.Print(formatLessonDisplayEntry(i+1, entry))
	}
	fmt.Printf("共 %d 门课程\n", len(entries))
	if countsFromCache && counts != nil {
		fmt.Printf("警告：容量数据获取失败，使用本地副本：%s，数据可能不准确\n", counts.FetchedAt.Format("2006-01-02 15:04:05"))
	}
	return nil
}

func runWarmup(args []string) error {
	fs := newFlagSet("warmup")
	if err := fs.Parse(args); err != nil {
		return err
	}

	client, _, relogin, err := jwxt.EnsureLogin()
	if err != nil {
		return err
	}

	channels, err := jwxt.FetchAndCacheChannels(client)
	if err != nil {
		return err
	}

	if relogin {
		fmt.Println("已重新登录并刷新 Cookie")
	} else {
		fmt.Println("复用现有 Cookie")
	}
	fmt.Printf("已缓存 %d 个选课通道到 %s\n", len(channels), jwxt.ChannelsCachePath())
	return nil
}

func runSelect(args []string) error {
	fs := newFlagSet("select")
	profileID := fs.String("profile", "", "选课通道 profileID")
	lessonID := fs.String("lesson-id", "", "课程ID")
	courseName := fs.String("name", "", "课程名称，将从映射缓存里解析到课程ID")
	retry := fs.Int("retry", 1, "最大尝试次数，0 表示无限重试")
	interval := fs.Duration("interval", 500*time.Millisecond, "重试间隔")
	if err := fs.Parse(args); err != nil {
		return err
	}

	if *profileID == "" {
		return errors.New("缺少 --profile")
	}
	if (*lessonID == "" && *courseName == "") || (*lessonID != "" && *courseName != "") {
		return errors.New("必须二选一传入 --lesson-id 或 --name")
	}

	client, _, err := jwxt.ClientFromSavedLogin()
	if err != nil {
		return err
	}
	if !jwxt.IsSessionValid(client) {
		return errors.New("当前 Cookie 无效，请先执行 warmup")
	}

	resolvedLessonID := *lessonID
	if resolvedLessonID == "" {
		mapping, err := jwxt.LoadLessonMappingCache(*profileID)
		if err != nil {
			return fmt.Errorf("读取课程映射缓存失败: %w", err)
		}
		resolvedLessonID, err = jwxt.ResolveLessonIDByName(mapping, *courseName)
		if err != nil {
			return err
		}
		fmt.Printf("resolved lessonID: %s\n", resolvedLessonID)
	}

	attempt := 0
	for {
		attempt++
		body, err := jwxt.SelectLesson(client, *profileID, resolvedLessonID)
		if err != nil {
			fmt.Printf("[%d] 请求失败: %v\n", attempt, err)
		} else {
			fmt.Printf("[%d] %s\n", attempt, jwxt.SummarizeSelectionResponse(body))
			if jwxt.SelectionSucceeded(body) {
				return nil
			}
		}

		if *retry > 0 && attempt >= *retry {
			return errors.New("达到最大重试次数，仍未成功")
		}
		time.Sleep(*interval)
	}
}

func runDrop(args []string) error {
	fs := newFlagSet("drop")
	profileID := fs.String("profile", "", "选课通道 profileID")
	lessonID := fs.String("lesson-id", "", "课程ID")
	courseName := fs.String("name", "", "课程名称，将从映射缓存里解析到课程ID")
	retry := fs.Int("retry", 1, "最大尝试次数，0 表示无限重试")
	interval := fs.Duration("interval", 500*time.Millisecond, "重试间隔")
	if err := fs.Parse(args); err != nil {
		return err
	}

	if *profileID == "" {
		return errors.New("缺少 --profile")
	}
	if (*lessonID == "" && *courseName == "") || (*lessonID != "" && *courseName != "") {
		return errors.New("必须二选一传入 --lesson-id 或 --name")
	}

	client, _, err := jwxt.ClientFromSavedLogin()
	if err != nil {
		return err
	}
	if !jwxt.IsSessionValid(client) {
		return errors.New("当前 Cookie 无效，请先执行 warmup")
	}

	resolvedLessonID := *lessonID
	if resolvedLessonID == "" {
		mapping, err := jwxt.LoadLessonMappingCache(*profileID)
		if err != nil {
			return fmt.Errorf("读取课程映射缓存失败: %w", err)
		}
		resolvedLessonID, err = jwxt.ResolveLessonIDByName(mapping, *courseName)
		if err != nil {
			return err
		}
		fmt.Printf("resolved lessonID: %s\n", resolvedLessonID)
	}

	attempt := 0
	for {
		attempt++
		body, err := jwxt.DropLesson(client, *profileID, resolvedLessonID)
		if err != nil {
			fmt.Printf("[%d] 请求失败: %v\n", attempt, err)
		} else {
			fmt.Printf("[%d] %s\n", attempt, jwxt.SummarizeSelectionResponse(body))
			if jwxt.SelectionSucceeded(body) {
				return nil
			}
		}

		if *retry > 0 && attempt >= *retry {
			return errors.New("达到最大重试次数，仍未成功")
		}
		time.Sleep(*interval)
	}
}
