package main

import (
	"errors"
	"fmt"
	"time"
)

func runStatus(args []string) error {
	fs := newFlagSet("status")
	if err := fs.Parse(args); err != nil {
		return err
	}

	client, cookies, err := clientFromSavedLogin()
	if err != nil {
		fmt.Println("cookieFile: missing")
		return nil
	}

	fmt.Printf("cookieFile: present (%d cookies)\n", len(cookies))
	for _, line := range cookieSummary(cookies) {
		fmt.Println(line)
	}
	fmt.Printf("sessionValid: %t\n", isSessionValid(client))
	fmt.Printf("channelsCache: %t\n", cacheExists(channelsCachePath()))
	return nil
}

func runWarmup(args []string) error {
	fs := newFlagSet("warmup")
	if err := fs.Parse(args); err != nil {
		return err
	}

	client, _, relogin, err := ensureLogin()
	if err != nil {
		return err
	}

	channels, err := fetchAndCacheChannels(client)
	if err != nil {
		return err
	}

	if relogin {
		fmt.Println("已重新登录并刷新 Cookie")
	} else {
		fmt.Println("复用现有 Cookie")
	}
	fmt.Printf("已缓存 %d 个选课通道到 %s\n", len(channels), channelsCachePath())
	return nil
}

func runSelect(args []string) error {
	fs := newFlagSet("select")
	profileID := fs.String("profile", "", "选课通道 profileID")
	lessonID := fs.String("lesson", "", "课程 lessonID")
	courseName := fs.String("name", "", "课程名称，将从映射缓存里解析到 lessonID")
	retry := fs.Int("retry", 1, "最大尝试次数，0 表示无限重试")
	interval := fs.Duration("interval", 500*time.Millisecond, "重试间隔")
	if err := fs.Parse(args); err != nil {
		return err
	}

	if *profileID == "" {
		return errors.New("缺少 --profile")
	}
	if (*lessonID == "" && *courseName == "") || (*lessonID != "" && *courseName != "") {
		return errors.New("必须二选一传入 --lesson 或 --name")
	}

	client, _, err := clientFromSavedLogin()
	if err != nil {
		return err
	}
	if !isSessionValid(client) {
		return errors.New("当前 Cookie 无效，请先执行 warmup")
	}

	resolvedLessonID := *lessonID
	if resolvedLessonID == "" {
		mapping, err := loadLessonMappingCache(*profileID)
		if err != nil {
			return fmt.Errorf("读取课程映射缓存失败: %w", err)
		}
		resolvedLessonID, err = resolveLessonIDByName(mapping, *courseName)
		if err != nil {
			return err
		}
		fmt.Printf("resolved lessonID: %s\n", resolvedLessonID)
	}

	attempt := 0
	for {
		attempt++
		body, err := selectLesson(client, *profileID, resolvedLessonID)
		if err != nil {
			fmt.Printf("[%d] 请求失败: %v\n", attempt, err)
		} else {
			fmt.Printf("[%d] %s\n", attempt, summarizeSelectionResponse(body))
			if selectionSucceeded(body) {
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
	lessonID := fs.String("lesson", "", "课程 lessonID")
	courseName := fs.String("name", "", "课程名称，将从映射缓存里解析到 lessonID")
	retry := fs.Int("retry", 1, "最大尝试次数，0 表示无限重试")
	interval := fs.Duration("interval", 500*time.Millisecond, "重试间隔")
	if err := fs.Parse(args); err != nil {
		return err
	}

	if *profileID == "" {
		return errors.New("缺少 --profile")
	}
	if (*lessonID == "" && *courseName == "") || (*lessonID != "" && *courseName != "") {
		return errors.New("必须二选一传入 --lesson 或 --name")
	}

	client, _, err := clientFromSavedLogin()
	if err != nil {
		return err
	}
	if !isSessionValid(client) {
		return errors.New("当前 Cookie 无效，请先执行 warmup")
	}

	resolvedLessonID := *lessonID
	if resolvedLessonID == "" {
		mapping, err := loadLessonMappingCache(*profileID)
		if err != nil {
			return fmt.Errorf("读取课程映射缓存失败: %w", err)
		}
		resolvedLessonID, err = resolveLessonIDByName(mapping, *courseName)
		if err != nil {
			return err
		}
		fmt.Printf("resolved lessonID: %s\n", resolvedLessonID)
	}

	attempt := 0
	for {
		attempt++
		body, err := dropLesson(client, *profileID, resolvedLessonID)
		if err != nil {
			fmt.Printf("[%d] 请求失败: %v\n", attempt, err)
		} else {
			fmt.Printf("[%d] %s\n", attempt, summarizeSelectionResponse(body))
			if selectionSucceeded(body) {
				return nil
			}
		}

		if *retry > 0 && attempt >= *retry {
			return errors.New("达到最大重试次数，仍未成功")
		}
		time.Sleep(*interval)
	}
}
