package main

import (
	"fmt"
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
