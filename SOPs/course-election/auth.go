package main

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"strings"
	"time"

	"github.com/chromedp/cdproto/network"
	"github.com/chromedp/chromedp"
)

func getCookiesViaBrowser() ([]*http.Cookie, error) {
	opts := append(chromedp.DefaultExecAllocatorOptions[:],
		chromedp.Flag("headless", false),
	)
	allocCtx, cancel := chromedp.NewExecAllocator(context.Background(), opts...)
	defer cancel()

	ctx, cancel := chromedp.NewContext(allocCtx)
	defer cancel()

	var cdpCookies []*network.Cookie

	err := chromedp.Run(ctx,
		chromedp.Navigate(baseURL+"/home.action"),
		chromedp.ActionFunc(func(ctx context.Context) error {
			fmt.Println("请在浏览器中完成登录...")
			for {
				var currentURL string
				if err := chromedp.Location(&currentURL).Do(ctx); err != nil {
					return err
				}
				if !strings.Contains(strings.ToLower(currentURL), "login") {
					fmt.Println("检测到登录成功")
					return nil
				}
				time.Sleep(500 * time.Millisecond)
			}
		}),
		chromedp.ActionFunc(func(ctx context.Context) error {
			var err error
			cdpCookies, err = network.GetCookies().Do(ctx)
			return err
		}),
	)
	if err != nil {
		return nil, err
	}

	cookies := make([]*http.Cookie, 0, len(cdpCookies))
	for _, c := range cdpCookies {
		cookie := &http.Cookie{
			Name:     c.Name,
			Value:    c.Value,
			Domain:   c.Domain,
			Path:     c.Path,
			HttpOnly: c.HTTPOnly,
			Secure:   c.Secure,
		}
		if c.Expires > 0 {
			cookie.Expires = time.Unix(int64(c.Expires), 0)
		}
		cookies = append(cookies, cookie)
	}
	return cookies, nil
}

func buildClient(cookies []*http.Cookie) *http.Client {
	jar, _ := cookiejar.New(nil)
	u, _ := url.Parse(baseURL)
	jar.SetCookies(u, cookies)

	return &http.Client{
		Jar:     jar,
		Timeout: time.Duration(defaultTimeout) * time.Second,
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}
}

func clientFromSavedLogin() (*http.Client, []*http.Cookie, error) {
	cookies := loadCookies()
	if len(cookies) == 0 {
		return nil, nil, errors.New("未找到可用 Cookie")
	}
	return buildClient(cookies), cookies, nil
}

func ensureLogin() (*http.Client, []*http.Cookie, bool, error) {
	client, cookies, err := clientFromSavedLogin()
	if err == nil && isSessionValid(client) {
		return client, cookies, false, nil
	}

	cookies, err = getCookiesViaBrowser()
	if err != nil {
		return nil, nil, false, fmt.Errorf("浏览器登录失败: %w", err)
	}
	if len(cookies) == 0 {
		return nil, nil, false, errors.New("未获取到任何 Cookie")
	}
	if err := saveCookies(cookies); err != nil {
		return nil, nil, false, fmt.Errorf("保存 Cookie 失败: %w", err)
	}
	client = buildClient(cookies)
	return client, cookies, true, nil
}

func isSessionValid(client *http.Client) bool {
	resp, err := client.Get(baseURL + "/stdElectCourse!defaultPage.action")
	if err != nil {
		return false
	}
	defer resp.Body.Close()
	return resp.StatusCode != http.StatusFound
}
