package jwxt

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"strings"
	"sync"
	"time"

	"github.com/chromedp/cdproto/network"
	"github.com/chromedp/chromedp"
)

var cookieFileMu sync.Mutex

type persistentJar struct {
	inner   *cookiejar.Jar
	baseURL *url.URL
	store   map[string]*http.Cookie
}

func getCookiesViaBrowser() ([]*http.Cookie, error) {
	opts := append(chromedp.DefaultExecAllocatorOptions[:],
		chromedp.Flag("headless", false),
		chromedp.UserDataDir(chromeProfile),
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
	u, _ := url.Parse(baseURL)
	jar, _ := newPersistentJar(u, cookies)

	return &http.Client{
		Jar:     jar,
		Timeout: time.Duration(defaultTimeout) * time.Second,
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}
}

func newPersistentJar(base *url.URL, cookies []*http.Cookie) (*persistentJar, error) {
	inner, err := cookiejar.New(nil)
	if err != nil {
		return nil, err
	}

	jar := &persistentJar{
		inner:   inner,
		baseURL: base,
		store:   map[string]*http.Cookie{},
	}
	if len(cookies) > 0 {
		jar.SetCookies(base, cookies)
	}
	return jar, nil
}

func (j *persistentJar) SetCookies(u *url.URL, cookies []*http.Cookie) {
	j.inner.SetCookies(u, cookies)
	if j.baseURL == nil {
		return
	}

	cookieFileMu.Lock()
	defer cookieFileMu.Unlock()
	for _, cookie := range cookies {
		key := cookieKey(cookie)
		if cookie.MaxAge < 0 || (!cookie.Expires.IsZero() && cookie.Expires.Before(now())) {
			delete(j.store, key)
			continue
		}
		j.store[key] = cloneCookie(cookie)
	}
	_ = saveCookies(j.snapshot())
}

func (j *persistentJar) Cookies(u *url.URL) []*http.Cookie {
	return j.inner.Cookies(u)
}

func (j *persistentJar) snapshot() []*http.Cookie {
	cookies := make([]*http.Cookie, 0, len(j.store))
	for _, cookie := range j.store {
		cookies = append(cookies, cloneCookie(cookie))
	}
	return cookies
}

func cookieKey(cookie *http.Cookie) string {
	return strings.Join([]string{cookie.Name, cookie.Domain, cookie.Path}, "\x00")
}

func cloneCookie(cookie *http.Cookie) *http.Cookie {
	if cookie == nil {
		return nil
	}
	clone := *cookie
	return &clone
}

func ClientFromSavedLogin() (*http.Client, []*http.Cookie, error) {
	cookies := loadCookies()
	if len(cookies) == 0 {
		return nil, nil, errors.New("未找到可用 Cookie")
	}
	return buildClient(cookies), cookies, nil
}

func EnsureLogin() (*http.Client, []*http.Cookie, bool, error) {
	client, cookies, err := ClientFromSavedLogin()
	if err == nil && IsSessionValid(client) {
		return client, cookies, false, nil
	}

	cookies, err = getCookiesViaBrowser()
	if err != nil {
		return nil, nil, false, fmt.Errorf("浏览器登录失败: %w", err)
	}
	if len(cookies) == 0 {
		return nil, nil, false, errors.New("未获取到任何 Cookie")
	}
	client = buildClient(cookies)
	return client, cookies, true, nil
}

func IsSessionValid(client *http.Client) bool {
	resp, err := client.Get(baseURL + "/stdElectCourse.action")
	if err != nil {
		return false
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusFound {
		location := strings.ToLower(resp.Header.Get("Location"))
		if strings.Contains(location, "login") || strings.Contains(location, "cas") || strings.Contains(location, "sso") {
			return false
		}
	}
	return resp.StatusCode == http.StatusOK
}
