package jwxt

import (
	"context"
	"encoding/base64"
	"encoding/json"
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

type ocrResult struct {
	answer string
	err    error
}

type LoginAutofillOptions struct {
	Name            string
	Password        string
	AutofillCaptcha bool
}

type persistentJar struct {
	inner   *cookiejar.Jar
	baseURL *url.URL
	store   map[string]*http.Cookie
}

func getCookiesViaBrowser(opts LoginAutofillOptions) ([]*http.Cookie, error) {
	browserOpts := append(chromedp.DefaultExecAllocatorOptions[:],
		chromedp.Flag("headless", false),
		chromedp.UserDataDir(chromeProfile),
	)
	allocCtx, cancel := chromedp.NewExecAllocator(context.Background(), browserOpts...)
	defer cancel()

	ctx, cancel := chromedp.NewContext(allocCtx)
	defer cancel()

	var cdpCookies []*network.Cookie

	err := chromedp.Run(ctx, chromedp.Navigate(baseURL+"/home.action"))
	if err != nil {
		return nil, err
	}

	if opts.Name != "" || opts.Password != "" || opts.AutofillCaptcha {
		if err := waitForCASLoginPage(ctx); err != nil {
			return nil, err
		}
		if err := performAutomatedLogin(ctx, opts); err != nil {
			return nil, err
		}
	} else {
		fmt.Println("请在浏览器中完成登录...")
	}

	err = chromedp.Run(ctx,
		chromedp.ActionFunc(waitForLoginSuccess),
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

func performAutomatedLogin(ctx context.Context, opts LoginAutofillOptions) error {
	var ocrCh chan ocrResult
	if opts.AutofillCaptcha {
		captchaImg, err := fetchCaptchaImage(ctx)
		if err != nil {
			return fmt.Errorf("验证码抓取失败: %w", err)
		}

		ocrCh = make(chan ocrResult, 1)
		go func() {
			ocrCh <- solveMathCaptcha(captchaImg)
		}()
	}

	if opts.Name != "" {
		if err := setInputValue(ctx, "#username", opts.Name); err != nil {
			return fmt.Errorf("用户名填写失败: %w", err)
		}
	}
	if opts.Password != "" {
		if err := setInputValue(ctx, "#password", opts.Password); err != nil {
			return fmt.Errorf("密码填写失败: %w", err)
		}
	}
	if opts.Name != "" || opts.Password != "" {
		fmt.Println("已自动填充账号密码")
	}

	if opts.AutofillCaptcha {
		ocrRes := <-ocrCh
		if ocrRes.err != nil {
			_ = chromedp.Run(ctx, chromedp.Click(`#captchaImg`))
			return fmt.Errorf("验证码识别失败: %w", ocrRes.err)
		}
		fmt.Printf("验证码识别结果: %s\n", ocrRes.answer)

		if err := setInputValue(ctx, "#validateCode", ocrRes.answer); err != nil {
			return fmt.Errorf("验证码填写失败: %w", err)
		}
	}

	if opts.Name != "" && opts.Password != "" && opts.AutofillCaptcha {
		if err := submitLoginForm(ctx); err != nil {
			return fmt.Errorf("提交失败: %w", err)
		}
		return nil
	}

	fmt.Println("已按参数完成自动填充，请手动补全剩余字段并提交")
	return nil
}

func fetchCaptchaImage(ctx context.Context) ([]byte, error) {
	var dataURL string
	script := `(() => {
		const img = document.querySelector('#captchaImg');
		if (!img || !img.complete || !img.naturalWidth || !img.naturalHeight) {
			return '';
		}
		const canvas = document.createElement('canvas');
		canvas.width = img.naturalWidth;
		canvas.height = img.naturalHeight;
		const context = canvas.getContext('2d');
		if (!context) {
			return '';
		}
		context.drawImage(img, 0, 0);
		return canvas.toDataURL('image/png');
	})()`

	if err := chromedp.Run(ctx, chromedp.Evaluate(script, &dataURL)); err != nil {
		return nil, err
	}
	if dataURL == "" {
		return nil, errors.New("captcha image is not ready")
	}
	prefix := "data:image/png;base64,"
	if !strings.HasPrefix(dataURL, prefix) {
		return nil, errors.New("unexpected captcha data url")
	}
	return base64.StdEncoding.DecodeString(strings.TrimPrefix(dataURL, prefix))
}

func setInputValue(ctx context.Context, preferredSelector, value string) error {
	preferredSelectorJSON, err := json.Marshal(preferredSelector)
	if err != nil {
		return err
	}
	valueJSON, err := json.Marshal(value)
	if err != nil {
		return err
	}

	script := fmt.Sprintf(`(() => {
		const usernameSelectors = [
			'input[name="username"]',
			'input[name="userName"]',
			'input[name="j_username"]',
			'input[id="username"]',
			'input[id="userName"]',
			'input[type="text"]'
		];
		const passwordSelectors = [
			'input[name="password"]',
			'input[name="pwd"]',
			'input[name="j_password"]',
			'input[id="password"]',
			'input[id="pwd"]',
			'input[type="password"]'
		];
		const captchaSelectors = [
			'input[name="validateCode"]',
			'input[id="validateCode"]'
		];

		function pick(selectors, preferred) {
			if (preferred) {
				const direct = document.querySelector(preferred);
				if (direct) return direct;
			}
			for (const selector of selectors) {
				const el = document.querySelector(selector);
				if (el) return el;
			}
			return null;
		}

		function setValue(el, value) {
			if (!el || value === '') return;
			el.focus();
			el.value = value;
			el.dispatchEvent(new Event('input', { bubbles: true }));
			el.dispatchEvent(new Event('change', { bubbles: true }));
			el.dispatchEvent(new Event('blur', { bubbles: true }));
		}

		let target = null;
		if (%s === '#username') target = pick(usernameSelectors, %s);
		if (%s === '#password') target = pick(passwordSelectors, %s);
		if (%s === '#validateCode') target = pick(captchaSelectors, %s);
		if (!target && %s) target = document.querySelector(%s);
		if (!target) return false;

		setValue(target, %s);
		return true;
	})()`,
		string(preferredSelectorJSON), string(preferredSelectorJSON),
		string(preferredSelectorJSON), string(preferredSelectorJSON),
		string(preferredSelectorJSON), string(preferredSelectorJSON),
		string(preferredSelectorJSON), string(preferredSelectorJSON),
		string(valueJSON),
	)

	var ok bool
	if err := chromedp.Run(ctx, chromedp.Evaluate(script, &ok)); err != nil {
		return err
	}
	if !ok {
		return errors.New("未找到登录表单")
	}
	return nil
}

func waitForCASLoginPage(ctx context.Context) error {
	for {
		var currentURL string
		if err := chromedp.Run(ctx, chromedp.Location(&currentURL)); err != nil {
			return err
		}
		var ready bool
		script := `(() => {
			const img = document.querySelector('#captchaImg');
			return !!document.querySelector('#fm1') &&
				!!document.querySelector('#username') &&
				!!document.querySelector('#password') &&
				!!document.querySelector('#validateCode') &&
				!!img &&
				!!img.complete &&
				!!img.naturalWidth &&
				!!img.naturalHeight;
		})()`
		if err := chromedp.Run(ctx, chromedp.Evaluate(script, &ready)); err != nil {
			return err
		}
		if strings.Contains(strings.ToLower(currentURL), "cas/login") && ready {
			return nil
		}
		time.Sleep(200 * time.Millisecond)
	}
}

func waitForLoginSuccess(ctx context.Context) error {
	for {
		var currentURL string
		if err := chromedp.Run(ctx, chromedp.Location(&currentURL)); err != nil {
			return err
		}
		if !strings.Contains(strings.ToLower(currentURL), "login") {
			fmt.Println("检测到登录成功")
			return nil
		}
		time.Sleep(200 * time.Millisecond)
	}
}

func submitLoginForm(ctx context.Context) error {
	script := `(() => {
		const form = document.querySelector('#fm1');
		const submitButton = document.querySelector('button[name="submit"]');
		if (!form) return false;
		if (typeof form.requestSubmit === 'function') {
			form.requestSubmit(submitButton || undefined);
			return true;
		}
		if (submitButton) {
			submitButton.click();
			return true;
		}
		form.submit();
		return true;
	})()`

	var ok bool
	if err := chromedp.Run(ctx, chromedp.Evaluate(script, &ok)); err != nil {
		return err
	}
	if !ok {
		return errors.New("未找到登录表单")
	}
	return nil
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
	return EnsureLoginWithOptions(LoginAutofillOptions{})
}

func EnsureLoginWithCredentials(username, password string) (*http.Client, []*http.Cookie, bool, error) {
	return EnsureLoginWithOptions(LoginAutofillOptions{
		Name:     username,
		Password: password,
	})
}

func EnsureLoginWithOptions(opts LoginAutofillOptions) (*http.Client, []*http.Cookie, bool, error) {
	client, cookies, err := ClientFromSavedLogin()
	if err == nil && IsSessionValid(client) {
		return client, cookies, false, nil
	}

	cookies, err = getCookiesViaBrowser(opts)
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
