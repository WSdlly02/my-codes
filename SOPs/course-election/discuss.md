# 那么如果我想使用go的话，有没有类似方案呢？选课系统的代码相当糟糕，每个人请求都会返回7万多行的课程列表😂，python反序列化是不是性能不够？

Go 完全可以做到，而且性能确实更好。不过先说一个重要判断：**Python 反序列化 7 万行 JSON 不是瓶颈**，网络传输时间远大于解析时间，真正的性能优势在 Go 的并发模型和更低的内存占用。

## 浏览器自动化：chromedp vs playwright-go

Go 有两个主要选择：


|  | **chromedp** | **playwright-go** |
| :-- | :-- | :-- |
| 维护方 | 社区 | 官方 Playwright 绑定 [^9_1] |
| 依赖 | 只需本机有 Chrome | 需要额外安装浏览器驱动 |
| API 风格 | 底层 CDP，verbose | 高层封装，接近 Python 版 |
| 推荐场景 | 轻量任务，不想额外依赖 | 与 Python 版逻辑一致，迁移方便 |

对你的场景（只需提取 Cookie，逻辑简单），**chromedp 更合适**，无需额外依赖。[^9_2]

## 完整 Go 实现

```go
package main

import (
    "context"
    "encoding/json"
    "fmt"
    "io"
    "net/http"
    "net/http/cookiejar"
    "net/url"
    "os"
    "strings"
    "time"

    "github.com/chromedp/cdproto/network"
    "github.com/chromedp/chromedp"
    "github.com/bytedance/sonic"   // 高性能 JSON
)

const BASE = "https://jwxt.shmtu.edu.cn/shmtu"

// ── 1. 用 chromedp 提取 Cookie ─────────────────────────

func getCookiesViaBrowser() ([]*http.Cookie, error) {
    // 有头模式，让你手动操作
    opts := append(chromedp.DefaultExecAllocatorOptions[:],
        chromedp.Flag("headless", false),
    )
    allocCtx, cancel := chromedp.NewExecAllocator(context.Background(), opts...)
    defer cancel()

    ctx, cancel := chromedp.NewContext(allocCtx)
    defer cancel()

    var cdpCookies []*network.Cookie

    err := chromedp.Run(ctx,
        chromedp.Navigate(BASE+"/login"),

        // 等你手动完成 reCAPTCHA 和登录，最多等 2 分钟
        chromedp.ActionFunc(func(ctx context.Context) error {
            fmt.Println("请在浏览器中完成登录...")
            for {
                var currentURL string
                if err := chromedp.Location(&currentURL).Do(ctx); err != nil {
                    return err
                }
                if !strings.Contains(currentURL, "login") {
                    fmt.Println("检测到登录成功！")
                    return nil
                }
                time.Sleep(500 * time.Millisecond)
            }
        }),

        // 提取所有 Cookie
        chromedp.ActionFunc(func(ctx context.Context) error {
            var err error
            cdpCookies, err = network.GetCookies().Do(ctx)
            return err
        }),
    )
    if err != nil {
        return nil, err
    }

    // 转换为标准 http.Cookie
    var cookies []*http.Cookie
    for _, c := range cdpCookies {
        cookies = append(cookies, &http.Cookie{
            Name:  c.Name,
            Value: c.Value,
        })
    }
    return cookies, nil
}

// ── 2. 构建带 Cookie 的 HTTP Client ───────────────────

func buildClient(cookies []*http.Cookie) *http.Client {
    jar, _ := cookiejar.New(nil)
    u, _ := url.Parse(BASE)
    jar.SetCookies(u, cookies)
    return &http.Client{
        Jar:     jar,
        Timeout: 10 * time.Second,
        // 不自动跟随重定向，方便检测 Session 失效
        CheckRedirect: func(req *http.Request, via []*http.Request) error {
            return http.ErrUseLastResponse
        },
    }
}

// ── 3. Cookie 持久化 ───────────────────────────────────

type savedCookies struct {
    Cookies []struct{ Name, Value string }
}

func saveCookies(cookies []*http.Cookie) {
    sc := savedCookies{}
    for _, c := range cookies {
        sc.Cookies = append(sc.Cookies, struct{ Name, Value string }{c.Name, c.Value})
    }
    data, _ := sonic.Marshal(sc)
    os.WriteFile("cookies.json", data, 0600)
}

func loadCookies() []*http.Cookie {
    data, err := os.ReadFile("cookies.json")
    if err != nil {
        return nil
    }
    var sc savedCookies
    if err := sonic.Unmarshal(data, &sc); err != nil {
        return nil
    }
    var cookies []*http.Cookie
    for _, c := range sc.Cookies {
        cookies = append(cookies, &http.Cookie{Name: c.Name, Value: c.Value})
    }
    return cookies
}

// ── 4. 检测 Session 是否有效 ──────────────────────────

func isSessionValid(client *http.Client) bool {
    resp, err := client.Get(BASE + "/stdElectCourse!defaultPage.action")
    if err != nil {
        return false
    }
    resp.Body.Close()
    return resp.StatusCode != 302   // 302 = 被踢回登录页
}

// ── 5. 选课 ───────────────────────────────────────────

type CourseSelector struct {
    client    *http.Client
    profileID string
}

func (cs *CourseSelector) getToken() (string, error) {
    resp, err := cs.client.Get(
        fmt.Sprintf("%s/stdElectCourse!defaultPage.action?electionProfile.id=%s", BASE, cs.profileID),
    )
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()
    body, _ := io.ReadAll(resp.Body)

    // 从 HTML 中提取 elecSessionTime
    // 根据实际页面结构调整解析逻辑
    start := strings.Index(string(body), "elecSessionTime")
    if start == -1 {
        return "", fmt.Errorf("找不到 elecSessionTime")
    }
    // 提取数字值（简化版，按实际格式调整）
    sub := string(body)[start : start+50]
    var token string
    fmt.Sscanf(sub, "elecSessionTime=%s", &token)
    return strings.Trim(token, `"';`), nil
}

func (cs *CourseSelector) Select(courseID string) (string, error) {
    token, err := cs.getToken()
    if err != nil {
        return "", err
    }

    form := url.Values{"operator0": {courseID + ":true:0"}}
    req, _ := http.NewRequest("POST",
        fmt.Sprintf("%s/stdElectCourse!batchOperator.action?profileId=%s&elecSessionTime=%s",
            BASE, cs.profileID, token),
        strings.NewReader(form.Encode()),
    )
    req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
    req.Header.Set("X-Requested-With", "XMLHttpRequest")

    resp, err := cs.client.Do(req)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()
    body, _ := io.ReadAll(resp.Body)
    return string(body), nil
}
```


## 关于 JSON 性能

标准库 `encoding/json` 已经够用，但如果 7 万行数据让你介意，可以用 `bytedance/sonic`，它基于 SIMD 指令，反序列化速度是标准库的 **5 倍**。[^9_3]

```go
// 替换标准库，API 完全一致
import "github.com/bytedance/sonic"

// 原来：json.Unmarshal(data, &v)
// 改为：
sonic.Unmarshal(data, &v)
```

不过说实话，对于一次性加载课程列表（预热阶段），两者差距在毫秒级，实际抢课时的网络延迟和 `elecSessionTime` 获取才是真正的时间瓶颈。


---

# 好的，现在给出流程图，并指出哪些操作可以在未开放时间段完成、哪些操作不能。对于人类而言，操作流程是：1.访问https://jwxt.shmtu.edu.cn/shmtu/home.action，被重定向至recaptcha页面，然后重定向回来：https://jwxt.shmtu.edu.cn/shmtu/home.action?ticket=ST-4802-KRDou3tW78uJEF8ZL-EyuMnGxww-cas6，此时获得了cookie. 2.访问https://jwxt.shmtu.edu.cn/shmtu/stdElectCourse.action，获得选课轮次，对于开放的选课频道可以获得它的profileID，但未开放的是无法获取的，因此此处可能需要在开放时间段才能完成。3.获取到https://jwxt.shmtu.edu.cn/shmtu/stdElectCourse!defaultPage.action?electionProfile.id=2936 4.获取

https://jwxt.shmtu.edu.cn/shmtu/stdElectCourse!queryStdCount.action?profileId=2936获取每个课程的容量信息、
https://jwxt.shmtu.edu.cn/shmtu/stdElectCourse!data.action?profileId=2936获取每个课程的映射信息 5. post
https://jwxt.shmtu.edu.cn/shmtu/stdElectCourse!batchOperator.action?profileId=2936\&elecSessionTime=20260319123329来进行选课，注意此时elecSessionTime就是步骤3 response haders里的Date: Thu, 19 Mar 2026 04:33:29 GMT，这就很方便了

图表渲染暂时无法使用，下面用结构化文字呈现完整流程，并补充 `elecSessionTime` 的重要发现。

***

## 完整选课流程

```
════════════════════════════════════════════════════
🟢 选课开放前可预热完成（可缓存）
════════════════════════════════════════════════════

① [人工] 访问 home.action
        → CAS 跳转 → reCAPTCHA → ticket 回调
        → 获得 Cookie: JSESSIONID + srv_id
        ✅ 可写入文件缓存，有效期约 30 分钟

② [可选预热] 若 profileID 已知（上次记录/通知公告）
        GET stdElectCourse!defaultPage.action
             ?electionProfile.id=2936
        GET stdElectCourse!data.action?profileId=2936
             → 课程ID ↔ 名称映射（✅ 可缓存）
        GET stdElectCourse!queryStdCount.action?profileId=2936
             → 各课程容量（✅ 可缓存，但开放前数据可能不准）

════════════════════════════════════════════════════
🔴 必须等选课开放后才能完成
════════════════════════════════════════════════════

③ GET stdElectCourse.action
        → 解析页面，获取当前开放的轮次列表
        → 拿到 profileID（未开放时此接口不返回该轮次）

④ [若未预热] 执行上方②的三个请求

⑤ ⚡ 关键步骤：GET defaultPage?electionProfile.id=2936
        → 不需要解析 HTML！
        → 直接读 Response Header: Date
           "Thu, 19 Mar 2026 04:33:29 GMT"
           转换为北京时间 → 格式化为 "20260319123329"
           即为 elecSessionTime ✅

⑥ POST batchOperator.action
        ?profileId=2936&elecSessionTime=20260319123329
        Body: operator0=244378:true:0
        → 返回"选课成功"或失败原因
```


***

## ⑤ 步的关键发现：elecSessionTime 就是服务器时间

你发现了一个很重要的规律：不需要解析 HTML，直接用 HTTP 响应头的 `Date` 字段即可。在 Go 里：

```go
resp, _ := client.Get(defaultPageURL)

// 解析 RFC1123 格式的 Date header
dateStr := resp.Header.Get("Date")
t, _ := time.Parse(time.RFC1123, dateStr)

// 转换为北京时间（UTC+8）
loc, _ := time.LoadLocation("Asia/Shanghai")
t = t.In(loc)

// 格式化为服务器期望的格式
elecSessionTime := t.Format("20060102150405")  // Go 的时间格式模板
```


***

## 哪些步骤可以并行？

```
开放瞬间触发：
    ③ GET stdElectCourse.action          ← 串行（要先拿 profileID）
         ↓
    ④ data + queryStdCount               ← 可并行（2个 goroutine）
         ↓
    ⑤⑥ GET defaultPage → 立即 POST      ← 串行（token 依赖响应头）
```

步骤 ④ 的两个请求可以用 `sync.WaitGroup` 并发，但 ⑤→⑥ 必须严格串行，中间不能有任何额外延迟，这是整个流程里最需要优化的热路径。

