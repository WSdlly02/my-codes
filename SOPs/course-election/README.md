# course-election

上海海事大学教务系统选课 REPL。程序使用 Tokio 与长期存活的 `reqwest::Client`，通过纯 HTTP CAS 登录，不依赖浏览器。

## 运行

```bash
cargo run --release
```

启动后直接进入唯一交互入口：

```text
course-election>
```

当前会话支持 `↑/↓` 浏览历史命令、左右/Home/End 编辑、`Ctrl-C` 取消输入和 `Ctrl-D` 退出；历史不会写入磁盘。

## 登录

```text
course-election> login 202410000000
密码: ********
```

密码不会显示或写入文件。程序会：

1. 请求教务系统，按实际重定向直接或经 `ng.shmtu.edu.cn` 网关进入 CAS；
2. 解析当前页面的 `execution`；
3. 请求带 `captchaToken` 的验证码；
4. 使用本地 Ollama 识别算术验证码；
5. 提交 CAS 表单，核对 ticket 回调与本次 service 一致，跟随教务或网关回调链；网关后的教务 CAS 认证复用同一 Cookie Jar 自动完成，不重复提交密码；
6. 验证教务系统 Session 并保存 JWXT Cookie。

验证码错误最多自动刷新三次；密码错误立即停止。默认 OCR 服务为：

每次识别都会把验证码以权限 `0600` 保存到系统临时目录，并打印图片路径与最终整数结果；CAS 异常响应也会保存为临时 HTML 供排查。

```text
http://10.144.144.64:11434/api/generate
model=qwen3-vl:8b-instruct
```

## 常用流程

```text
course-election> channels
course-election> profile 2936
course-election> refresh
course-election> find 海事法
course-election> target 242153
```

- `channels`：在线刷新选课轮次。
- `profile <id>`：选择轮次；切换轮次会清除当前 target。
- `refresh`：显式联网更新课程列表和容量，即使已有课程缓存也重新获取；容量获取失败时可回退到旧容量缓存。
- `find [--selected] [--id ID|--code CODE|名称]`：只读本地课程和容量缓存，按名称、ID、课程号过滤，不自动联网；课程缓存缺失时提示先执行 `refresh`。容量为上次缓存值，可能过时；缺失时仍可搜索课程。`--selected` 是例外，会额外联网获取当前已选状态。
- `target <lesson-id|完整课程名>`：固定热路径目标；完整课程名必须唯一匹配。
- `status`：查看登录、profile、target 和缓存状态。

## 连接预热与选课

手动待命：

```text
course-election> arm
正在预热；按 Enter 或输入 fire 触发，输入 cancel 取消
```

待命期间立即预热，完成后等待 10 秒再保活。仅访问入口 `stdElectCourse.action`，不访问会更新服务端选课 token 的 `defaultPage`。预热请求最多 2 秒、不重试，失败只提示；输入和定时触发不等待预热，触发时取消本地未完成的预热（不保证服务器已经停止处理）。触发后读取 `defaultPage` HTML 隐藏字段 `elecSessionTime`，随后 POST，尽量复用连接池中的连接。

定时触发使用 RFC3339 时间：

```text
course-election> arm 2026-09-01T12:00:00+08:00
```

程序在 T-5 秒开始预热入口连接，并在目标时间发起一次选课。打印预热耗时和定时触发偏差；偏差依据本地时钟，不代表请求到达服务器的时间。

直接选课及重试：

```text
course-election> fire
course-election> fire 20 500
course-election> fire 0 500
```

参数依次为尝试次数和间隔毫秒；次数 `0` 表示无限。一轮 `fire` 内复用页面 token：

```text
首次：defaultPage 完整 HTML → elecSessionTime → batchOperator
普通失败后的重试：同一 elecSessionTime → batchOperator
明确“同时打开多个选课页面”拒绝：清除 token → 下次允许的尝试重新 GET 后 POST
```

token 只来自 HTML，不从 HTTP Date 推算；字段缺失或异常时不提交。token 不跨 `fire` 命令缓存，避免 `refresh` / `find --selected` 等操作更新页面后误用旧值。失效刷新不增加隐式 POST 或额外尝试额度；网络异常仍不能证明服务端未执行，不自动判定成功。

获取 token 时打印 `defaultPage` 完整响应及解析耗时（含 GET 重试），提交时打印 POST 完整响应及本次选课总耗时。Cookie 在内存中即时更新，整轮成功或耗尽次数后保存，正常退出时也保存。强制终止进程不保证落盘。重试间隔仍从一次尝试结束后计算，POST 保持串行。

本地慢服务器回归测试：`python3 tests/arm_prewarm.py ./course-election`，使用隔离目录和本地代理，不访问真实选课接口。

选课链路回归：`python3 tests/selection_path.py target/debug/course-election`（先 `cargo build`，需 Python 3 与 `openssl`）。使用临时证书、本地 TLS 模拟服务器和隔离目录，验证 HTML token、复用、失效刷新、尝试额度、缺失字段及退课；不连接真实教务系统。

捡漏（事件驱动）：

```text
course-election> watch
course-election> watch 5 1800
course-election> watch 3 600 --dry-run
```

`watch` 需要先 `profile <id>` 和 `target <lesson-id>`。启动时访问一次 `defaultPage`，
建立选课上下文并取得 token，再只读地轮询名额快照
`stdElectCourse!queryStdCount.action?profileId=<id>`（页面自身每 20 秒刷新同一接口）。
只有目标出现空位（`lc - sc - wc > 0`）时才复用 token 提交 `batchOperator`。
命中即停止；被他人抢先则继续等待。查询的临时网络错误、429/可重试 5xx 等即使耗尽底层 GET 重试，
仍会打印原因后等待下一轮；明确认证/权限错误、重定向或无法解析的响应退出，不无限掩盖协议变化。

参数依次为轮询间隔秒（默认 `5`）和最长等待秒（默认 `1800`，`0` 表示不限时）；`--dry-run`
只观察不出手。名额无变化时每 12 轮打印一次心跳，避免刷屏。

总期限从初始化上下文前开始计时，查询和睡眠受剩余时间限制；过期后不发起新 POST，
包括 token 失效后重新获取页面耗尽期限的情况。已发出的 POST 仍等待结果（受 HTTP 请求超时限制），
不因为监视期限到达而取消并丢失结果。持续有空位但提交失败时，每轮仍会尝试一次。

三个命令共享命令内的 `ElectionContext`，负责 token 初始化、复用、失效和提交截止时间；
调度方式各自保留，不跨命令缓存上下文：

| 命令 | 何时访问 defaultPage | 何时提交 |
|---|---|---|
| `fire` | 首次选课；明确 token 失效后的下一次允许尝试 | 按次数与间隔 |
| `watch` | 启动时建立查询上下文；明确 token 失效后下一次出手 | 快照有空位且未到期 |
| `drop` | 不要求访问；沿用 `undefined` 参数 | 按次数与间隔 |

`refresh`、`find --selected` 也会读取 defaultPage，预热不会。defaultPage 是有状态的页面初始化，
不是每次写入前的通用校验；外部浏览器重新打开页面仍可能使当前 token 失效。

离线回归：`python3 tests/watch_path.py target/debug/course-election`，覆盖正常命中、dry-run、
截止时间、慢查询/初始化/token 刷新、临时失败恢复、认证错误和在途 POST 结果保留。

`fire` 与 `watch` 的分工：`fire` 盲目重复写请求（适合已知有余量、只需重试）；`watch` 先盯名额、
出现空位才写，适合满员课程等人的退课。

退课：

```text
course-election> drop
course-election> drop 5 1000
```

## 导出课程表

```text
course-election> export-schedule
course-election> export-schedule 415
course-election> export-schedule 415 my-schedule.html
```

未指定学期时，程序以 `2025` 学年秋季学期 `395` 为基准自动滚动：每学年增加 `20`，春季学期为同学年秋季 `+1`。也可通过 `COURSE_ELECTION_SEMESTER_ID` 校准默认值。

默认输出为 `class-schedule-<semester-id>.html`。导出复用当前登录 Session，不会启动浏览器或执行第二套登录。

## 本地状态

```text
cache/cookies.json
cache/channels.json
cache/mapping_<profileID>.json
cache/counts_<profileID>.json
```

Cookie 只保存 JWXT 域（含网关在该域签发的 Cookie），并在网络操作结束后写入；密码、CAS TGC 和 URL 中的一次性 ticket 不会持久化。直接 CAS 与网关链路共用受限跳转处理，整个登录最多跟随 10 次重定向，仅允许已验证的主机、路径和 HTTPS；教务 HTTP 首页回调先升级为 HTTPS，保留 `;jsessionid=...`。未知跳转仅显示脱敏地址；服务端 CAS 校验的 TLS 握手错误会单独提示，错误正文以权限 `0600` 保存到临时文件。

`clear` 清除登录 Cookie；`clear all` 同时清除课程映射和容量缓存。

## REPL 命令

```text
login <用户名>
status
channels
profile <id>
refresh
find [--selected] [--id ID|--code CODE|名称]
target <lesson-id|完整课程名>
export-schedule [semester-id] [output.html]
arm [RFC3339时间]
watch [间隔秒] [超时秒]
fire [次数] [间隔毫秒]
drop [次数] [间隔毫秒]
clear [all]
cancel
quit
```

## REPL 驱动工具（自动化操作）

`repl-driver/repl-pty.py` 在当前终端运行程序，同时提供 Unix socket 输入通道和追加日志。
人可以直接操作原生 REPL，Agent 可以异步提交命令并累计读取最近一次提交后的输出；Agent
输入产生的逐字符终端重绘不会写入日志：

```bash
python3 repl-driver/repl-pty.py run './course-election' \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock

python3 repl-driver/repl-pty.py write 'status' \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
python3 repl-driver/repl-pty.py read --log /tmp/course-election-repl.log
```

关闭程序时在 REPL 输入 `quit`；驱动会自动删除 socket、log 和 baseline。详细语义和两步登录示例见
[repl-driver/repl.md](repl-driver/repl.md)。
