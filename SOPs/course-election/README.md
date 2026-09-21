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

1. 请求教务系统，直接跳转到 CAS（service 指向教务首页）；
2. 解析当前页面的 `execution`；
3. 请求带 `captchaToken` 的验证码；
4. 使用本地 Ollama 识别算术验证码；
5. 提交 CAS 表单，将返回的教务 ticket 回调升级为 HTTPS 后访问；
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
- `refresh`：获取课程数据并刷新容量。
- `find [--selected] [--id ID|--code CODE|名称]`：查询全部课程或按名称、ID、课程号、已选状态过滤。
- `target <lesson-id|完整课程名>`：固定热路径目标；完整课程名必须唯一匹配。
- `status`：查看登录、profile、target 和缓存状态。

## 连接预热与选课

手动待命：

```text
course-election> arm
正在预热；按 Enter 或输入 fire 触发，输入 cancel 取消
```

待命期间立即预热，完成后等待 10 秒再保活。预热请求最多 2 秒、不重试，失败只提示；输入和定时触发不等待预热，触发时取消本地未完成的预热（不保证服务器已经停止处理）。触发时使用本次 `defaultPage` 响应头的 `Date` 生成 `elecSessionTime`，随后立即 POST，尽量复用连接池中的连接。

定时触发使用 RFC3339 时间：

```text
course-election> arm 2026-09-01T12:00:00+08:00
```

程序在 T-5 秒开始并发预热，并在目标时间发起一次选课。打印预热耗时和定时触发偏差；偏差依据本地时钟，不代表请求到达服务器的时间。

直接选课及重试：

```text
course-election> fire
course-election> fire 20 500
course-election> fire 0 500
```

参数依次为尝试次数和间隔毫秒；次数 `0` 表示无限。每次尝试都会重新执行：

```text
fresh defaultPage → Date → elecSessionTime → batchOperator
```

每次尝试打印 `defaultPage` 响应头耗时（含 GET 重试）、POST 完整响应耗时及选课总耗时。页面响应体在后台排空，不阻塞结果输出或下一次尝试；Cookie 在内存中即时更新，整轮成功或耗尽次数后保存，正常退出时也保存。强制终止进程不保证落盘。重试间隔仍从一次尝试结束后计算，POST 保持串行。

本地慢服务器回归测试：`python3 tests/arm_prewarm.py ./course-election`，使用隔离目录和本地代理，不访问真实选课接口。

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

Cookie 只保存 JWXT 域，并在网络操作结束后写入；密码、CAS TGC 和 URL 中的一次性 ticket 不会持久化。登录按当前直接 CAS 链路实现，限制跳转主机、路径和 HTTPS；支持教务首页路径中的 `;jsessionid=...`，HTTP 首页回调会先升级为 HTTPS。

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
