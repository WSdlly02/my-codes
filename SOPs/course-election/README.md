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

## 登录

```text
course-election> login 202410000000
密码: ********
```

密码不会显示或写入文件。程序会：

1. 请求教务系统并进入 CAS；
2. 解析当前页面的 `execution`；
3. 请求带 `captchaToken` 的验证码；
4. 使用本地 Ollama 识别算术验证码；
5. 提交 CAS 表单并受限跟随 ticket 回调；
6. 验证教务系统 Session 并保存 JWXT Cookie。

验证码错误最多自动刷新三次；密码错误立即停止。默认 OCR 服务为：

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
已预热；按 Enter 或输入 fire 触发，输入 cancel 取消
```

待命期间每 10 秒保活一次。触发时使用本次 `defaultPage` 响应头的 `Date` 生成 `elecSessionTime`，随后立即通过第二条预热连接 POST。

定时触发使用 RFC3339 时间：

```text
course-election> arm 2026-09-01T12:00:00+08:00
```

程序在 T-5 秒执行双连接预热，并在目标时间发起一次选课。

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

Cookie 只保存 JWXT 域，并在网络操作结束后写入；密码、CAS TGC 和 ticket 不会持久化。

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
