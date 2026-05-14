# course-election 使用文档

本文以 Rust 版本 `course-election` 为准。

## 功能概览

`course-election` 是上海海事大学教务系统选课 CLI。它通过本地浏览器获取登录 Cookie，再用 HTTP 接口完成通道查询、课程查询、容量刷新、选课和退课。

主要能力：

- 浏览器登录预热，并缓存 Cookie。
- 查询选课通道和 `profileID`。
- 按课程名、课程 ID、课程号查询课程详情。
- 查询当前已选课程。
- 查询课表 HTML。
- 按课程 ID 或课程名执行选课、退课。
- 自动维护运行中更新的 Cookie，并写回 `cache/cookies.json`。

## 环境要求

- Rust toolchain。
- 本机可启动 Chrome/Chromium。
- 可访问 `https://jwxt.shmtu.edu.cn/shmtu`。
- 可选：如果使用 `--autofill-captcha`，需要本地 Ollama 服务：
  - 地址：`http://localhost:11434/api/generate`
  - 默认模型：`qwen3-vl:8b-instruct`

## 构建

开发运行：

```bash
./course-election <command> [options]
```

发布构建：

```bash
cargo build --release
./target/release/course-election <command> [options]
```

如果当前目录已有编译产物，也可以直接运行：

```bash
./course-election <command> [options]
```

## 文件与缓存

程序会使用以下本地状态:

- `cache/cookies.json`：登录 Cookie。
- `cache/channels.json`：选课通道缓存。
- `cache/mapping_<profileID>.json`：课程映射缓存。
- `cache/counts_<profileID>.json`：课程容量缓存。
- `chrome-profile/`：Chrome 用户数据目录，用于复用浏览器登录状态。

运行中服务端返回的新 Cookie 会自动写回 `cache/cookies.json`。查询课表时写入的 `semester.id` 也会通过同一逻辑持久化。

## 推荐流程

### 1. 登录预热

手动登录：

```bash
./course-election warmup
```

程序会打开浏览器。请在浏览器中完成登录，程序检测到登录成功后会保存 Cookie，并尝试缓存选课通道。

自动填充账号密码：

```bash
./course-election warmup --username <学号> --password <密码>
```

这只会自动填充账号密码，验证码仍需手动填写并提交。

自动识别验证码并提交：

```bash
./course-election warmup --username <学号> --password <密码> --autofill-captcha
```

只有同时提供 `--username`、`--password` 和 `--autofill-captcha` 时，程序才会自动提交登录表单。验证码 OCR 失败时不会中断登录流程；程序会打印警告，并等待你手动填写验证码、提交登录表单。

登录等待超时时间为 120 秒。

注意：`warmup` 会允许通道缓存失败。也就是说，只要 Cookie 已保存，即使通道解析失败，命令也会以警告形式结束。之后仍可通过已知 `profileID` 查询或操作。

### 2. 查看状态

```bash
./course-election status
```

输出内容包括：

- Cookie 文件是否存在。
- Cookie 过期时间。
- 当前会话是否有效。
- 通道缓存是否存在。
- 各个 mapping/counts 缓存状态。

### 3. 查询选课通道

```bash
./course-election query
```

输出示例：

```text
[1] 第一轮选课 | profile=2936 | 已开放 | ...
```

后续查询、选课和退课都需要使用 `profileID`。

### 4. 查询课程

按通道查询全部课程：

```bash
./course-election query --profile 2936
```

按课程名关键词查询：

```bash
./course-election query --profile 2936 --name 海事法
```

按课程 ID 查询：

```bash
./course-election query --profile 2936 --lesson-id 242153
```

按课程号查询：

```bash
./course-election query --profile 2936 --code FX120230
```

课程查询会优先使用本地 `mapping` 缓存；如果缓存不存在且 Cookie 有效，会在线拉取并缓存课程映射。容量数据会尽量在线刷新；如果刷新失败但本地存在旧容量缓存，会使用本地副本并打印警告。

### 5. 查询已选课程

```bash
./course-election query --profile 2936 --selected-lessons
```

也可以叠加筛选条件：

```bash
./course-election query --profile 2936 --selected-lessons --name 海事法
```

`--selected-lessons` 必须配合 `--profile` 使用，并且需要当前 Cookie 有效。

### 6. 查询课表 HTML

```bash
./course-election query --class-schedule
```

该命令会请求课表入口页，提取唯一学号，再请求课表 HTML 并直接打印。它需要当前 Cookie 有效。

### 7. 选课

按课程 ID 选课：

```bash
./course-election select --profile 2936 --lesson-id 242153
```

按课程名选课：

```bash
./course-election select --profile 2936 --name 海事法
```

按课程名选课依赖本地 `mapping_<profileID>.json`。如果同名课程对应多个 `lessonID`，程序会拒绝执行，并提示改用 `--lesson-id`。

重试选课：

```bash
./course-election select --profile 2936 --lesson-id 242153 --retry 20 --interval 500ms
```

无限重试：

```bash
./course-election select --profile 2936 --lesson-id 242153 --retry 0 --interval 500ms
```

`--interval` 只支持 `ms` 或 `s`，例如 `500ms`、`1s`。

选课实现流程：

1. 请求 `defaultPage`。
2. 从响应头 `Date` 解析服务器时间。
3. 将服务器时间转换为 `elecSessionTime`。
4. 立即 POST `batchOperator`。
5. 响应摘要包含“成功”时停止重试。

### 8. 退课

按课程 ID 退课：

```bash
./course-election drop --profile 2936 --lesson-id 242153
```

按课程名退课：

```bash
./course-election drop --profile 2936 --name 海事法
```

退课也支持 `--retry` 和 `--interval`：

```bash
./course-election drop --profile 2936 --lesson-id 242153 --retry 5 --interval 1s
```

退课请求中 `elecSessionTime` 固定为 `undefined`。

### 9. 清理状态

只清除登录 Cookie：

```bash
./course-election flush-state
```

同时清除课程映射和容量缓存：

```bash
./course-election flush-state --all
```

`flush-state --all` 会删除 `mapping_*.json` 和 `counts_*.json`，但保留 `channels.json`。

## 常用命令速查

```bash
# 登录并缓存 Cookie
./course-election warmup

# 检查状态
./course-election status

# 查看通道
./course-election query

# 查询课程
./course-election query --profile 2936 --name 海事法
./course-election query --profile 2936 --code FX120230
./course-election query --profile 2936 --lesson-id 242153

# 查询已选课程
./course-election query --profile 2936 --selected-lessons

# 查询课表 HTML
./course-election query --class-schedule

# 选课
./course-election select --profile 2936 --lesson-id 242153 --retry 0 --interval 500ms

# 退课
./course-election drop --profile 2936 --lesson-id 242153

# 清理登录状态
./course-election flush-state
```

## 注意事项

- `profileID` 是选课通道 ID，不是课程 ID。
- 对用户可见的“课程 ID”对应服务端 lesson ID，即课程数据中的 `id` 字段。
- 如果通道未开放，服务端可能返回 HTML 错误页或重定向，程序会提示登录态或通道状态可能无效。
- 课程容量数据实时性依赖教务系统接口。如果在线刷新失败，程序可能使用本地旧缓存并给出警告。
- `--retry 0` 表示无限重试，使用前应确认课程 ID 和通道 ID 正确。
- 自动验证码识别依赖本地 OCR 模型，不能保证识别正确。识别失败时可以手动填写；识别错误并自动提交后，程序会等待登录成功，最终可能超时。
