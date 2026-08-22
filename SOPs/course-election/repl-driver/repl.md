# course-election REPL 驱动工具

让选课 REPL 在后台常驻，通过 Unix socket 用短命令自动化操作（抢课、脚本化、AI 助手驱动）。

## 原理

```
每次操作 = 短命 client（pty_client.py）
    │ 连接 /tmp/course-repl.sock
    ▼
┌──────────────────────────────────────┐
│ pty_driver.py（常驻进程）               │
│  ├─ 分配伪终端（PTY），挂载 REPL 子进程  │
│  └─ socket ⇄ PTY 双向转发字节           │
└──────────────┬───────────────────────┘
               ▼
     course-election（你的 Rust REPL，一直活着）
```

关键点：

- REPL 进程**不随命令结束**，状态（登录 Cookie、profile、target）一直保留；
- 用 PTY 而不是管道，所以 `rpassword` 隐藏密码输入、终端回显等行为都正常；
- 每次执行只是一次 socket 连接：发输入、收输出、断开。

## 文件

| 文件 | 作用 |
|---|---|
| `pty_driver.py` | 常驻驱动：起 PTY、挂 REPL、开 socket |
| `pty_client.py` | 客户端：发送 stdin 内容，打印 REPL 输出 |
| `repl_login.py` | 用 `.env` 的 USERNAME/PASSWORD 安全登录 |
| `start-repl.sh` / `stop-repl.sh` | 一键启停 |
| `repl.md` | 本文档 |

## 快速开始

```bash
# 1. 启动（后台常驻）
./repl-driver/start-repl.sh

# 2. 发命令（短命令）
printf 'status\n' | python3 repl-driver/pty_client.py /tmp/course-repl.sock --wait 'course-election> '

# 3. 登录（读 .env，密码不回显不落盘）
python3 repl-driver/repl_login.py

# 4. 停止
./repl-driver/stop-repl.sh
```

## pty_client.py 参数

- `--wait 'course-election> '`：**推荐**。等到 REPL 提示符出现才返回，能抓完整输出，
  适合 `login` / `refresh` / `export-schedule` 等耗时命令。
- `--idle 秒数`：默认 1.2。最后一段输出后空闲 N 秒即返回（`--wait` 未给时生效）。
  适合会持续输出或中途等待的命令（如 `arm` 待命状态没有提示符）。
- `--max 秒数`：硬性上限，默认 120，防卡死。

示例：

```bash
printf 'find 海事法\n' | python3 repl-driver/pty_client.py /tmp/course-repl.sock --wait 'course-election> '
printf 'arm\n'         | python3 repl-driver/pty_client.py /tmp/course-repl.sock --idle 3 --max 30
printf 'fire\n'        | python3 repl-driver/pty_client.py /tmp/course-repl.sock --wait 'course-election> '
printf 'quit\n'        | python3 repl-driver/pty_client.py /tmp/course-repl.sock --idle 1 --max 10
```

## 抢课标准流程

```bash
python3 repl-driver/repl_login.py                                    # 1. 登录（.env）
printf 'channels\n' | python3 repl-driver/pty_client.py /tmp/course-repl.sock --wait 'course-election> '   # 2. 看轮次
printf 'profile 2936\n' | ... --wait 'course-election> '             # 3. 选轮次
printf 'refresh\n' | ... --wait 'course-election> '                  # 4. 刷新数据
printf 'find 海事法\n' | ... --wait 'course-election> '              # 5. 查课
printf 'target 242153\n' | ... --wait 'course-election> '            # 6. 固定目标
printf 'arm 2026-09-01T12:00:00+08:00\n' | ... --wait 'course-election> '   # 7a. 定时抢
printf 'arm\n' | ... --idle 3 --max 60                               # 7b. 手动待命（有保活输出）
printf 'fire 20 500\n' | ... --wait 'course-election> '              # 8. 触发/重试
```

注意：

- `arm`（不带时间）进入待命后**不再打印提示符**，直到输入 `fire` 或 `cancel`；
- `fire 0 500` 是无限重试，会持续输出，请用 `--max` 限时或改用有限次数；
- `export-schedule` 产物写到 REPL 的工作目录（`start-repl.sh` 已设为项目根目录）。

## 技巧与坑

1. **长命令一定用 `--wait` 提示符**，不要用默认 idle：`login` 在“密码: ”之后可能安静
   十几秒（OCR/网络），idle 会提前断开导致输出丢失。
2. **输出不缓存**：没有客户端连接期间 REPL 的输出会丢（驱动只转发给在线客户端）。
   先连再发，天然满足；丢了也没关系，用 `status` 等命令确认状态。
3. **一次一个会话**：socket 被占用时 `start-repl.sh` 会拒绝启动；先 stop 再 start。
4. **工作目录**：导出文件写到 REPL 的 cwd（`start-repl.sh` 已设为项目目录）。
5. **pkill 自匹配**：`stop-repl.sh` 用 `[.]` 转义避免误杀自身命令行。
6. 驱动是通用的：任何交互式程序都能套（`python3 -i`、node、其他 Rust REPL），
   换 `--cmd` 即可。

## 安全

- 密码只在 `repl_login.py` 进程内读取并直接发给 REPL 的隐藏提示符，**不打印、不写文件**；
- REPL 本身不持久化密码（只存 JWXT Cookie 到 `cache/cookies.json`）；
- 不要把 `.env` 提交进 git；建议 `chmod 600 .env`。
