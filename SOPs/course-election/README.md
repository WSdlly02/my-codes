# course-election

上海海事大学选课工具。两个独立二进制：**CLI 负责离线读与发送指令，daemon 持有会话并执行意图**。两者通过数据目录下的 Unix socket 通信。仅支持类 Unix 系统。

## 启动与使用

```bash
cargo build --release --bins
# 终端 A，前台运行；数据目录须已存在
./target/release/course-electiond --data-dir "$PWD"

# 终端 B，或 Agent；两端必须使用同一个数据目录
./target/release/course-election login 学号
./target/release/course-election channels --refresh
./target/release/course-election profile 3112
./target/release/course-election refresh
./target/release/course-election find 航运 --profile 3112
./target/release/course-election watch --lesson 252279 --interval 5s
./target/release/course-election watch --lesson 252282 --interval 5s
./target/release/course-election --json jobs
```

CLI **不启动、不停止 daemon**。在 daemon 终端按 Ctrl-C 或发送 SIGTERM 优雅关停：停止新授权，等待已发请求和已接受的维护操作收尾后退出。

**意图只存在于 daemon 内存中，不跨进程继承。** daemon 退出（包括崩溃）后所有意图随之消失；重启后需重新 profile 并重新下达。崩溃时若有在途 POST，其结果只能用 `selected --refresh` 人工确认。

构建只更新 target，**不会替换根目录现有的旧版二进制**。

## 命令

全局参数：`--data-dir DIR`、`--json`。完整参数以各子命令 `--help` 为准。

| 命令 | 行为 |
| --- | --- |
| login USER [--password-stdin] | 默认隐藏输入密码；凭据交给 daemon 完成 CAS/OCR，不经过命令行参数 |
| logout [--force] | 清除会话；有活动意图时需要 force |
| profile ID [--force] | **实际打开 defaultPage**，即使 ID 相同；查询当前 profile 用 status |
| prepare | 预热连接并确保当前上下文；不无故旋转 token |
| refresh | 显式刷新课程映射/容量缓存，更新页面上下文 |
| find [名称] --profile ID [--id ID\|--code CODE] [--selected] | 纯离线查缓存；无缓存直接报错，不偷偷联网 |
| channels [--refresh] | 默认离线，refresh 才联网 |
| selected --profile ID / selected --refresh | 分别读已选缓存 / 从服务器刷新 |
| fire / drop --lesson ID [--attempts N] [--interval 500ms] [--wait] | 创建选课 / 退课意图；默认尝试一次，0 为不限次数 |
| watch --lesson ID [--interval 5s] [--timeout 30m] [--dry-run] [--wait] | 动态捡漏，满员时不 POST；间隔至少 1s，dry-run 只记录机会 |
| arm --lesson ID --at RFC3339 [--attempts 2] [--wait] | 定时意图，提前约 5 秒预热，沿用当前 token |
| status / jobs / job show ID | 快照查询，不等待上游写请求 |
| job pause / resume / cancel ID | 控制后续执行；cancel 不是退课 |
| job wait ID | 等待结束/暂停；Ctrl-C 只停止等待，不取消后台意图 |
| job reconcile ID | 对 unknown 意图查询当前已选状态，记录目标是否达成；**不重发 POST，也不证明原请求是否执行** |
| logs [--follow] | daemon 内存日志环（512 条）；缺口会提示，jobs 为权威状态 |
| export-schedule [--semester ID] [--output FILE] | 共用登录会话导出 HTML 课程表；默认学期保留原有自动滚动规则 |
| cache status / cache clear | 离线查看缓存概况 / 请求 daemon 清除课程映射和容量缓存 |

`--lesson` 也接受缓存中唯一匹配的完整课程名；有歧义时必须用 ID。无需再维护独立 target。为避免互相冲突，同一 profile/课程只允许一个活动或 unknown 意图。

默认 watch 的 timeout 从创建时开始计时，暂停也不延长截止时间；`--timeout 0s` 禁用截止。fire/drop 默认不限总时长，但有次数上限。业务失败可以重试，HTTP 错误、断连、无法识别的提交响应一律 unknown。准备失败也计入 fire/drop/arm 的尝试上限。达到 deadline 后不授权新 POST，已经授权的请求仍收尾。

## 运行时与取消边界

```text
CLI（clap） → Unix socket 上的 HTTP（axum） → Manager（意图池、调度、授权）
                                            ↑                ↓
                                 Read side（共享名额读取）   Executor（唯一 Session、串行执行）
```

- Manager 只做决策、不做网络请求；调度规则都是 Manager 上的纯方法，有单元测试。主循环按"下一个需要行动的时刻"精确休眠，被请求、Executor 结果或名额读取结果唤醒，不做固定间隔轮询。
- 一个共享轮询器按活动 watch 中最短间隔读容量，再唤醒有空位的意图，不为每门课创建轮询循环。
- Executor 是唯一可打开页面、切换会话和提交写入的所有者，一次只执行一项工作，结束时报告当前页面上下文。读侧只有容量查询能力；POST 在途时读侧仍能运行。
- Manager 只在名额读取结束（必要时先取消它）之后，才下发可能改变页面上下文的工作（维护操作、需要准备页面的意图）。因此到达的读取结果总是属于当前上下文。
- 提交前，Executor 先准备，再向 Manager 请求授权。Manager 核验 profile、截止时间、取消状态、watch 空位后把意图置为 InFlight，才放行一次 POST。
- 取消在授权前生效：不发 POST；授权后取消仅在 POST 返回后生效，真实结果仍记录。关闭 CLI 或断开连接不会取消已接受指令。
- profile 切换检查其他 profile 的活动意图，未 force 则在访问网站前拒绝。force 取消它们的后续执行，等在途 POST 收尾，再加载新页面；历史结果保留。
- 维护请求只接纳一个，期间不再授权新提交；忙时明确报错，不无限排队。
- unknown 不能直接 cancel/resume，`job reconcile` 核对后才能新建同课意图。

## token 与热路径

`Session` 是页面上下文唯一所有者。profile 命令打开页面，从 HTML 隐藏字段取得真实 `elecSessionTime`；之后 fire/drop/watch/arm 共享，不从 Date 头猜时间，不按命令重复 GET。

选课提交真实 token；退课同样需要页面初始化，但参数仍为 `undefined`。refresh、selected/reconcile 等实际加载页面的操作会更新同一个上下文。服务端明确返回“同时打开多个选课页面”才失效它；下一次允许的尝试先重建，watch 还需重新读取空位。

连接预热复用长期存活的 reqwest Client/连接池；prepare 与 arm 提前预热都不主动清除有效 token。**外部浏览器/其他进程仍可使 token 失效**，请避免同时操作。arm 与其他写意图共用串行 Executor，无法保证繁忙时精确卡秒；需精确时点时请提前暂停其他写意图。

## 本地 IPC 与数据

daemon 监听 `cache/runtime/daemon.sock`，runtime 目录权限 0700，访问控制即文件权限。同数据目录进程锁避免启动第二实例；崩溃遗留的 socket 文件在下次启动时清理。

Cookie、课程映射/容量/已选数据写在 cache，原子替换避免读到半份 JSON。缓存目录包含敏感信息，勿提交或分享。不同数据目录不会协调学校的单会话限制，不要据此运行多个真实选课 daemon。

默认 CLI 输出可读 JSON；`--json` 输出单行结构化 JSON，写入接受响应并不等于课程已选中，须查 job 或加 --wait。API 为 socket 上的 `POST /v1/command`，协议类型定义在 `src/app/protocol.rs`；维护类命令形如 `{"command":"maintenance","op":"refresh"}`。没有停止 daemon 的 API。

## 登录与导出

保留纯 HTTP CAS/网关重定向校验、验证码/OCR、Cookie 恢复和课表导出。密码只在本次请求内传递，不写入意图或日志；不应通过 shell 参数传密码。OCR 图片路径、识别结果、详细请求计时在 **daemon 终端** 输出。

默认 OCR：`http://10.144.144.64:11434/api/generate`，模型 `qwen3-vl:8b-instruct`。验证码以 0600 保存到临时目录；异常登录响应也保存供诊断。登录前需结束活动意图；登录失败不会继续使用旧账户的会话。课程表导出沿用默认学期计算，也可显式覆盖。

## 验证

```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo build --bins
python3 tests/daemon_path.py
```

daemon_path 通过 `tests/fixture.py` 的本地 TLS CONNECT 代理模拟教务系统，不访问真实学校网站；覆盖单实例锁、共享读取/token、取消、强制切换、drop、未知结果、arm 定时精度、CLI 与退出清理。真实服务器在共享 Session 下读写并行的行为仍需实机验证，本地测试不代表真实抢课成功。
