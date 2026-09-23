# course-election

上海海事大学选课工具。两个独立二进制：**CLI 负责离线读与发送指令，daemon 持有会话并执行意图**。两者通过数据目录下的 Unix socket 通信。仅支持类 Unix 系统。

## 启动与使用

```bash
cargo build --release --bins
# 终端 A，前台运行；数据目录须已存在。--poll 是所有 watch 共享的名额读取间隔（默认 5s，至少 1s）
./target/release/course-electiond --data-dir "$PWD" --poll 2s

# 终端 B，或 Agent；两端必须使用同一个数据目录
./target/release/course-election login 学号
./target/release/course-election channels --refresh
./target/release/course-election profile 3112
./target/release/course-election refresh
./target/release/course-election find 航运 --profile 3112
./target/release/course-election fire --lesson 252279 --at 2026-09-23T13:00:00+08:00 --attempts 3 --wait
./target/release/course-election watch --lesson 252282
./target/release/course-election --json jobs
```

CLI **不启动、不停止 daemon**。在 daemon 终端按 Ctrl-C 或发送 SIGTERM 优雅关停：取消全部意图的等待，已交给执行器的提交收尾后退出。

**意图只存在于 daemon 内存中**：运行中的意图可以查询，结束即消失，结果留在 `logs` 里（或由 `--wait` 返回）。没有暂停/恢复，也不跨进程继承；daemon 退出（包括崩溃）后需重新 profile 并重新下达。崩溃时若有在途 POST，其结果用 `selected --refresh` 确认。

构建只更新 target，**不会替换根目录现有的旧版二进制**。

## 命令

全局参数：`--data-dir DIR`、`--json`。完整参数以各子命令 `--help` 为准。

| 命令 | 行为 |
| --- | --- |
| login USER [--password-stdin] | 登录完成后全部意图结束；默认隐藏输入密码，凭据交给 daemon 完成 CAS/OCR |
| logout | 清除会话，全部意图随之结束 |
| profile ID | **实际打开 defaultPage**（即使 ID 相同），其他 profile 的意图随之结束；当前 profile 见 status |
| prepare | 预热连接并确保页面已打开；不无故旋转 token |
| refresh | 显式刷新课程映射/容量缓存，更新页面上下文 |
| find [名称] --profile ID [--id ID\|--code CODE] [--selected] | 纯离线查缓存；无缓存直接报错，不偷偷联网 |
| channels [--refresh] | 默认离线，refresh 才联网 |
| selected --profile ID / selected --refresh | 分别读已选缓存 / 从服务器刷新 |
| fire --lesson ID [--at RFC3339] [--attempts 1] [--interval 500ms] [--wait] | 到点（缺省立即）直接提交选课，不看容量；有 `--at` 时提前 5 秒预热 |
| drop （参数同 fire） | 同 fire，提交退课 |
| watch --lesson ID [--timeout 30m] [--dry-run] [--wait] | 捡漏：名额读取出现空位才提交选课；`--timeout 0s` 不设截止，dry-run 只记录机会 |
| status / jobs / job show ID | 快照查询，不等待上游请求 |
| job wait ID | 等待意图结束；Ctrl-C 只停止等待 |
| job cancel ID | 停止意图的等待；已交给执行器的提交仍会完成并记录。cancel 不是退课 |
| logs [--follow] | daemon 内存日志环（512 条），含每个意图的最终结果 |
| export-schedule [--semester ID] [--output FILE] | 共用登录会话导出 HTML 课程表；默认学期保留原有自动滚动规则 |
| cache status / cache clear | 离线查看缓存概况 / 请求 daemon 清除课程映射和容量缓存 |

`--lesson` 也接受缓存中唯一匹配的完整课程名；有歧义时必须用 ID。同一 profile/课程同时只允许一个运行中的意图。

服务器明确拒绝（未开放、已满等）可以重试，每次提交计一次尝试；HTTP 错误、断连、无法识别的响应记为 unknown，绝不自动重试。

## 运行时与取消边界

```text
CLI（clap） → Unix socket 上的 HTTP（axum） → Runtime（运行中意图的注册表）
                                                  │ 每个意图一个 task
                                                  ▼
                   名额轮询（watch channel） ──→ 意图 task ──→ Executor actor（唯一 Session，按到达顺序串行）
```

- **意图 = 一个 task**：等待触发（fire 等时刻，watch 等一次新鲜的空位读取），把提交交给 Executor，失败则按规则重试或结束。
- **取消与截止只作用于等待**：用 `CancellationToken` 和 `timeout_at` 包住等待。提交一旦交给 Executor 就一定完成，结果记入意图；此后的取消只阻止后续重试。
- **Executor 是 actor**：独占 `Session`，通过 channel 接收"确保页面 / 提交 / 维护"请求，逐个执行，先到先服务。
- **一个共享轮询器**：只在有 watch 运行且页面就绪时按 `--poll` 读名额，用 watch channel 发布最新一次读取。watch 只对订阅之后的新读取做出反应，同一次读取不会触发两次提交。
- **读写不交叠**：页面上下文变化（打开 defaultPage、登录、切换 profile）持有 RwLock 写锁，名额读取持有读锁；提交不取锁，POST 在途时读取照常进行。
- **意图绑定上下文**：意图的每次等待都同时监听 Executor 发布的 profile，一旦不再是自己的（切换 profile、login、logout，或切换失败导致没有 profile），就以"上下文已切换"结束。所以切换期间才被接纳的意图也会被收掉，与发起切换的请求是否还连着无关。已交给 Executor 的提交排在切换之前的会正常完成，排在之后的会因 profile 不符而不发送。

## token 与热路径

`Session` 是页面上下文唯一所有者。profile 命令打开页面，从 HTML 隐藏字段取得真实 `elecSessionTime`；之后所有意图共享，不从 Date 头猜时间，不按命令重复 GET。

**所有准备都在触发前完成**。定时 fire 在 T−5s 预热连接并确认页面已打开；watch 在每轮等待前确认页面已打开。触发后只剩一次 channel 跳转和一次 POST。

选课提交真实 token；退课同样需要页面初始化，但参数仍为 `undefined`。refresh、selected 等实际加载页面的操作会更新同一个上下文。服务端明确返回"同时打开多个选课页面"才使页面失效，意图在下一轮等待开始前重新打开，不放到下次触发之后。

连接预热复用长期存活的 reqwest Client/连接池，预热不主动清除有效 token。**外部浏览器/其他进程仍可使 token 失效**，请避免同时操作。Executor 串行：开抢时刻若正有别的提交在途，这次 fire 会排在它之后。另外时刻 T 以本机时钟为准，开抢前请确认 NTP 已同步。

## 本地 IPC 与数据

daemon 监听 `cache/runtime/daemon.sock`，runtime 目录权限 0700，访问控制即文件权限。同数据目录进程锁避免启动第二实例；崩溃遗留的 socket 文件在下次启动时清理。

Cookie、课程映射/容量/已选数据写在 cache，原子替换避免读到半份 JSON。缓存目录包含敏感信息，勿提交或分享。不同数据目录不会协调学校的单会话限制，不要据此运行多个真实选课 daemon。

CLI 默认输出给人看的文本（课程名取自本地课程映射缓存，没有缓存时只显示 ID）；`--json` 每行输出一个 JSON 值，供脚本和程序解析。意图被接受不等于课程已选中，须查 job、logs 或加 --wait。API 为 socket 上的 `POST /v1/command`，协议类型定义在 `src/app/protocol.rs`；维护类命令形如 `{"command":"maintenance","op":"refresh"}`。没有停止 daemon 的 API。

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

daemon_path 通过 `tests/fixture.py` 的本地 TLS CONNECT 代理模拟教务系统，不访问真实学校网站；覆盖单实例锁、共享轮询、取消边界、POST 在途时的读取、profile 切换、drop、重试、unknown、定时精度、CLI、logout 与退出清理。意图 task 的定时与取消逻辑另有基于 tokio 暂停时钟的单元测试。真实服务器在共享 Session 下读写并行的行为仍需实机验证，本地测试不代表真实抢课成功。
