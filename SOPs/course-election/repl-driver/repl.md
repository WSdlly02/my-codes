# course-election PTY 驱动

`repl-pty.py` 在当前终端中运行 `course-election`，将键盘输入直接转发给子 PTY，并把程序输出
追加到日志。Agent 通过 Unix socket 写入同一个 PTY，通过日志读取结果；不依赖 Zellij 或 tmux。

## 启动

在希望长期保留的终端中运行：

```bash
python3 repl-driver/repl-pty.py run './course-election' \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
```

这个前台进程持有 REPL 和其中的登录状态、Cookie、课程缓存及预热连接。人可以直接在该终端
输入，方向键历史、Ctrl+C/D 等行为与直接运行 `course-election` 相同。

同一个 socket 已有活跃进程时，第二次启动会拒绝覆盖；异常退出留下的失效 socket 会自动清理。

## Agent 异步操作

提交命令：

```bash
python3 repl-driver/repl-pty.py write 'profile 2936' \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
```

`write` 会先把日志当前长度保存为唯一 baseline，再将命令和回车写入 PTY，然后立即退出。

读取结果：

```bash
python3 repl-driver/repl-pty.py read --log /tmp/course-election-repl.log
python3 repl-driver/repl-pty.py read --wait 5 --log /tmp/course-election-repl.log
```

`read` 输出 `baseline` 到当前日志末尾的全部字节，但不移动 baseline。因此重复读取会得到从最近
一次 `write` 开始的累计输出；下一次 `write` 才会覆盖观察窗口。`--wait` 在暂时没有新输出时
最多等待指定秒数，空输出只表示程序尚未打印新内容。

查看完整日志：

```bash
python3 repl-driver/repl-pty.py read --all --log /tmp/course-election-repl.log
```

Agent 通过 socket 提交命令时，驱动会丢弃输入回显结束前的 PTY 输出，因此逐字符重绘和命令
回显不会写入日志。之后的程序输出会剥离 ANSI 控制序列（光标移动、清屏、颜色等）后按可见
文本写入日志，人类终端仍显示原生输出。已经写入的历史不会因终端尺寸变化而重排。人类直接
键盘输入不经过该过滤器。

## 两步登录

假设 shell 中已有 `USERNAME` 和 `PASSWORD`：

```bash
python3 repl-driver/repl-pty.py write "login $USERNAME" \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
python3 repl-driver/repl-pty.py read --wait 10 --log /tmp/course-election-repl.log

python3 repl-driver/repl-pty.py write "$PASSWORD" \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
python3 repl-driver/repl-pty.py read --wait 10 --log /tmp/course-election-repl.log
```

第一次 `read` 用于确认已经出现密码提示；第二次 `write` 会建立新的 baseline，随后 `read` 获取
OCR、CAS 登录结果及后续输出。

## 人与 Agent 同时操作

人类键盘和 Agent socket 都写入同一个 PTY，终端上能看到双方操作。避免在同一瞬间输入，
否则字节可能交错。人类输入不会改变 Agent baseline，但对应输出会自然出现在当前累计日志中。

## 退出

人类可直接输入 `quit`，Agent 也可发送：

```bash
python3 repl-driver/repl-pty.py write 'quit' \
  --log /tmp/course-election-repl.log --in-sock /tmp/course-agent.sock
```

子程序退出后，驱动恢复当前终端设置，并删除输入 socket、日志和 `<log>.baseline`。这些文件只在
当前 REPL 运行期间存在；强制杀死驱动或断电时可能残留。

## 参数

| 参数 | 默认值 | 作用 |
|---|---|---|
| `run <程序>` | 无 | 启动指定的交互程序 |
| `--log` | `repl.log` | 追加输出日志 |
| `--in-sock` | `/tmp/repl-pty.sock` | Agent 输入 socket |
| `--wait` | `0` | `read` 等待新输出的秒数 |
| `--all` | 关闭 | `read` 输出完整日志 |
| `--lf` | 关闭 | `write` 使用 LF 而不是 CR 提交命令 |
