# 选课路径实测：2026-09-22

范围：用户授权 profile `3112`、lesson `252279`（海上保险法 FX110220_001）的选课/退课。
同一登录 Session、同一 Python HTTPSConnection，串行请求，无自动重试。测试开始时目标未选。
时间为北京时间约 14:59–15:01，不是开抢拥堵时段。未修改生产代码或二进制。

## 页面契约

- `defaultPage` 返回 `<input name="elecSessionTime" ... value="20260922145946">`。
- 页面实际引用 `stdElectionCourse-0.3.1.js?v=32`。脚本读取该隐藏字段并传给 `tip.submit`，最终传到 `batchOperator` 查询参数。
- 提交完成回调更新本地课程对象并调用 `electCourseTable.init()`；这条提交链没有重新请求 `defaultPage`，也没有更新隐藏 token。
- 成功响应通过脚本更新 `elected` / `defaultElected`，没有轮换 token 的代码。

## 操作与证据

| 顺序 | 操作 | 结果 | 完整响应耗时 |
|---|---|---|---|
| 1 | GET defaultPage，取得 token A | 目标未选 | 203ms |
| 2 | 使用 A 选课 | 成功 | 137ms |
| 3 | 退课，时间参数 undefined | 成功 | 65ms |
| 4 | GET stdElectCourse.action | 入口正常 | 108ms |
| 5 | 再使用 A 选课，不刷新页面 | 成功 | 69ms |
| 6 | GET defaultPage，取得 token B | 目标已选，B 与 A 不同 | 84ms |
| 7 | 退课 | 成功 | 69ms |
| 8 | 使用旧 A 选课 | 拒绝：同时打开多个选课页面，请至最新页面进行操作 | 31ms |
| 9 | 使用 B 选课，不重新获取页面 | 成功 | 58ms |
| 10 | 退课 | 成功 | 63ms |
| 11 | GET defaultPage 核对 | 目标未选；完整已选集合与测试前相同，均为 11 门 | 88ms |

共 7 次写请求：3 次选课成功、3 次退课成功、1 次旧 token 选课被拒绝。
这些单次耗时只能说明本次请求表现，不是拥堵时段的性能基准。

## 结论与设计

1. 将 HTML 隐藏字段作为唯一 token 来源，不从 HTTP Date 推算；读不到有效字段时禁止提交。
2. 一轮 fire 首次获取页面 token，后续 POST 复用；本次已验证 token 在成功选课、退课、访问入口后仍有效，至少跨越约 34 秒。
3. 收到明确的“最新页面”拒绝时，清除本轮 token；如还有重试额度，下次重新 GET 后提交，不额外增加 POST 次数。
4. token 不跨 fire 命令持久缓存，避免 refresh、find --selected 等页面访问后使用旧值。
5. 预热删除 defaultPage，只访问入口。入口在本次测试中未使 token 失效；仍不代表它在所有部署下无副作用。
6. 网络错误不能推断写操作未执行；不新增隐式 POST 重试，不把重复选课响应直接判为成功。

尚未证明：token 的最大寿命、跨轮次行为、开抢拥堵时的收益、外部浏览器并发访问下的时序。

后续实施：上述设计已落入源码；`tests/selection_path.py` 提供离线 REPL/HTTP 链路回归，`tests/arm_prewarm.py` 验证单入口预热的超时及取消。根目录二进制未替换，修改后的代码未再次执行实机选课/退课。

原始响应存放在 `/tmp/course-path-probe-yii7cchh/`，目录及文件限制权限，未纳入仓库。
`1.html` 为初始页面，`3/4/6/8/9/10/11.html` 为写请求响应，`7.html` 为更新 token 的页面，`12.html` 为最终状态，`script-v32.js` 为页面引用的精确版本脚本。
