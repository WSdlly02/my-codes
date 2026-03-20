package app

import (
	"flag"
	"fmt"
	"io"
	"os"
)

func usage() {
	fmt.Fprintf(os.Stderr, `选课 CLI

用法:
  course-election status
  course-election query [--profile <profileID>] [--name <关键词>] [--lesson-id <课程ID>] [--code <课程号>] [--selected-lessons]
  course-election warmup
  course-election select --profile <profileID> (--lesson-id <课程ID> | --name <课程名>) [--retry N] [--interval 500ms]
  course-election drop --profile <profileID> (--lesson-id <课程ID> | --name <课程名>) [--retry N] [--interval 500ms]

说明:
  status  检查 Cookie 是否存在、是否已过期、当前会话是否仍有效
  query   查询通道；指定 --profile 后按统一格式查询该通道下课程详情，--selected-lessons 仅显示当前已选课程
  warmup  必要时打开浏览器手动登录获取 Cookie，然后解析并缓存选课通道
  select  按步骤 5、6 执行最终选课：GET defaultPage 取 Date，随后立即 POST；--retry 0 表示无限重试
  drop    执行退课：POST batchOperator，elecSessionTime 固定为 undefined；--retry 0 表示无限重试
`)
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

func newFlagSet(name string) *flag.FlagSet {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	return fs
}
