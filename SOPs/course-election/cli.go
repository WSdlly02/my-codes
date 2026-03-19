package main

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
  course-election warmup

说明:
  status  检查 Cookie 是否存在、是否已过期、当前会话是否仍有效
  warmup  必要时打开浏览器手动登录获取 Cookie，然后解析并缓存选课通道
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
