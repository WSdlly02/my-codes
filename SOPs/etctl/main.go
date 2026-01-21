package main

import (
	"context"
	"etctl/config"
	"etctl/install"
	"fmt"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"runtime"
	"syscall"
	"time"
)

func main() {
	if err := config.LoadConfig(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// === Install EasyTier ===
	var (
		path string
		ok   bool
	)
	if path, ok = install.DetectEasyTierExist(); !ok {
		log.Println("EasyTier does not exist, starting installation...")
		if err := install.InstallEasyTier(); err != nil {
			log.Println(err)
			os.Exit(1)
		}
		log.Println("EasyTier installation completed")
		// 重新检测路径
		if path, ok = install.DetectEasyTierExist(); !ok {
			log.Println("installation failed")
			os.Exit(1)
		}
	} else {
		log.Println("EasyTier already exists, skipping installation")
	}

	// === Build EasyTier Args ===
	args, err := config.BuildEasyTierArgs()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// === Signal Handler ===
	ctx, stop := signal.NotifyContext(
		context.Background(),
		os.Interrupt,
		syscall.SIGTERM,
	)
	defer stop()

	// === Run EasyTier ===
	log.Println("EasyTier starting...")
	cmd, err := startEasyTier(path, args)
	if err != nil {
		log.Println("EasyTier start failed:", err)
		os.Exit(1)
	}
	log.Println("EasyTier started, PID:", cmd.Process.Pid)
	// errCh 代表进程退出信号（携带退出错误，nil 表示正常退出）
	errCh := make(chan error, 1)
	go func() {
		waitErr := cmd.Wait()
		errCh <- waitErr // 可能是 nil（正常退出）或非 nil（异常退出）
		close(errCh)
	}()

	// === Wait for Exit ===
	select {
	case <-ctx.Done():
		log.Println("Signal received, starting graceful shutdown...")
	case err := <-errCh:
		// 进程自己退出了
		if err != nil {
			log.Println("EasyTier Abnormal Exit:", err)
			os.Exit(1)
		}
		log.Println("EasyTier Normal Exit")
		return
	}

	// === Stop EasyTier ===
	// 走到这一步说明选择了 <-ctx.Done() 分支
	shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	// 发送终止信号
	if runtime.GOOS == "windows" {
		_ = cmd.Process.Signal(os.Interrupt)
	} else {
		_ = cmd.Process.Signal(syscall.SIGTERM)
	}

	// === Wait for Stop ===
	select {
	case <-errCh:
		log.Println("EasyTier graceful shutdown completed")
	case <-shutdownCtx.Done():
		log.Println("graceful shutdown timeout, force termination...")
		_ = cmd.Process.Kill()
		<-errCh // 等待 goroutine 结束
		log.Println("EasyTier force terminated")
	}
}

// startEasyTier 启动 easytier-core，stdout/stderr 直接输出到控制台
// 注意：不使用 CommandContext，由调用方负责生命周期管理
func startEasyTier(binPath string, args []string) (*exec.Cmd, error) {
	cmd := exec.Command(binPath, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Start(); err != nil {
		return nil, err
	}

	return cmd, nil
}
