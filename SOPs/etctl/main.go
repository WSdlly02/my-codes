package main

import (
	"bufio"
	"context"
	"etctl/config"
	"etctl/install"
	"etctl/tui"
	"fmt"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"runtime"
	"syscall"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

func main() {
	if err := config.LoadConfig(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// === Install EasyTier ===
	var (
		corePath string
		cliPath  string
		ok       bool
	)
	if corePath, cliPath, ok = install.DetectEasyTierExist(); !ok {
		log.Println("EasyTier does not exist, starting installation...")
		if err := install.InstallEasyTier(); err != nil {
			log.Println(err)
			os.Exit(1)
		}
		log.Println("EasyTier installation completed")
		// 重新检测路径
		if _, _, ok = install.DetectEasyTierExist(); !ok {
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

	// === 创建日志 Channel ===
	logCh := make(chan string, 100)

	// === Run EasyTier ===
	log.Println("EasyTier starting...")
	cmd, err := startEasyTierCoreWithLog(corePath, args, logCh)
	if err != nil {
		log.Println("EasyTier start failed:", err)
		os.Exit(1)
	}
	log.Println("EasyTier started, PID:", cmd.Process.Pid)

	// errCh 代表进程退出信号
	errCh := make(chan error, 1)
	go func() {
		waitErr := cmd.Wait()
		errCh <- waitErr
		close(errCh)
		close(logCh) // 进程退出后关闭日志 channel
	}()

	// === 启动 TUI ===
	tuiModel := tui.NewModel(logCh, cliPath)
	p := tea.NewProgram(tuiModel, tea.WithAltScreen())

	// 在单独的 goroutine 中监听退出信号
	go func() {
		select {
		case <-ctx.Done():
			// 收到系统信号，退出 TUI
			p.Quit()
		case <-errCh:
			// easytier-core 退出，退出 TUI
			p.Quit()
		}
	}()

	// 运行 TUI（阻塞）
	if _, err := p.Run(); err != nil {
		log.Println("TUI error:", err)
	}

	// === TUI 退出后，优雅关闭 EasyTier ===
	log.Println("Shutting down EasyTier...")

	// 检查进程是否还在运行
	select {
	case <-errCh:
		// 进程已经退出
		log.Println("EasyTier already exited")
		return
	default:
		// 进程还在运行，需要关闭
	}

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
		<-errCh
		log.Println("EasyTier force terminated")
	}
}

// startEasyTierCoreWithLog 启动 easytier-core，将输出发送到 logCh
func startEasyTierCoreWithLog(binPath string, args []string, logCh chan<- string) (*exec.Cmd, error) {
	cmd := exec.Command(binPath, args...)

	// 获取 stdout 和 stderr 管道
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, err
	}
	// 启动 easytier-core
	if err := cmd.Start(); err != nil {
		return nil, err
	}

	// 启动 goroutine 读取输出
	go tui.StreamReader(bufio.NewReader(stdout), logCh, "")
	go tui.StreamReader(bufio.NewReader(stderr), logCh, "[stderr] ")

	return cmd, nil
}
