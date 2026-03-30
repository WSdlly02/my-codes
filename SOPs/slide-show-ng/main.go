// main.go
package main

import (
	"context"
	"crypto/rand"
	"embed"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"log"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sync"
	"time"
)

//go:embed all:frontend/dist
var frontend embed.FS

func main() {
	port := findPort()
	addr := "127.0.0.1:" + port
	token := randomToken()
	var httpServer *http.Server
	var browserCmd *exec.Cmd
	var managed bool
	var cleanup func()
	var err error
	var shutdownOnce sync.Once
	shutdownApp := func() {
		shutdownOnce.Do(func() {
			if managed && browserCmd != nil && browserCmd.Process != nil {
				_ = browserCmd.Process.Kill()
			}
			if httpServer != nil {
				shutdownServer(httpServer)
			}
			os.Exit(0)
		})
	}
	server := newDeckServer(token, shutdownApp)

	sub, _ := fs.Sub(frontend, "frontend/dist")
	mux := http.NewServeMux()
	mux.Handle("/", http.FileServer(http.FS(sub)))
	mux.HandleFunc("/api/ping", server.handlePing)
	mux.HandleFunc("/api/capabilities", server.handleCapabilities)
	mux.HandleFunc("/ws", server.handleWS)
	httpServer = &http.Server{
		Addr:    addr,
		Handler: mux,
	}
	serverErr := make(chan error, 1)

	go func() {
		err := httpServer.ListenAndServe()
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			serverErr <- err
			return
		}
		serverErr <- nil
	}()

	waitReady(addr)
	url := fmt.Sprintf("http://%s?token=%s", addr, token)
	browserCmd, managed, cleanup, err = openBrowser(url)
	if err != nil {
		log.Fatal(err)
	}
	defer cleanup()

	if managed {
		go func() {
			_ = browserCmd.Wait()
			shutdownApp()
		}()
	}

	if err := <-serverErr; err != nil {
		log.Fatal(err)
	}
}

func openBrowser(url string) (*exec.Cmd, bool, func(), error) {
	profileDir, err := os.MkdirTemp("", "pitchdeck-browser-*")
	if err != nil {
		return nil, false, func() {}, err
	}

	cleanup := func() {
		_ = os.RemoveAll(profileDir)
	}

	args := []string{
		"--app=" + url,
		"--new-window",
		"--window-size=1920,1080",
		"--disable-infobars",
		"--no-default-browser-check",
		"--no-first-run",
		"--user-data-dir=" + filepath.Clean(profileDir),
	}

	var candidates []string
	switch runtime.GOOS {
	case "linux":
		candidates = []string{"google-chrome", "chromium", "chromium-browser", "microsoft-edge", "brave-browser"}
	case "darwin":
		candidates = []string{
			"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
			"/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
			"/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
		}
	case "windows":
		candidates = []string{
			`C:\Program Files\Google\Chrome\Application\chrome.exe`,
			`C:\Program Files (x86)\Google\Chrome\Application\chrome.exe`,
			`C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe`,
			`C:\Program Files\Microsoft\Edge\Application\msedge.exe`,
			`C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe`,
		}
	}

	for _, browser := range candidates {
		cmd := exec.Command(browser, args...)
		if err := cmd.Start(); err == nil {
			return cmd, true, cleanup, nil
		}
	}

	switch runtime.GOOS {
	case "linux":
		if err := exec.Command("xdg-open", url).Start(); err == nil {
			return nil, false, cleanup, nil
		}
	case "darwin":
		if err := exec.Command("open", url).Start(); err == nil {
			return nil, false, cleanup, nil
		}
	case "windows":
		if err := exec.Command("rundll32", "url.dll,FileProtocolHandler", url).Start(); err == nil {
			return nil, false, cleanup, nil
		}
	}

	cleanup()
	return nil, false, func() {}, fmt.Errorf("failed to launch a browser")
}

func shutdownServer(server *http.Server) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	_ = server.Shutdown(ctx)
	os.Exit(0)
}

func randomToken() string {
	buf := make([]byte, 16)
	if _, err := rand.Read(buf); err != nil {
		return fmt.Sprintf("fallback-%d", time.Now().UnixNano())
	}
	return hex.EncodeToString(buf)
}

func findPort() string {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return "3000"
	}
	defer l.Close()
	return fmt.Sprintf("%d", l.Addr().(*net.TCPAddr).Port)
}

func waitReady(addr string) {
	for i := 0; i < 50; i++ {
		conn, err := net.Dial("tcp", addr)
		if err == nil {
			conn.Close()
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
}
