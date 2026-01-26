package tui

import (
	"bufio"
	"fmt"
	"os/exec"
	"strings"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// LogMsg 表示一条日志消息
type LogMsg string

// CLIOutputMsg 表示 CLI 命令的输出
type CLIOutputMsg struct {
	Output string
	Err    error
}

// Model 是 TUI 的状态模型
type Model struct {
	viewport viewport.Model // 日志显示区域
	textarea textarea.Model // 命令输入框
	logs     []string       // 日志内容
	logCh    <-chan string  // 接收日志的 channel
	cliPath  string         // easytier-cli 路径
	width    int
	height   int
	ready    bool
	quitting bool

	// 样式
	logStyle    lipgloss.Style
	cliStyle    lipgloss.Style
	promptStyle lipgloss.Style
}

// NewModel 创建一个新的 TUI Model
func NewModel(logCh <-chan string, cliPath string) Model {
	// 初始化 textarea
	ta := textarea.New()
	ta.Placeholder = "Enter command (peer, route, connector, help)..."
	ta.Focus()
	ta.Prompt = "> "
	ta.CharLimit = 256
	ta.SetWidth(80)
	ta.SetHeight(1)
	ta.ShowLineNumbers = false
	ta.KeyMap.InsertNewline.SetEnabled(false)
	// 移除光标行样式
	ta.FocusedStyle.CursorLine = lipgloss.NewStyle()

	// 初始化 viewport
	vp := viewport.New(80, 20)
	vp.SetContent("EasyTier Console\nWaiting for logs...\n")

	return Model{
		viewport:    vp,
		textarea:    ta,
		logs:        []string{"[Console] EasyTier Console started"},
		logCh:       logCh,
		cliPath:     cliPath,
		logStyle:    lipgloss.NewStyle().Foreground(lipgloss.Color("252")),
		cliStyle:    lipgloss.NewStyle().Foreground(lipgloss.Color("86")),
		promptStyle: lipgloss.NewStyle().Foreground(lipgloss.Color("205")),
	}
}

// Init 初始化 TUI
func (m Model) Init() tea.Cmd {
	return tea.Batch(
		textarea.Blink,
		waitForLog(m.logCh),
	)
}

// Update 处理消息
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		taCmd tea.Cmd
		vpCmd tea.Cmd
	)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height

		// textarea 固定 1 行高度
		m.textarea.SetWidth(msg.Width)
		m.textarea.SetHeight(1)

		// viewport 占据剩余空间（减去 textarea 高度和分隔符）
		headerHeight := 1 // 分隔线
		footerHeight := 3 // textarea + 边距
		m.viewport.Width = msg.Width
		m.viewport.Height = msg.Height - headerHeight - footerHeight

		// 更新内容
		m.updateViewportContent()
		m.viewport.GotoBottom()
		m.ready = true

	case tea.KeyMsg:
		switch msg.Type {
		case tea.KeyCtrlC:
			m.quitting = true
			return m, tea.Quit

		case tea.KeyEnter:
			cmd := strings.TrimSpace(m.textarea.Value())
			if cmd != "" {
				m.logs = append(m.logs, m.promptStyle.Render("> "+cmd))
				m.textarea.Reset()
				m.updateViewportContent()
				m.viewport.GotoBottom()

				// 执行命令
				return m, tea.Batch(
					execCLI(m.cliPath, cmd),
					waitForLog(m.logCh),
				)
			}
		}

	case LogMsg:
		// 收到日志消息
		m.logs = append(m.logs, m.logStyle.Render(string(msg)))
		m.updateViewportContent()
		m.viewport.GotoBottom()
		return m, waitForLog(m.logCh)

	case CLIOutputMsg:
		// 收到 CLI 输出
		if msg.Err != nil {
			m.logs = append(m.logs, m.cliStyle.Render("[CLI Error] "+msg.Err.Error()))
		} else {
			lines := strings.Split(strings.TrimSpace(msg.Output), "\n")
			for _, line := range lines {
				m.logs = append(m.logs, m.cliStyle.Render("[CLI] "+line))
			}
		}
		m.updateViewportContent()
		m.viewport.GotoBottom()
		return m, waitForLog(m.logCh)
	}

	// 更新子组件
	m.textarea, taCmd = m.textarea.Update(msg)
	m.viewport, vpCmd = m.viewport.Update(msg)

	return m, tea.Batch(taCmd, vpCmd)
}

// View 渲染界面
func (m Model) View() string {
	if !m.ready {
		return "Initializing..."
	}

	if m.quitting {
		return "Quitting...\n"
	}

	// 分隔线
	separator := lipgloss.NewStyle().
		Foreground(lipgloss.Color("240")).
		Render(strings.Repeat("─", m.width))

	return fmt.Sprintf("%s\n%s\n%s",
		m.viewport.View(),
		separator,
		m.textarea.View(),
	)
}

// updateViewportContent 更新 viewport 的内容
func (m *Model) updateViewportContent() {
	// 限制日志行数，避免内存溢出
	maxLines := 1000
	if len(m.logs) > maxLines {
		m.logs = m.logs[len(m.logs)-maxLines:]
	}

	content := strings.Join(m.logs, "\n")
	// 应用宽度换行
	wrapped := lipgloss.NewStyle().Width(m.viewport.Width).Render(content)
	m.viewport.SetContent(wrapped)
}

// waitForLog 返回一个 Cmd，等待日志 channel
func waitForLog(logCh <-chan string) tea.Cmd {
	return func() tea.Msg {
		log, ok := <-logCh
		if !ok {
			return nil
		}
		return LogMsg(log)
	}
}

// execCLI 执行 CLI 命令
func execCLI(cliPath, command string) tea.Cmd {
	return func() tea.Msg {
		// 命令映射
		var args []string
		switch command {
		case "peer", "peers":
			args = []string{"peer"}
		case "route", "routes":
			args = []string{"route"}
		case "connector", "connectors":
			args = []string{"connector"}
		case "help", "?":
			return CLIOutputMsg{
				Output: "Available commands:\n  peer      - Display peer list\n  route     - Display routing table\n  connector - Display connector status\n  help      - Display help information",
			}
		default:
			// 尝试直接作为参数传递
			args = strings.Fields(command)
		}
		var cmd *exec.Cmd
		if args[0] == "shell" || args[0] == "sh" {
			// Pass the command to shell
			cmd = exec.Command(args[1], args[2:]...)
		} else {
			cmd = exec.Command(cliPath, args...)
		}
		output, err := cmd.CombinedOutput()
		return CLIOutputMsg{
			Output: string(output),
			Err:    err,
		}
	}
}

// StreamReader 从 Reader 读取并发送到 channel
func StreamReader(r *bufio.Reader, ch chan<- string, prefix string) {
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			return
		}
		line = strings.TrimRight(line, "\n\r")
		if line != "" {
			ch <- prefix + line
		}
	}
}
