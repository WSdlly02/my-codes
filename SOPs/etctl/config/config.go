package config

import (
	"fmt"
	"hash/fnv"
	"os"
	"regexp"

	"github.com/BurntSushi/toml"
	"github.com/google/uuid"
)

// hash 使用 FNV-1a 算法将字符串转换为 uint32
func hash(s string) uint32 {
	h := fnv.New32a()
	h.Write([]byte(s))
	return h.Sum32()
}

// ClientConfig 定义了 EasyTier 客户端的配置结构
type ClientConfig struct {
	EasyTierVersion string          `toml:"easytier_version"` // EasyTier 版本号
	BootStrap       bootStrapConfig `toml:"bootstrap"`
	Network         networkConfig   `toml:"network"`
	Node            nodeConfig      `toml:"node"` // 节点配置
}
type bootStrapConfig struct {
	Peer string `toml:"peer"`
}
type networkConfig struct {
	Name   string `toml:"name"`
	Secret string `toml:"secret"`
}
type nodeConfig struct {
	InterfaceName string `toml:"interfaceName"`
	IPv4          string `toml:"ipv4"`
	DeviceName    string `toml:"deviceName"`
	MachineID     string `toml:"machine-id"`
}

// AppConfig 是全局变量，存储已加载的客户端配置
var AppConfig ClientConfig

// LoadConfig 从当前目录下的 config.toml 加载并解析配置文件
func LoadConfig() error {
	file, err := os.ReadFile("config.toml")
	if err != nil {
		return err
	}
	// Unmarshal 用于将文件内容解析为结构体
	if err := toml.Unmarshal(file, &AppConfig); err != nil {
		return err
	}

	updates := make(map[string]string) // 需要更新的配置项
	if AppConfig.Node.DeviceName == "" {
		fmt.Println("Getting Device Name...")
		hostname, err := os.Hostname()
		if err != nil {
			return err
		}
		AppConfig.Node.DeviceName = hostname
		updates["deviceName"] = hostname
	}
	if AppConfig.Node.MachineID == "" {
		fmt.Println("Generating Machine ID...")
		AppConfig.Node.MachineID = uuid.New().String()
		updates["machine-id"] = AppConfig.Node.MachineID
	}
	if AppConfig.Node.IPv4 == "" {
		fmt.Println("Generating IP Address...")
		host := 2 + (hash(AppConfig.Node.MachineID) % 250)
		AppConfig.Node.IPv4 = fmt.Sprintf("10.144.144.%d/24", host)
		updates["ipv4"] = AppConfig.Node.IPv4
	}
	if len(updates) > 0 { // Needs to update config.toml
		contentStr := string(file)
		for key, value := range updates {
			contentStr = updateTOMLValue(contentStr, key, value)
		}
		if err := os.WriteFile("config.toml", []byte(contentStr), 0644); err != nil {
			return err
		}
	}
	fmt.Println("Configuration loaded")
	return nil
}

// updateTOMLValue 通用的 TOML 值替换函数
func updateTOMLValue(content, key, newValue string) string {
	// 匹配顶层或嵌套的键: key = "value" 或 key = value
	pattern := fmt.Sprintf(`(?m)^(\s*%s\s*=\s*)["']?[^"\n]*["']?\s*$`, regexp.QuoteMeta(key))
	replacement := fmt.Sprintf(`${1}"%s"`, newValue)

	re := regexp.MustCompile(pattern)
	return re.ReplaceAllString(content, replacement)
}

func BuildEasyTierArgs() ([]string, error) {
	if AppConfig.Network.Name == "" {
		return nil, fmt.Errorf("network.name is empty")
	}
	if AppConfig.Network.Secret == "" {
		return nil, fmt.Errorf("network.secret is empty")
	}
	if AppConfig.BootStrap.Peer == "" {
		return nil, fmt.Errorf("bootstrap.peer is empty")
	}
	if AppConfig.Node.InterfaceName == "" {
		return nil, fmt.Errorf("node.interfaceName is empty")
	}
	if AppConfig.Node.IPv4 == "" {
		return nil, fmt.Errorf("node.ipv4 is empty")
	}
	if AppConfig.Node.DeviceName == "" {
		return nil, fmt.Errorf("node.deviceName is empty")
	}

	args := []string{
		"--network-name", AppConfig.Network.Name,
		"--network-secret", AppConfig.Network.Secret,
		// 定义接口名称
		"--dev-name", AppConfig.Node.InterfaceName,
		// -i 等价于 --ipv4
		"-i", AppConfig.Node.IPv4,
		// 设备名展示用
		"--hostname", AppConfig.Node.DeviceName,
		// 你的 VPS 当握手发现节点
		"-p", AppConfig.BootStrap.Peer,
	}

	return args, nil
}
