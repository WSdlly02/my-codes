package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// ==========================================
//              1. Configuration
// ==========================================

const (
	MihomoDir    = "/home/wsdlly02/.config/mihomo"
	LocalConfig  = MihomoDir + "/config.yaml"
	BackupConfig = "/home/wsdlly02/Disks/Files/mihomo-config.yaml"
	ServiceName  = "mihomo.service"
	ResolverURL  = "http://127.0.0.1:8088/config/full"
)

type DBSource struct {
	URL      string
	Filename string
}

var DBSources = map[string]DBSource{
	"geoip": {
		URL:      "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip-lite.dat",
		Filename: "geoip-lite.dat",
	},
	"geosite": {
		URL:      "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geosite.dat",
		Filename: "geosite.dat",
	},
	"mmdb": {
		URL:      "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip.metadb",
		Filename: "geoip.metadb",
	},
	"asn": {
		URL:      "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/GeoLite2-ASN.mmdb",
		Filename: "GeoLite2-ASN.mmdb",
	},
}

// ==========================================
//              2. Core Logic
// ==========================================

func downloadData(url string, dst string, timeout time.Duration) error {
	dstDir := filepath.Dir(dst)
	if err := os.MkdirAll(dstDir, 0755); err != nil {
		return fmt.Errorf("failed to create directory %s: %w", dstDir, err)
	}

	tmpPath := dst + ".tmp"
	client := &http.Client{
		Timeout: timeout,
	}

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("User-Agent", "Clash/Meta")

	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad status: %s", resp.Status)
	}

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return fmt.Errorf("failed to read body: %w", err)
	}

	if len(data) < 100 {
		return fmt.Errorf("downloaded data too short: %d", len(data))
	}

	if err := os.WriteFile(tmpPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write tmp file: %w", err)
	}

	if err := os.Rename(tmpPath, dst); err != nil {
		return fmt.Errorf("failed to rename tmp file to dst: %w", err)
	}

	fmt.Printf("✅ Downloaded: %s\n", filepath.Base(dst))
	return nil
}

func fetchConfig(url string, timeout time.Duration) ([]byte, error) {
	client := &http.Client{
		Timeout: timeout,
	}

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("User-Agent", "Clash/Meta")

	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("HTTP error: %s", resp.Status)
	}

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read body: %w", err)
	}
	return data, nil
}

func saveYaml(data []byte, path string) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create directory %s: %w", dir, err)
	}

	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("failed to create file %s: %w", path, err)
	}
	defer f.Close()

	if _, err := f.Write(data); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}
	return nil
}

func restartService() error {
	cmd := exec.Command("sudo", "systemctl", "restart", ServiceName)
	fmt.Printf("[service] Restarting service: %s\n", cmd.String())
	if output, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("failed to restart service: %w, output: %s", err, string(output))
	}
	return nil
}

// ==========================================
//              3. Main Program
// ==========================================

func main() {
	fmt.Printf("=== Mihomo Updater: %s ===\n", time.Now().Format("2006-01-02 15:04:05.999999"))

	if err := os.MkdirAll(MihomoDir, 0755); err != nil {
		fmt.Printf("❌ Failed to create mihomo dir: %v\n", err)
		os.Exit(1)
	}

	// 1. Fetch config from Resolver
	fmt.Printf("Fetching config from %s...\n", ResolverURL)
	remoteData, err := fetchConfig(ResolverURL, 30*time.Second)
	if err != nil {
		fmt.Printf("❌ Failed to fetch remote config: %v\n", err)
		os.Exit(1)
	}
	if remoteData == nil {
		fmt.Println("❌ Config is empty")
		os.Exit(1)
	}
	fmt.Println("✅ Remote config fetched successfully")

	// 2. Backup
	if _, err := os.Stat(LocalConfig); err == nil {
		ts := time.Now().Format("20060102-150405")
		backupPath := filepath.Join(MihomoDir, "backup", fmt.Sprintf("config.yaml.bak.%s", ts))
		if err := os.MkdirAll(filepath.Dir(backupPath), 0755); err != nil {
			fmt.Printf("❌ Failed to create backup dir: %v\n", err)
		} else {
			if err := os.Rename(LocalConfig, backupPath); err != nil {
				fmt.Printf("❌ Failed to backup config: %v\n", err)
			}
		}
	}

	// 3. Save new config
	if err := saveYaml(remoteData, LocalConfig); err != nil {
		fmt.Printf("❌ Failed to save local config: %v\n", err)
		os.Exit(1)
	}
	// Original script saves to BACKUP_CONFIG as well
	if err := saveYaml(remoteData, BackupConfig); err != nil {
		fmt.Printf("❌ Failed to save backup config: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("✅ Config and backup saved")

	// 4. Update Databases
	fmt.Println("[db] Checking for database updates...")
	for key, cfg := range DBSources {
		dst := filepath.Join(MihomoDir, cfg.Filename)
		if err := downloadData(cfg.URL, dst, 30*time.Second); err != nil {
			fmt.Printf("⚠️ Database %s update failed: %v, skipping\n", key, err)
		}
	}

	// 5. Restart Service
	fmt.Println("[service] Restarting Mihomo Service...")
	if err := restartService(); err != nil {
		fmt.Printf("⚠️ Service restart failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("=== Completed ===")
}
