package install

import (
	"archive/zip"
	"etctl/config"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// getArch 将 Go 的 runtime.GOARCH 转换为 EasyTier 发行版使用的架构名称
func getArch() string {
	switch runtime.GOARCH {
	case "amd64":
		return "x86_64"
	case "arm64":
		return "aarch64"
	case "386":
		return "i686"
	case "arm":
		return "arm"
	default:
		return runtime.GOARCH
	}
}

// DetectEasyTierExist 检查当前目录下是否已存在指定版本的 EasyTier 核心程序
func DetectEasyTierExist() (string, bool) {
	exe := "easytier-core"
	if runtime.GOOS == "windows" {
		exe = "easytier-core.exe"
	}
	path := fmt.Sprintf(
		"easytier-%s-%s-%s/%s",
		runtime.GOOS,
		getArch(),
		config.AppConfig.EasyTierVersion,
		exe)
	_, err := os.Stat(path)
	return path, err == nil
}

// InstallEasyTier 根据配置的版本和系统环境，从 GitHub 下载并安装 EasyTier
func InstallEasyTier() error {
	// https://github.com/EasyTier/EasyTier/releases/download/v2.4.5/easytier-windows-x86_64-v2.4.5.zip
	// https://github.com/EasyTier/EasyTier/releases/download/v2.4.5/easytier-linux-x86_64-v2.4.5.zip
	url := fmt.Sprintf(
		"https://github.com/easytier/easytier/releases/download/%s/easytier-%s-%s-%s.zip",
		config.AppConfig.EasyTierVersion,
		runtime.GOOS,
		getArch(),
		config.AppConfig.EasyTierVersion)
	fmt.Printf("Downloading %s...\n", url)
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("Download failed: %d", resp.StatusCode)
	}

	// 创建临时文件来保存 zip
	tmpFile, err := os.CreateTemp("", "easytier-*.zip")
	if err != nil {
		return err
	}
	defer os.Remove(tmpFile.Name())
	defer tmpFile.Close()

	if _, err := io.Copy(tmpFile, resp.Body); err != nil {
		return fmt.Errorf("Save file failed: %w", err)
	}

	fmt.Println("Download completed, unzipping...")
	return unzip(tmpFile.Name(), ".")
}

// unzip 解压 zip 文件到目标目录，并将内部的 easytier-{OS}-{ARCH} 文件夹重命名为带版本的名称
func unzip(src string, dest string) error {
	zipReadCloser, err := zip.OpenReader(src)
	if err != nil {
		return err
	}
	defer zipReadCloser.Close()

	// 将目标目录转换为绝对路径
	destAbs, err := filepath.Abs(dest)
	if err != nil {
		return err
	}

	oldBrand := fmt.Sprintf("easytier-%s-%s", runtime.GOOS, getArch())
	newBrand := fmt.Sprintf("easytier-%s-%s-%s", runtime.GOOS, getArch(), config.AppConfig.EasyTierVersion)

	for _, zipFile := range zipReadCloser.File {
		// zipFile.Name 的值是 zip 文件中的文件路径，例如：easytier-linux-x86_64-v2.4.5/easytier-core
		relPath := zipFile.Name
		if strings.HasPrefix(relPath, oldBrand) {
			// 替换文件夹名称
			// 例如：easytier-linux-x86_64/easytier-core -> easytier-linux-x86_64-v2.4.5/easytier-core
			relPath = newBrand + relPath[len(oldBrand):]
		}

		fpath := filepath.Join(destAbs, relPath)

		// 正确的安全检查：使用绝对路径
		if !strings.HasPrefix(
			fpath,
			filepath.Clean(destAbs)+string(os.PathSeparator),
		) {
			return fmt.Errorf("Invalid file path: %s", fpath)
		}

		// 若是目录，创建目录
		if zipFile.FileInfo().IsDir() {
			if err := os.MkdirAll(fpath, os.ModePerm); err != nil {
				return err
			}
			continue // 是文件夹，跳过循环
		}

		// 确保父目录存在
		if err = os.MkdirAll(filepath.Dir(fpath), os.ModePerm); err != nil {
			return err
		}

		// 创建文件
		outFile, err := os.OpenFile(fpath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, zipFile.Mode())
		if err != nil {
			return err
		}

		// 打开 zip 文件中的文件
		zipFileReadCloser, err := zipFile.Open()
		if err != nil {
			outFile.Close() // 出错，提前关闭文件
			return err
		}

		// 将 zip 文件中的文件复制到目标文件
		_, err = io.Copy(outFile, zipFileReadCloser)
		outFile.Close()
		zipFileReadCloser.Close() // 在遍历中，手动关闭

		if err != nil {
			return err
		}
	}
	return nil
}
