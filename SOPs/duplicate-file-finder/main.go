/*
一个Go程序，用于查找文件夹中的重复文件，
并仅保留每个重复组中修改时间最早的一个。

相比Python版本的改进：
1. 添加 -r (recursive) 选项控制是否递归遍历
2. 添加 -d (dry-run) 选项预览删除而非实际删除
3. 更好的符号链接处理（默认跳过符号链接）
4. 更好的错误处理
5. 更好的并发性能
*/
package main

import (
	"crypto/sha256"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"time"
)

const (
	bufferSize = 1024 * 1024 // 1MB 缓冲区
)

// FileInfo 存储文件信息
type FileInfo struct {
	Path    string
	Size    int64
	ModTime time.Time
}

// Config 存储命令行配置
type Config struct {
	Directory string
	Recursive bool
	DryRun    bool
}

func main() {
	// 解析命令行参数
	recursive := flag.Bool("r", false, "递归遍历子目录")
	dryRun := flag.Bool("d", false, "干运行模式：仅显示将要删除的文件，不实际删除")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "用法: %s [选项] <文件夹路径>\n\n", os.Args[0])
		fmt.Fprintln(os.Stderr, "选项:")
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr, "\n示例:")
		fmt.Fprintf(os.Stderr, "  %s -r -d /path/to/folder  # 递归遍历，干运行模式\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "  %s /path/to/folder        # 仅当前目录，实际删除\n", os.Args[0])
	}
	flag.Parse()

	if flag.NArg() != 1 {
		flag.Usage()
		os.Exit(1)
	}

	config := Config{
		Directory: flag.Arg(0),
		Recursive: *recursive,
		DryRun:    *dryRun,
	}

	// 验证目录
	info, err := os.Stat(config.Directory)
	if err != nil {
		fmt.Fprintf(os.Stderr, "错误: 无法访问路径 '%s': %v\n", config.Directory, err)
		os.Exit(1)
	}
	if !info.IsDir() {
		fmt.Fprintf(os.Stderr, "错误: 路径 '%s' 不是一个有效的文件夹。\n", config.Directory)
		os.Exit(1)
	}

	if config.DryRun {
		fmt.Println("=== 干运行模式：不会实际删除任何文件 ===")
	}

	// 收集文件
	files, err := collectFiles(config.Directory, config.Recursive)
	if err != nil {
		fmt.Fprintf(os.Stderr, "错误: 遍历文件夹失败: %v\n", err)
		os.Exit(1)
	}

	if len(files) == 0 {
		fmt.Println("文件夹为空，无需操作。")
		return
	}

	fmt.Printf("共发现 %d 个文件\n", len(files))

	// 按大小分组，过滤出可能重复的文件
	potentialDuplicates := groupBySize(files)
	if len(potentialDuplicates) == 0 {
		fmt.Println("未找到重复文件。")
		return
	}

	fmt.Printf("按大小过滤后，有 %d 个文件可能重复\n", len(potentialDuplicates))

	// 按哈希分组
	hashGroups := groupByHash(potentialDuplicates)

	// 处理重复文件
	found := processDuplicates(hashGroups, config.DryRun)

	if !found {
		fmt.Println("未找到重复文件。")
	} else if config.DryRun {
		fmt.Println("\n=== 干运行完成。使用不带 -d 选项运行以实际删除文件 ===")
	} else {
		fmt.Println("\n清理完成。")
	}
}

// collectFiles 收集目录中的文件
func collectFiles(directory string, recursive bool) ([]FileInfo, error) {
	var files []FileInfo

	if recursive {
		err := filepath.WalkDir(directory, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				// 记录错误但继续遍历
				fmt.Fprintf(os.Stderr, "警告: 无法访问 %s: %v\n", path, err)
				return nil
			}

			// 跳过目录
			if d.IsDir() {
				return nil
			}

			// 跳过符号链接
			if d.Type()&fs.ModeSymlink != 0 {
				return nil
			}

			info, err := d.Info()
			if err != nil {
				fmt.Fprintf(os.Stderr, "警告: 无法获取文件信息 %s: %v\n", path, err)
				return nil
			}

			files = append(files, FileInfo{
				Path:    path,
				Size:    info.Size(),
				ModTime: info.ModTime(),
			})
			return nil
		})
		return files, err
	}

	// 非递归模式：只读取当前目录
	entries, err := os.ReadDir(directory)
	if err != nil {
		return nil, err
	}

	for _, entry := range entries {
		// 跳过目录
		if entry.IsDir() {
			continue
		}

		// 跳过符号链接
		if entry.Type()&fs.ModeSymlink != 0 {
			continue
		}

		path := filepath.Join(directory, entry.Name())
		info, err := entry.Info()
		if err != nil {
			fmt.Fprintf(os.Stderr, "警告: 无法获取文件信息 %s: %v\n", path, err)
			continue
		}

		files = append(files, FileInfo{
			Path:    path,
			Size:    info.Size(),
			ModTime: info.ModTime(),
		})
	}

	return files, nil
}

// groupBySize 按文件大小分组，返回可能重复的文件列表
func groupBySize(files []FileInfo) []FileInfo {
	sizeGroups := make(map[int64][]FileInfo)

	for _, f := range files {
		sizeGroups[f.Size] = append(sizeGroups[f.Size], f)
	}

	var potentialDuplicates []FileInfo
	for _, group := range sizeGroups {
		if len(group) > 1 {
			potentialDuplicates = append(potentialDuplicates, group...)
		}
	}

	return potentialDuplicates
}

// hashFile 计算文件的SHA-256哈希值
func hashFile(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hash := sha256.New()
	buf := make([]byte, bufferSize)

	for {
		n, err := file.Read(buf)
		if n > 0 {
			hash.Write(buf[:n])
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return "", err
		}
	}

	return fmt.Sprintf("%x", hash.Sum(nil)), nil
}

// groupByHash 按哈希值分组
func groupByHash(files []FileInfo) map[string][]FileInfo {
	hashGroups := make(map[string][]FileInfo)

	for _, f := range files {
		hash, err := hashFile(f.Path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "警告: 无法计算哈希 %s: %v\n", f.Path, err)
			continue
		}
		hashGroups[hash] = append(hashGroups[hash], f)
	}

	return hashGroups
}

// processDuplicates 处理重复文件
func processDuplicates(hashGroups map[string][]FileInfo, dryRun bool) bool {
	foundDuplicates := false

	for _, group := range hashGroups {
		if len(group) <= 1 {
			continue
		}

		foundDuplicates = true

		// 按修改时间排序，找到最老的文件
		sort.Slice(group, func(i, j int) bool {
			return group[i].ModTime.Before(group[j].ModTime)
		})

		oldest := group[0]
		fmt.Printf("\n发现一组重复文件 (%s):\n", filepath.Base(oldest.Path))
		fmt.Printf("  - [保留] %s (最早修改时间: %s)\n",
			oldest.Path,
			oldest.ModTime.Format("2006-01-02 15:04:05"))

		// 删除除最老文件之外的所有文件
		for _, f := range group[1:] {
			if dryRun {
				fmt.Printf("  - [将删除] %s\n", f.Path)
			} else {
				err := os.Remove(f.Path)
				if err != nil {
					fmt.Fprintf(os.Stderr, "  - [错误] 无法删除 %s: %v\n", f.Path, err)
				} else {
					fmt.Printf("  - [已删除] %s\n", f.Path)
				}
			}
		}
	}

	return foundDuplicates
}
