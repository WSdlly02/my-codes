use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;
use std::time::SystemTime;

use chrono::{DateTime, Local};
use clap::Parser;
use hex;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

const BUFFER_SIZE: usize = 1024 * 1024; // 1MB

#[derive(Parser, Debug)]
#[command(
    name = "dff",
    about = "查找文件夹中的重复文件，并仅保留每组中修改时间最早的一个",
    after_help = "示例:\n  dff -r -d /path/to/folder  # 递归遍历，干运行模式\n  dff /path/to/folder        # 仅当前目录，实际删除"
)]
struct Cli {
    #[arg(short = 'r', long = "recursive", help = "递归遍历子目录")]
    recursive: bool,

    #[arg(
        short = 'd',
        long = "dry-run",
        help = "干运行模式：仅显示将要删除的文件，不实际删除"
    )]
    dry_run: bool,

    #[arg(value_name = "文件夹路径")]
    directory: PathBuf,
}

#[derive(Clone, Debug)]
struct FileInfo {
    path: PathBuf,
    size: u64,
    mod_time: SystemTime,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("错误: {err}");
        process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let cli = Cli::parse();

    let metadata = fs::metadata(&cli.directory)
        .map_err(|err| format!("无法访问路径 '{}': {err}", cli.directory.display()))?;
    if !metadata.is_dir() {
        return Err(format!(
            "路径 '{}' 不是一个有效的文件夹",
            cli.directory.display()
        ));
    }

    if cli.dry_run {
        println!("=== 干运行模式：不会实际删除任何文件 ===");
    }

    let files = collect_files(&cli.directory, cli.recursive);
    if files.is_empty() {
        println!("文件夹为空，无需操作。");
        return Ok(());
    }

    println!("共发现 {} 个文件", files.len());

    let potential_duplicates = group_by_size(files);
    if potential_duplicates.is_empty() {
        println!("未找到重复文件。");
        return Ok(());
    }

    println!(
        "按大小过滤后，有 {} 个文件可能重复",
        potential_duplicates.len()
    );

    let hash_groups = group_by_hash(potential_duplicates);
    let found = process_duplicates(hash_groups, cli.dry_run);

    if !found {
        println!("未找到重复文件。");
    } else if cli.dry_run {
        println!("\n=== 干运行完成。使用不带 -d 选项运行以实际删除文件 ===");
    } else {
        println!("\n清理完成。");
    }

    Ok(())
}

fn collect_files(directory: &Path, recursive: bool) -> Vec<FileInfo> {
    if recursive {
        collect_files_recursive(directory)
    } else {
        collect_files_flat(directory)
    }
}

fn collect_files_recursive(directory: &Path) -> Vec<FileInfo> {
    let mut files = Vec::new();

    for entry in WalkDir::new(directory) {
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => {
                eprintln!("警告: 无法访问目录项: {err}");
                continue;
            }
        };

        let file_type = entry.file_type();
        if file_type.is_symlink() || !file_type.is_file() {
            continue;
        }

        match entry.metadata() {
            Ok(metadata) => files.push(FileInfo {
                path: entry.into_path(),
                size: metadata.len(),
                mod_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
            }),
            Err(err) => eprintln!("警告: 无法获取文件信息 '{}': {err}", entry.path().display()),
        }
    }

    files
}

fn collect_files_flat(directory: &Path) -> Vec<FileInfo> {
    let mut files = Vec::new();

    let entries = match fs::read_dir(directory) {
        Ok(entries) => entries,
        Err(err) => {
            eprintln!("警告: 无法读取目录 '{}': {err}", directory.display());
            return files;
        }
    };

    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => {
                eprintln!("警告: 读取目录项失败 '{}': {err}", directory.display());
                continue;
            }
        };

        let file_type = match entry.file_type() {
            Ok(file_type) => file_type,
            Err(err) => {
                eprintln!("警告: 无法获取文件类型 '{}': {err}", entry.path().display());
                continue;
            }
        };

        if file_type.is_symlink() || !file_type.is_file() {
            continue;
        }

        match entry.metadata() {
            Ok(metadata) => files.push(FileInfo {
                path: entry.path(),
                size: metadata.len(),
                mod_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
            }),
            Err(err) => eprintln!("警告: 无法获取文件信息 '{}': {err}", entry.path().display()),
        }
    }

    files
}

fn group_by_size(files: Vec<FileInfo>) -> Vec<FileInfo> {
    let mut size_groups: HashMap<u64, Vec<FileInfo>> = HashMap::new();
    for file in files {
        size_groups.entry(file.size).or_default().push(file);
    }

    size_groups
        .into_values()
        .filter(|group| group.len() > 1) // filter out duplicates
        .flatten()
        .collect()
}

fn group_by_hash(files: Vec<FileInfo>) -> HashMap<String, Vec<FileInfo>> {
    let mut hash_groups: HashMap<String, Vec<FileInfo>> = HashMap::new();

    for file in files {
        match hash_file(&file.path) {
            Ok(hash) => {
                hash_groups.entry(hash).or_default().push(file);
            }
            Err(err) => eprintln!("警告: 无法计算哈希 '{}': {err}", file.path.display()),
        }
    }

    hash_groups
}

fn hash_file(path: &Path) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; BUFFER_SIZE];

    while let Ok(n) = file.read(&mut buffer) {
        if n == 0 {
            break;
        }
        hasher.update(&buffer[..n]);
    }

    let result = hasher.finalize();

    Ok(hex::encode(result))
}

fn process_duplicates(hash_groups: HashMap<String, Vec<FileInfo>>, dry_run: bool) -> bool {
    let mut duplicate_groups: Vec<Vec<FileInfo>> = hash_groups
        .into_values()
        .filter(|group| group.len() > 1)
        .collect();

    duplicate_groups.sort_by_key(|group| {
        let mut sorted = group.clone();
        sort_group_by_mod_time(&mut sorted);
        let oldest = &sorted[0];
        (oldest.mod_time, oldest.path.clone())
    });

    let mut found_duplicates = false;

    for mut group in duplicate_groups {
        found_duplicates = true;
        sort_group_by_mod_time(&mut group);

        let oldest = &group[0];
        println!("\n发现一组重复文件 ({}):", display_name(&oldest.path));
        println!(
            "  - [保留] {} (最早修改时间: {})",
            oldest.path.display(),
            format_system_time(oldest.mod_time)
        );

        for file in &group[1..] {
            if dry_run {
                println!("  - [将删除] {}", file.path.display());
            } else {
                match fs::remove_file(&file.path) {
                    Ok(()) => println!("  - [已删除] {}", file.path.display()),
                    Err(err) => eprintln!("  - [错误] 无法删除 {}: {err}", file.path.display()),
                }
            }
        }
    }

    found_duplicates
}

fn sort_group_by_mod_time(group: &mut [FileInfo]) {
    group.sort_by_key(|file| (file.mod_time, file.path.clone()));
}

fn display_name(path: &Path) -> String {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| path.display().to_string())
}

fn format_system_time(time: SystemTime) -> String {
    DateTime::<Local>::from(time)
        .format("%Y-%m-%d %H:%M:%S")
        .to_string()
}
