use futures::future::join_all;
use rand::prelude::SliceRandom;
use reqwest::Response;
use std::{collections::HashMap, sync::LazyLock, time::Duration};
use tokio::task;

// 阿里 DNS: https://223.5.5.5/resolve
// DNSPod: https://1.12.12.12/resolve
const DOH_RESOLVE_BASE: &str = "https://223.5.5.5/resolve";
// 查询域名的数量
const QUERY_COUNT: usize = 20;
// 统计区间大小（毫秒）
const STATS_INTERVAL: u128 = 50;

#[tokio::main]
async fn main() {
    LazyLock::force(&DOMAINS);
    let domains = (*DOMAINS)
        .choose_multiple(&mut rand::thread_rng(), QUERY_COUNT)
        .collect::<Vec<_>>();
    let mut handles = vec![];
    for domain in domains {
        let handle = task::spawn(query(domain));
        handles.push(handle);
    }

    let results = join_all(handles).await;
    let mut timeout_count = 0;
    let mut linechart_inputs = HashMap::new();

    for r in results {
        let (elapsed, r) = r.unwrap();
        match r {
            Ok(_resp) => {
                let elapsed_ms = elapsed.as_millis();
                let begin = (elapsed_ms / STATS_INTERVAL) * STATS_INTERVAL;
                let count = linechart_inputs.entry(begin).or_insert(0);
                *count += 1;
            }
            Err(e) => {
                if e.is_timeout() {
                    timeout_count += 1;
                }
            }
        }
    }
    let mut keys: Vec<_> = linechart_inputs.keys().collect();
    // 按大小顺序排列 `linechart_inputs` 的键,并打印`键:值``
    keys.sort();
    println!("Line Chart inputs:\n");
    for key in keys {
        println!("{}: {}", key, linechart_inputs.get(key).unwrap());
    }

    println!("\nTimeout count: {}", timeout_count);
}

static DOMAINS: LazyLock<Vec<String>> = LazyLock::new(|| {
    // 读取 domains.txt 文件,每一行一个域名.
    let content = std::fs::read_to_string("domains.txt")
        .unwrap()
        .trim()
        .to_owned();
    content.lines().map(|line| line.to_string()).collect()
});

async fn query(domain: &str) -> (Duration, Result<Response, reqwest::Error>) {
    let url = format!("{}?name={}&type=A", DOH_RESOLVE_BASE, domain);
    let start = std::time::Instant::now();
    let r = reqwest::get(&url).await;
    let elapsed = start.elapsed();

    (elapsed, r)
}
