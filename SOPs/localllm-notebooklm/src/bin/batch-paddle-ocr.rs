// Batch PaddleOCR using the async job API.
// Workflow v2 step 1: PDF -> markdown/pages/images cache.
use anyhow::{Context, Result, anyhow};
use clap::Parser;
use dotenvy::dotenv;
use localllm_notebooklm::{ApiEnvelope, JobStatusData, PdfManifestRow, SubmitJobData};
use localllm_notebooklm::{
    cleanup_tmp, is_nonempty_file, remove_path_if_exists, sanitize_relative_path, stable_name,
    write_error_log,
};
use reqwest::{Client, multipart};
use serde_json::{Value, json};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::{fs, sync::Semaphore, time::Duration};

const JOB_URL: &str = "https://paddleocr.aistudio-app.com/api/v2/ocr/jobs";
const MODEL: &str = "PaddleOCR-VL-1.6";

#[tokio::main]
async fn main() -> Result<()> {
    dotenv().ok();
    let token = Arc::new(std::env::var("PP_API_KEY").context("PP_API_KEY should be set")?);
    let args = Arc::new(Args::parse_and_validate()?);
    let client = Client::new();

    let rows = read_manifest(&args.manifest).await?;
    println!(
        "Loaded {} PDF tasks from {}",
        rows.len(),
        args.manifest.display()
    );

    let semaphore = Arc::new(Semaphore::new(args.concurrency));
    let mut handles = Vec::with_capacity(rows.len());

    for row in rows {
        let permit = semaphore.clone().acquire_owned().await?;
        let client = client.clone();
        let token = token.clone();
        let args = args.clone();
        handles.push(tokio::spawn(async move {
            let _permit = permit;
            process_with_retry(client, token, args, row).await
        }));
    }

    let mut done = 0usize;
    let mut skipped = 0usize;
    let mut failed = 0usize;

    for handle in handles {
        match handle.await.context("worker task panicked")?? {
            TaskOutcome::Done => done += 1,
            TaskOutcome::Skipped => skipped += 1,
            TaskOutcome::Failed => failed += 1,
        }
    }

    println!("Finished. done={done}, skipped={skipped}, failed={failed}");
    if failed > 0 {
        Err(anyhow!("{failed} task(s) failed"))
    } else {
        Ok(())
    }
}

#[derive(Parser, Debug, Clone)]
#[command(
    name = "batch-paddle-ocr",
    about = "Batch PaddleOCR PDF files from a minimal JSONL manifest"
)]
struct Args {
    #[arg(long, value_name = "PATH")]
    manifest: PathBuf,

    #[arg(long, value_name = "PATH")]
    cache_dir: PathBuf,

    #[arg(long, default_value_t = 4, value_name = "N")]
    concurrency: usize,

    #[arg(long = "poll-interval", default_value_t = 5, value_name = "SECONDS")]
    poll_interval_secs: u64,

    #[arg(long = "retry", default_value_t = 2, value_name = "N")]
    retries: usize,
}

impl Args {
    fn parse_and_validate() -> Result<Self> {
        let args = Self::parse();
        if args.concurrency == 0 {
            return Err(anyhow!("--concurrency must be greater than 0"));
        }
        if args.poll_interval_secs == 0 {
            return Err(anyhow!("--poll-interval must be greater than 0"));
        }
        Ok(args)
    }

    fn poll_interval(&self) -> Duration {
        Duration::from_secs(self.poll_interval_secs)
    }
}

#[derive(Debug)]
enum TaskOutcome {
    Done,
    Skipped,
    Failed,
}

async fn read_manifest(path: &Path) -> Result<Vec<PdfManifestRow>> {
    let content = fs::read_to_string(path)
        .await
        .with_context(|| format!("failed to read manifest {}", path.display()))?;
    let mut rows = Vec::new();

    for (line_index, line) in content.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let row: PdfManifestRow = serde_json::from_str(line).with_context(|| {
            format!(
                "failed to parse manifest line {} in {}",
                line_index + 1,
                path.display()
            )
        })?;
        rows.push(row);
    }

    Ok(rows)
}

async fn process_with_retry(
    client: Client,
    token: Arc<String>,
    args: Arc<Args>,
    row: PdfManifestRow,
) -> Result<TaskOutcome> {
    let output_dir = args.cache_dir.join(&row.id);
    let final_md = output_dir.join("merged.md");

    // Resume is filesystem-based: a non-empty final markdown is the only done state.
    if is_nonempty_file(&final_md).await {
        println!("[skip] {} {}", row.id, row.pdf_name);
        return Ok(TaskOutcome::Skipped);
    }

    for attempt in 0..=args.retries {
        if attempt > 0 {
            println!(
                "[retry {}/{}] {} {}",
                attempt, args.retries, row.id, row.pdf_name
            );
        } else {
            println!("[start] {} {}", row.id, row.pdf_name);
        }

        match process_once(&client, &token, &args, &row, &output_dir).await {
            Ok(()) => {
                println!("[done] {} {}", row.id, row.pdf_name);
                return Ok(TaskOutcome::Done);
            }
            Err(err) => {
                cleanup_tmp(&output_dir).await?;
                write_error_log(&output_dir, &err).await?;
                eprintln!("[error] {} {}: {err:#}", row.id, row.pdf_name);
            }
        }
    }

    Ok(TaskOutcome::Failed)
}

async fn process_once(
    client: &Client,
    token: &str,
    args: &Args,
    row: &PdfManifestRow,
    output_dir: &Path,
) -> Result<()> {
    let tmp_dir = output_dir.join(".tmp");

    // Failures only affect .tmp; the next attempt deletes it and starts from zero.
    cleanup_tmp(output_dir).await?;
    fs::create_dir_all(tmp_dir.join("pages")).await?;
    fs::create_dir_all(tmp_dir.join("images")).await?;

    let job_id = submit_job(client, token, &row.pdf_path).await?;
    println!("[job] {} {} -> {}", row.id, row.pdf_name, job_id);

    let jsonl_url = poll_job(client, token, args.poll_interval(), &job_id, row).await?;
    let jsonl = client
        .get(jsonl_url)
        .send()
        .await
        .context("failed to download PaddleOCR result jsonl")?
        .error_for_status()
        .context("PaddleOCR result jsonl download returned an error status")?
        .text()
        .await
        .context("failed to read PaddleOCR result jsonl response")?;

    fs::write(tmp_dir.join("result.jsonl"), &jsonl).await?;
    let merged = write_pages_and_images(client, &tmp_dir, row, &jsonl).await?;
    fs::write(tmp_dir.join("merged.md"), merged).await?;

    commit_tmp(output_dir).await?;
    Ok(())
}

async fn submit_job(client: &Client, token: &str, pdf_path: &Path) -> Result<String> {
    let file_bytes = fs::read(pdf_path)
        .await
        .with_context(|| format!("failed to read PDF {}", pdf_path.display()))?;
    let file_name = pdf_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("document.pdf")
        .to_string();
    let optional_payload = json!({
        "useDocOrientationClassify": false,
        "useDocUnwarping": false,
        "useChartRecognition": true
    });
    let file_part = multipart::Part::bytes(file_bytes)
        .file_name(file_name)
        .mime_str("application/pdf")?;
    let form = multipart::Form::new()
        .text("model", MODEL)
        .text("optionalPayload", optional_payload.to_string())
        .part("file", file_part);

    let response: ApiEnvelope<SubmitJobData> = client
        .post(JOB_URL)
        .bearer_auth(token)
        .multipart(form)
        .send()
        .await
        .context("failed to submit PaddleOCR job")?
        .error_for_status()
        .context("PaddleOCR job submission returned an error status")?
        .json()
        .await
        .context("failed to parse PaddleOCR job submission response")?;

    Ok(response.data.job_id)
}

async fn poll_job(
    client: &Client,
    token: &str,
    poll_interval: Duration,
    job_id: &str,
    row: &PdfManifestRow,
) -> Result<String> {
    loop {
        let url = format!("{JOB_URL}/{job_id}");
        let response: ApiEnvelope<JobStatusData> = client
            .get(url)
            .bearer_auth(token)
            .send()
            .await
            .context("failed to poll PaddleOCR job")?
            .error_for_status()
            .context("PaddleOCR job polling returned an error status")?
            .json()
            .await
            .context("failed to parse PaddleOCR job polling response")?;

        match response.data.state.as_str() {
            "pending" => {
                println!("[pending] {} {}", row.id, row.pdf_name);
            }
            "running" => {
                if let Some(progress) = response.data.extract_progress {
                    println!(
                        "[running] {} {} {}/{}",
                        row.id,
                        row.pdf_name,
                        progress.extracted_pages.unwrap_or(0),
                        progress.total_pages.unwrap_or(0)
                    );
                } else {
                    println!("[running] {} {}", row.id, row.pdf_name);
                }
            }
            "done" => {
                let json_url = response
                    .data
                    .result_url
                    .ok_or_else(|| anyhow!("PaddleOCR job is done but resultUrl is missing"))?
                    .json_url;
                return Ok(json_url);
            }
            "failed" => {
                return Err(anyhow!(
                    "PaddleOCR job failed: {}",
                    response
                        .data
                        .error_msg
                        .unwrap_or_else(|| "unknown error".to_string())
                ));
            }
            other => {
                return Err(anyhow!("unknown PaddleOCR job state: {other}"));
            }
        }

        tokio::time::sleep(poll_interval).await;
    }
}

async fn write_pages_and_images(
    client: &Client,
    tmp_dir: &Path,
    row: &PdfManifestRow,
    jsonl: &str,
) -> Result<String> {
    let mut page_no = 1usize;
    let mut merged = String::new();

    merged.push_str("---\n");
    merged.push_str(&format!("id: {}\n", row.id));
    merged.push_str(&format!("pdf_name: {}\n", row.pdf_name));
    merged.push_str(&format!("pdf_path: {}\n", row.pdf_path.display()));
    merged.push_str(&format!("ocr_provider: {}\n", MODEL));
    merged.push_str("---\n\n");
    merged.push_str(&format!("# {}\n\n", row.pdf_name));

    for (line_index, line) in jsonl.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let value: Value = serde_json::from_str(line)
            .with_context(|| format!("failed to parse result jsonl line {}", line_index + 1))?;
        let results = value
            .pointer("/result/layoutParsingResults")
            .and_then(Value::as_array)
            .ok_or_else(|| {
                anyhow!(
                    "result jsonl line {} has no result.layoutParsingResults array",
                    line_index + 1
                )
            })?;

        for res in results {
            let text = res
                .pointer("/markdown/text")
                .and_then(Value::as_str)
                .unwrap_or("")
                .trim();
            let markdown_images = download_markdown_images(client, tmp_dir, res).await?;
            let page_text = rewrite_markdown_image_paths(text, &markdown_images, "../");
            let merged_text = rewrite_markdown_image_paths(text, &markdown_images, "");

            let page_md = format!(
                "---\nid: {}\npdf_name: {}\npage_no: {}\n---\n\n{}\n",
                row.id, row.pdf_name, page_no, page_text
            );
            fs::write(
                tmp_dir.join("pages").join(format!("page_{page_no:04}.md")),
                page_md,
            )
            .await?;

            merged.push_str(&format!("<!-- PAGE {page_no:04} -->\n\n"));
            merged.push_str(&merged_text);
            merged.push_str("\n\n");

            download_output_images(client, tmp_dir, res, page_no).await?;
            page_no += 1;
        }
    }

    if page_no == 1 {
        return Err(anyhow!("PaddleOCR result contains no pages"));
    }

    Ok(merged)
}

async fn download_markdown_images(
    client: &Client,
    tmp_dir: &Path,
    res: &Value,
) -> Result<Vec<(String, String)>> {
    let Some(images) = res.pointer("/markdown/images").and_then(Value::as_object) else {
        return Ok(Vec::new());
    };

    let mut replacements = Vec::with_capacity(images.len());
    for (image_path, image_url) in images {
        let Some(image_url) = image_url.as_str() else {
            continue;
        };
        let safe_path = sanitize_relative_path(image_path)
            .unwrap_or_else(|| PathBuf::from(stable_name(image_path)));
        let stored_path = PathBuf::from("images").join("markdown").join(safe_path);
        let full_path = tmp_dir.join(&stored_path);
        download_file(client, image_url, &full_path).await?;
        replacements.push((
            image_path.to_string(),
            stored_path.to_string_lossy().replace('\\', "/"),
        ));
    }

    Ok(replacements)
}

fn rewrite_markdown_image_paths(
    text: &str,
    replacements: &[(String, String)],
    prefix: &str,
) -> String {
    let mut rewritten = text.to_string();
    for (original, stored) in replacements {
        let target = format!("{prefix}{stored}");
        rewritten = rewritten.replace(original, &target);
    }
    rewritten
}

async fn download_output_images(
    client: &Client,
    tmp_dir: &Path,
    res: &Value,
    page_no: usize,
) -> Result<()> {
    let Some(images) = res.pointer("/outputImages").and_then(Value::as_object) else {
        return Ok(());
    };

    for (image_name, image_url) in images {
        let Some(image_url) = image_url.as_str() else {
            continue;
        };
        let file_name = format!("{}_page_{page_no:04}.jpg", stable_name(image_name));
        let full_path = tmp_dir.join("images").join("output").join(file_name);
        download_file(client, image_url, &full_path).await?;
    }

    Ok(())
}

async fn download_file(client: &Client, url: &str, path: &Path) -> Result<()> {
    let bytes = client
        .get(url)
        .send()
        .await
        .with_context(|| format!("failed to download image {url}"))?
        .error_for_status()
        .with_context(|| format!("image download returned an error status: {url}"))?
        .bytes()
        .await
        .with_context(|| format!("failed to read image response: {url}"))?;

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).await?;
    }
    fs::write(path, bytes).await?;
    Ok(())
}

async fn commit_tmp(output_dir: &Path) -> Result<()> {
    let tmp_dir = output_dir.join(".tmp");
    fs::create_dir_all(output_dir).await?;

    // Derived artifacts are replaced only after the whole OCR task succeeds.
    remove_path_if_exists(&output_dir.join("pages")).await?;
    remove_path_if_exists(&output_dir.join("images")).await?;
    remove_path_if_exists(&output_dir.join("result.jsonl")).await?;
    remove_path_if_exists(&output_dir.join("merged.md")).await?;

    fs::rename(tmp_dir.join("pages"), output_dir.join("pages")).await?;
    fs::rename(tmp_dir.join("images"), output_dir.join("images")).await?;
    fs::rename(
        tmp_dir.join("result.jsonl"),
        output_dir.join("result.jsonl"),
    )
    .await?;
    fs::rename(tmp_dir.join("merged.md"), output_dir.join("merged.md")).await?;
    remove_path_if_exists(&output_dir.join("error.log")).await?;
    remove_path_if_exists(&tmp_dir).await?;
    Ok(())
}
