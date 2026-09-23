use std::process::ExitCode;

#[tokio::main]
async fn main() -> ExitCode {
    match course_election::app::run_daemon().await {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error:#}");
            ExitCode::FAILURE
        }
    }
}
