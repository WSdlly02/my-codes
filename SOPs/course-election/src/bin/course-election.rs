use course_election::app;
use std::process::ExitCode;

#[tokio::main]
async fn main() -> ExitCode {
    match app::run().await {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::from(1)
        }
    }
}
