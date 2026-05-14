use course_election::app;
use std::process::ExitCode;

fn main() -> ExitCode {
    match app::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::from(1)
        }
    }
}
