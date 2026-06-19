use std::{env, path::PathBuf, process};

use lettre::{
    Message, SmtpTransport, Transport,
    message::{Attachment, MultiPart, header::ContentType},
    transport::smtp::authentication::Credentials,
};

struct Config {
    smtp_host: String,
    smtp_user: String,
    smtp_pass: String,
    printer_email: String,
    pdf_path: PathBuf,
}

fn require_env(name: &str) -> String {
    env::var(name).unwrap_or_else(|_| {
        eprintln!("ERROR: missing environment variable: {name}");
        process::exit(2);
    })
}

fn main() {
    let config = Config {
        smtp_host: require_env("SMTP_HOST"),
        smtp_user: require_env("SMTP_USER"),
        smtp_pass: require_env("SMTP_PASS"),
        printer_email: require_env("PRINTER_EMAIL"),
        pdf_path: PathBuf::from(require_env("PDF_PATH")),
    };

    let pdf_path = &config.pdf_path;

    if !pdf_path.exists() {
        eprintln!("ERROR: PDF not found: {}", pdf_path.display());
        process::exit(1);
    }

    let pdf_bytes = std::fs::read(&pdf_path).unwrap_or_else(|e| {
        eprintln!("ERROR: failed to read PDF: {e}");
        process::exit(1);
    });

    let filename = pdf_path.file_name().unwrap().to_string_lossy().into_owned();

    let email = Message::builder()
        .from(config.smtp_user.parse().unwrap())
        .to(config.printer_email.parse().unwrap())
        .subject("epson-maintenance")
        .multipart(
            MultiPart::mixed().singlepart(
                Attachment::new(filename)
                    .body(pdf_bytes, ContentType::parse("application/pdf").unwrap()),
            ),
        )
        .unwrap();

    let creds = Credentials::new(config.smtp_user, config.smtp_pass);

    // Gmail: STARTTLS on port 587
    let mailer = SmtpTransport::starttls_relay(&config.smtp_host)
        .unwrap()
        .credentials(creds)
        .build();

    match mailer.send(&email) {
        Ok(_) => {
            println!("OK: maintenance email sent");
            println!("To: {}", config.printer_email);
            println!("PDF: {}", pdf_path.display());
        }
        Err(e) => {
            eprintln!("ERROR: failed to send email: {e}");
            process::exit(1);
        }
    }
}
