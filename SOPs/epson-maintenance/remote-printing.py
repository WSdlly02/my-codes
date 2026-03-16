import mimetypes
import os
import smtplib
import ssl
import sys
from email.message import EmailMessage
from pathlib import Path


def require_env(name: str) -> str:
    value = os.environ.get(name)
    if not value:
        print(f"ERROR: missing environment variable: {name}", file=sys.stderr)
        sys.exit(2)
    return value


def main() -> int:
    smtp_host = require_env("SMTP_HOST")
    smtp_port = int(require_env("SMTP_PORT"))
    smtp_mode = require_env("SMTP_MODE")  # ssl or starttls
    smtp_user = require_env("SMTP_USER")
    smtp_pass = require_env("SMTP_PASS")
    printer_email = require_env("PRINTER_EMAIL")
    pdf_path = Path(require_env("PDF_PATH")).expanduser().resolve()

    if not pdf_path.exists():
        print(f"ERROR: PDF not found: {pdf_path}", file=sys.stderr)
        return 1

    msg = EmailMessage()
    msg["From"] = smtp_user
    msg["To"] = printer_email
    msg["Subject"] = "epson-maintenance"

    # 正文尽量留空
    msg.set_content("")

    ctype, encoding = mimetypes.guess_type(str(pdf_path))
    if ctype is None or encoding is not None:
        ctype = "application/pdf"
    maintype, subtype = ctype.split("/", 1)

    with pdf_path.open("rb") as f:
        msg.add_attachment(
            f.read(),
            maintype=maintype,
            subtype=subtype,
            filename=pdf_path.name,
        )

    context = ssl.create_default_context()

    try:
        if smtp_mode == "ssl":
            with smtplib.SMTP_SSL(
                smtp_host, smtp_port, context=context, timeout=30
            ) as server:
                server.login(smtp_user, smtp_pass)
                server.send_message(msg)
        elif smtp_mode == "starttls":
            with smtplib.SMTP(smtp_host, smtp_port, timeout=30) as server:
                server.ehlo()
                server.starttls(context=context)
                server.ehlo()
                server.login(smtp_user, smtp_pass)
                server.send_message(msg)
        else:
            print("ERROR: SMTP_MODE must be 'ssl' or 'starttls'", file=sys.stderr)
            return 2
    except Exception as e:
        print(f"ERROR: failed to send email: {e}", file=sys.stderr)
        return 1

    print("OK: maintenance email sent")
    print(f"To:  {printer_email}")
    print(f"PDF: {pdf_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
