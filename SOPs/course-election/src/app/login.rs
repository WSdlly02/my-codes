use anyhow::{Context, Result, anyhow, bail};
mod captcha;
mod credentials;
pub(crate) use credentials::Credentials;
mod form;
use captcha::CaptchaSolver;
use form::{LoginStep, parse_form, totp, totp_answer};
use reqwest::Url;
use reqwest::header::{LOCATION, ORIGIN, REFERER};
use serde::Deserialize;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::app::http::Session;
use crate::app::support::{BASE_URL, now_ms};

pub(crate) const CAS_HOST: &str = "sso.shmtu.edu.cn";
const JWXT_HOST: &str = "jwxt.shmtu.edu.cn";
const MAX_REDIRECTS: usize = 10;
const CAPTCHA_ATTEMPTS: usize = 3;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CaptchaChallenge {
    image: String,
    token: String,
    expires_at: i64,
}

pub(crate) async fn login(credentials: Credentials) -> Result<Session> {
    let generator = totp(&credentials.totp_base32)?;
    let solver = CaptchaSolver::new(credentials.api_key)?;
    let session = Session::empty()?;
    let entry = session
        .client()
        .get(format!("{BASE_URL}/home.action"))
        .send()
        .await?;
    if !entry.status().is_redirection() {
        bail!("教务入口未跳转到 CAS（HTTP {}）", entry.status());
    }
    let cas = checked_location(entry.url(), entry.headers().get(LOCATION))?;
    let service = validate_cas_login_url(&cas)?;
    let mut response = session
        .client()
        .get(cas)
        .send()
        .await
        .map_err(reqwest::Error::without_url)?;
    let mut captcha_attempts = 0;
    let mut totp_attempts = 0;
    let mut previous_step = None;
    let mut redirects = 1;
    let mut form_steps = 0;

    loop {
        let url = response.url().clone();
        if response.status().is_redirection() {
            let next = checked_location(&url, response.headers().get(LOCATION))?;
            if url.host_str() == Some(CAS_HOST) {
                if next.host_str() == Some(JWXT_HOST) {
                    validate_ticket_callback(&next, &service)?;
                } else if next.query_pairs().any(|(key, _)| key == "service")
                    && validate_cas_login_url(&next)? != service
                {
                    bail!("CAS 跳转改变了教务 service");
                }
            } else if next.host_str() == Some(CAS_HOST) {
                bail!("认证回调再次跳转到 CAS，教务登录失败");
            }
            redirects += 1;
            if redirects > MAX_REDIRECTS {
                bail!("登录跳转超过 {MAX_REDIRECTS} 次");
            }
            response = session
                .client()
                .get(next)
                .send()
                .await
                .map_err(reqwest::Error::without_url)?;
            continue;
        }

        let status = response.status();
        if status == reqwest::StatusCode::UNAUTHORIZED && url.host_str() == Some(CAS_HOST) {
            bail!("CAS 拒绝认证（HTTP 401）");
        }
        let page = response.text().await.map_err(reqwest::Error::without_url)?;
        if let Some(reason) = login_page_error(status, &page) {
            let path = write_temp_file("login-error", "html", page.as_bytes())?;
            bail!(
                "{reason}: {status} {}，响应已保存到 {}",
                safe_url(&url),
                path.display()
            );
        }
        if url.host_str() == Some(JWXT_HOST) {
            if !session.is_session_valid().await {
                bail!("CAS 已返回 ticket，但教务系统登录态验证失败");
            }
            session.persist_cookies()?;
            return Ok(session);
        }
        form_steps += 1;
        if form_steps > 12 {
            bail!("CAS 认证步骤过多");
        }
        let mut form = parse_form(&url, &page)?;
        match form.step {
            LoginStep::Password => {
                if captcha_attempts > 0 && !is_captcha_rejection(&url) {
                    bail!("CAS 拒绝用户名或密码");
                }
                if captcha_attempts == CAPTCHA_ATTEMPTS {
                    bail!("验证码连续 {CAPTCHA_ATTEMPTS} 次识别错误");
                }
                captcha_attempts += 1;
                let challenge = fetch_captcha(&session, &url).await?;
                let answer = solver.solve(&challenge.image).await?;
                if now_ms() >= challenge.expires_at {
                    bail!("验证码在提交前已过期");
                }
                form.fields.extend([
                    ("username".into(), credentials.username.clone()),
                    ("password".into(), credentials.password.clone()),
                    ("validateCode".into(), answer),
                    ("captchaToken".into(), challenge.token),
                ]);
            }
            LoginStep::Totp => {
                if totp_attempts == 2 {
                    bail!("CAS 连续两次拒绝动态口令，请检查 TOTP_BASE32 与系统时间");
                }
                totp_attempts += 1;
                form.fields.insert(
                    "token".into(),
                    totp_answer(&generator, &mut previous_step).await?,
                );
            }
            LoginStep::SelectTotp | LoginStep::TrustDevice => {}
        }
        response = session
            .client()
            .post(form.action)
            .header(ORIGIN, "https://sso.shmtu.edu.cn")
            .header(REFERER, url.as_str())
            .form(&form.fields)
            .send()
            .await
            .map_err(reqwest::Error::without_url)
            .context("提交 CAS 表单失败")?;
    }
}

fn login_page_error(status: reqwest::StatusCode, body: &str) -> Option<&'static str> {
    if body.contains("SSLHandshakeException") {
        Some(
            "服务端认证链路 TLS 握手失败（教务回调阶段为校验 CAS 失败），非本地密码或 TLS 配置错误",
        )
    } else if !status.is_success()
        || body.contains("服务器内部错误")
        || body.contains("class=\"actionError\"")
    {
        Some("登录页面返回服务端错误")
    } else {
        None
    }
}

fn is_home_path(path: &str) -> bool {
    path == "/shmtu/home.action" || path.starts_with("/shmtu/home.action;jsessionid=")
}

fn safe_url(url: &Url) -> String {
    format!(
        "{}://{}{}",
        url.scheme(),
        url.host_str().unwrap_or_default(),
        url.path().split(';').next().unwrap_or_default()
    )
}

async fn fetch_captcha(session: &Session, page_url: &Url) -> Result<CaptchaChallenge> {
    let url = page_url.join(&format!("captcha?_={}", now_ms()))?;
    session
        .client()
        .get(url)
        .header(REFERER, page_url.as_str())
        .send()
        .await
        .context("请求验证码失败")?
        .error_for_status()?
        .json()
        .await
        .context("解析验证码响应失败")
}

fn write_temp_file(kind: &str, extension: &str, body: &[u8]) -> Result<PathBuf> {
    let nonce = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
    let path = std::env::temp_dir().join(format!(
        "course-election-{kind}-{}-{nonce}.{extension}",
        std::process::id()
    ));
    let mut options = OpenOptions::new();
    options.create_new(true).write(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut file = options
        .open(&path)
        .with_context(|| format!("创建临时文件失败: {}", path.display()))?;
    file.write_all(body)?;
    Ok(path)
}

fn validate_cas_login_url(url: &Url) -> Result<Url> {
    if url.scheme() != "https" || url.host_str() != Some(CAS_HOST) || url.path() != "/cas/login" {
        bail!("教务入口未跳转到预期 CAS 登录页");
    }
    let services: Vec<_> = url
        .query_pairs()
        .filter(|(key, _)| key == "service")
        .collect();
    if services.len() != 1 {
        bail!("CAS URL 必须包含唯一 service");
    }
    let service = checked_url(Url::parse(&services[0].1)?)?;
    let home = service.host_str() == Some(JWXT_HOST)
        && is_home_path(service.path())
        && service.query().is_none();
    if !home {
        bail!("CAS service 未指向已知认证回调: {}", safe_url(&service));
    }
    Ok(service)
}

fn validate_ticket_callback(url: &Url, service: &Url) -> Result<()> {
    let mut callback = checked_url(url.clone())?;
    let tickets: Vec<_> = callback
        .query_pairs()
        .filter(|(k, _)| k == "ticket")
        .collect();
    if tickets.len() != 1 || tickets[0].1.is_empty() {
        bail!("CAS 成功响应缺少唯一有效 ticket");
    }
    let pairs: Vec<_> = callback
        .query_pairs()
        .filter(|(k, _)| k != "ticket")
        .map(|(k, v)| (k.into_owned(), v.into_owned()))
        .collect();
    callback.set_query(None);
    if !pairs.is_empty() {
        callback.query_pairs_mut().extend_pairs(pairs);
    }
    if callback != *service {
        bail!("CAS ticket 回调与本次 service 不匹配: {}", safe_url(url));
    }
    Ok(())
}

fn is_captcha_rejection(url: &Url) -> bool {
    url.host_str() == Some(CAS_HOST)
        && url.path() == "/cas/login"
        && url.query_pairs().any(|(key, _)| key == "captchaError")
}

fn checked_location(base: &Url, location: Option<&reqwest::header::HeaderValue>) -> Result<Url> {
    let location = location
        .and_then(|value| value.to_str().ok())
        .ok_or_else(|| anyhow!("响应缺少 Location"))?;
    checked_url(base.join(location)?)
}

fn checked_url(mut url: Url) -> Result<Url> {
    // JWXT's own CAS service still emits HTTP ticket callbacks; send them over TLS.
    if url.scheme() == "http" && url.host_str() == Some(JWXT_HOST) && is_home_path(url.path()) {
        url.set_scheme("https")
            .map_err(|_| anyhow!("无法升级教务回调到 HTTPS"))?;
    }
    let allowed_path = match url.host_str() {
        Some(CAS_HOST) => url.path() == "/cas/login",
        Some(JWXT_HOST) => is_home_path(url.path()),
        _ => false,
    };
    if url.scheme() != "https"
        || url.port_or_known_default() != Some(443)
        || !url.username().is_empty()
        || url.password().is_some()
        || url.fragment().is_some()
        || !allowed_path
    {
        bail!("拒绝跟随非预期登录跳转: {}", safe_url(&url));
    }
    Ok(url)
}

#[cfg(test)]
mod tests {
    use super::*;
    use reqwest::Url;

    #[test]
    fn current_cas_routes_and_rejections() {
        let base = Url::parse("https://jwxt.shmtu.edu.cn/shmtu/home.action").unwrap();
        for target in [
            "https://sso.shmtu.edu.cn/cas/login?service=opaque",
            "http://jwxt.shmtu.edu.cn/shmtu/home.action;jsessionid=opaque?ticket=opaque",
        ] {
            let target = checked_location(&base, Some(&target.parse().unwrap())).unwrap();
            assert_eq!(target.scheme(), "https");
        }
        for target in [
            "https://example.com/",
            "http://ng.shmtu.edu.cn/wengine-auth/login",
            "https://ng.shmtu.edu.cn/other",
            "https://jwxt.shmtu.edu.cn/shmtu/home.action.evil",
            "https://ng.shmtu.edu.cn:444/wengine-auth/login",
        ] {
            assert!(checked_location(&base, Some(&target.parse().unwrap())).is_err());
        }
        let cas = Url::parse("https://sso.shmtu.edu.cn/cas/login?service=http%3A%2F%2Fjwxt.shmtu.edu.cn%2Fshmtu%2Fhome.action%3Bjsessionid%3Dopaque").unwrap();
        let service = validate_cas_login_url(&cas).unwrap();
        validate_ticket_callback(
            &Url::parse(
                "https://jwxt.shmtu.edu.cn/shmtu/home.action;jsessionid=opaque?ticket=opaque",
            )
            .unwrap(),
            &service,
        )
        .unwrap();
        assert!(
            validate_ticket_callback(
                &Url::parse(
                    "https://ng.shmtu.edu.cn/wengine-auth/login?cas_login=true&ticket=opaque"
                )
                .unwrap(),
                &service,
            )
            .is_err()
        );
    }

    #[test]
    fn rejects_missing_duplicate_or_mismatched_tickets() {
        let service = Url::parse("https://jwxt.shmtu.edu.cn/shmtu/home.action").unwrap();
        for query in ["", "ticket=", "ticket=a&ticket=b", "ticket=a&extra=x"] {
            let mut callback = service.clone();
            callback.set_query(Some(query));
            assert!(validate_ticket_callback(&callback, &service).is_err());
        }
    }

    #[test]
    fn detects_server_tls_failure_even_with_http_200() {
        for status in [
            reqwest::StatusCode::OK,
            reqwest::StatusCode::INTERNAL_SERVER_ERROR,
        ] {
            assert!(
                login_page_error(
                    status,
                    "javax.net.ssl.SSLHandshakeException: Received fatal alert: handshake_failure"
                )
                .unwrap()
                .contains("TLS")
            );
        }
        assert!(login_page_error(reqwest::StatusCode::OK, "服务器内部错误").is_some());
        assert!(login_page_error(reqwest::StatusCode::OK, "normal page").is_none());
        assert_eq!(
            safe_url(
                &Url::parse(
                    "https://jwxt.shmtu.edu.cn/shmtu/home.action;jsessionid=secret?ticket=secret"
                )
                .unwrap()
            ),
            "https://jwxt.shmtu.edu.cn/shmtu/home.action"
        );
    }

    #[tokio::test]
    #[ignore = "requires a real credentials file and isolated working directory"]
    async fn live_cas_login_and_cookie_restore() {
        let path = std::env::var_os("COURSE_ELECTION_TEST_CREDENTIALS").unwrap();
        let session = login(Credentials::read(std::path::Path::new(&path)).unwrap())
            .await
            .unwrap();
        assert!(session.cookies().iter().any(|c| c.name == "JSESSIONID"));
        let saved = crate::app::cache::load_saved_cookies().unwrap();
        assert!(
            saved
                .cookies
                .iter()
                .all(|c| c.domain.trim_start_matches('.') == JWXT_HOST)
        );
        assert!(
            Session::new(saved.cookies)
                .unwrap()
                .is_session_valid()
                .await
        );
    }
}
