use anyhow::{Context, Result, anyhow, bail};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use regex::Regex;
use reqwest::Url;
use reqwest::header::{LOCATION, REFERER};
use serde::Deserialize;
use serde_json::{Value, json};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::app::http::Session;
use crate::app::support::{BASE_URL, DEFAULT_OCR_MODEL, DEFAULT_OLLAMA_URL};

pub(crate) const CAS_HOST: &str = "sso.shmtu.edu.cn";
const JWXT_HOST: &str = "jwxt.shmtu.edu.cn";
pub(crate) const GATEWAY_HOST: &str = "ng.shmtu.edu.cn";
const MAX_REDIRECTS: usize = 10;
const CAPTCHA_ATTEMPTS: usize = 3;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CaptchaChallenge {
    image: String,
    token: String,
    expires_at: i64,
}

pub(crate) async fn login(username: &str, password: &str) -> Result<Session> {
    let session = Session::empty()?;
    let mut redirects = 0;
    let (mut page_url, mut page) = fetch_login_page(
        &session,
        Url::parse(&format!("{BASE_URL}/home.action"))?,
        &mut redirects,
    )
    .await?;

    for attempt in 1..=CAPTCHA_ATTEMPTS {
        let service = validate_cas_login_url(&page_url)?;
        let action = checked_url(page_url.join(&form_action(&page)?)?)?;
        if action.scheme() != "https"
            || action.host_str() != Some(CAS_HOST)
            || action.path() != "/cas/login"
        {
            bail!("CAS 表单 action 指向非预期主机");
        }
        let execution = input_value(&page, "execution")?;
        let event_id = input_value(&page, "_eventId").unwrap_or_else(|_| "submit".into());
        let geolocation = input_value(&page, "geolocation").unwrap_or_default();
        let fingerprint = input_value(&page, "deviceFingerprint").unwrap_or_default();
        let captcha = fetch_captcha(&session, &page_url).await?;
        let answer = solve_captcha(&captcha.image).await?;
        if unix_millis()? >= captcha.expires_at {
            bail!("验证码在提交前已过期");
        }
        let form = [
            ("username", username),
            ("password", password),
            ("validateCode", answer.as_str()),
            ("captchaToken", captcha.token.as_str()),
            ("execution", execution.as_str()),
            ("_eventId", event_id.as_str()),
            ("geolocation", geolocation.as_str()),
            ("deviceFingerprint", fingerprint.as_str()),
        ];
        let response = session
            .client()
            .post(action)
            .header(REFERER, page_url.as_str())
            .form(&form)
            .send()
            .await
            .map_err(reqwest::Error::without_url)
            .context("提交 CAS 登录表单失败")?;

        if response.status() == reqwest::StatusCode::UNAUTHORIZED {
            bail!("用户名或密码错误");
        }
        if !response.status().is_redirection() {
            let status = response.status();
            let body = response.bytes().await.context("读取 CAS 错误响应失败")?;
            let path = write_temp_file("cas-error", "html", &body)?;
            bail!("CAS 返回 {status}，响应已保存到 {}", path.display());
        }
        let location = checked_location(response.url(), response.headers().get(LOCATION))?;
        count_redirect(&mut redirects)?;
        if is_captcha_rejection(&location) {
            if attempt == CAPTCHA_ATTEMPTS {
                bail!("验证码连续 {CAPTCHA_ATTEMPTS} 次识别错误");
            }
            eprintln!("验证码识别错误，正在刷新（{attempt}/{CAPTCHA_ATTEMPTS}）");
            (page_url, page) = fetch_login_page(&session, location, &mut redirects).await?;
            continue;
        }
        validate_ticket_callback(&location, &service)?;
        let (final_url, _) = fetch_login_page(&session, location, &mut redirects).await?;
        if final_url.host_str() != Some(JWXT_HOST) || !is_home_path(final_url.path()) {
            bail!("认证回调未到达教务首页: {}", safe_url(&final_url));
        }
        if !session.is_session_valid().await {
            bail!("CAS 已返回 ticket，但教务系统登录态验证失败");
        }
        session.persist_cookies()?;
        return Ok(session);
    }
    unreachable!()
}

fn count_redirect(count: &mut usize) -> Result<()> {
    if *count >= MAX_REDIRECTS {
        bail!("登录跳转超过 {MAX_REDIRECTS} 次");
    }
    *count += 1;
    Ok(())
}

async fn fetch_login_page(
    session: &Session,
    url: Url,
    redirects: &mut usize,
) -> Result<(Url, String)> {
    let mut url = checked_url(url)?;
    loop {
        let service = if url.host_str() == Some(CAS_HOST) {
            Some(validate_cas_login_url(&url)?)
        } else {
            None
        };
        let response = session
            .client()
            .get(url.clone())
            .send()
            .await
            .map_err(reqwest::Error::without_url)
            .with_context(|| format!("请求登录页面失败: {}", safe_url(&url)))?;
        if response.status().is_redirection() {
            let next = checked_location(&url, response.headers().get(LOCATION))?;
            if let Some(service) = service
                && !is_captcha_rejection(&next)
            {
                validate_ticket_callback(&next, &service)?;
            }
            count_redirect(redirects)?;
            response
                .bytes()
                .await
                .map_err(reqwest::Error::without_url)?;
            url = next;
            continue;
        }
        let status = response.status();
        let body = response.text().await.map_err(reqwest::Error::without_url)?;
        if let Some(reason) = login_page_error(status, &body) {
            let path = write_temp_file("login-error", "html", body.as_bytes())?;
            bail!(
                "{reason}: {status} {}，响应已保存到 {}",
                safe_url(&url),
                path.display()
            );
        }
        return Ok((url, body));
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
    let url = page_url.join(&format!("captcha?_={}", unix_millis()?))?;
    session
        .client()
        .get(url)
        .header(REFERER, page_url.as_str())
        .send()
        .await
        .context("请求验证码失败")?
        .json()
        .await
        .context("解析验证码响应失败")
}

async fn solve_captcha(image: &str) -> Result<String> {
    let encoded = image
        .strip_prefix("data:image/png;base64,")
        .ok_or_else(|| anyhow!("验证码图片不是 PNG data URL"))?;
    let image = BASE64.decode(encoded).context("验证码图片 base64 无效")?;
    let image_path = write_temp_file("captcha", "png", &image)?;
    eprintln!("OCR: {}", image_path.display());
    let payload = json!({
        "model": DEFAULT_OCR_MODEL,
        "system": "识别图片中的数学题，只返回计算结果，不要解释。",
        "prompt": "计算图片中的算式。",
        "images": [encoded],
        "think": false,
        "stream": false,
        "options": { "temperature": 0.1 }
    });
    let response = reqwest::Client::builder()
        .timeout(Duration::from_secs(60))
        .build()?
        .post(DEFAULT_OLLAMA_URL)
        .json(&payload)
        .send()
        .await
        .context("请求本地验证码 OCR 失败")?;
    let status = response.status();
    let body = response.text().await?;
    if !status.is_success() {
        bail!("验证码 OCR 返回 {status}");
    }
    let value: Value = serde_json::from_str(&body).context("解析 OCR 响应失败")?;
    let answer = value
        .get("response")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .ok_or_else(|| anyhow!("验证码 OCR 未返回答案"))?;
    let answer = normalize_ocr_answer(answer)?;
    eprintln!("识别结果: {answer}");
    Ok(answer)
}

fn normalize_ocr_answer(answer: &str) -> Result<String> {
    answer
        .trim()
        .parse::<i64>()
        .map(|value| value.to_string())
        .with_context(|| format!("验证码 OCR 结果不是整数: {answer:?}"))
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
    let gateway = service.host_str() == Some(GATEWAY_HOST)
        && service.path() == "/wengine-auth/login"
        && service.query() == Some("cas_login=true");
    let home = service.host_str() == Some(JWXT_HOST)
        && is_home_path(service.path())
        && service.query().is_none();
    if !gateway && !home {
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
        Some(GATEWAY_HOST) => url.path() == "/wengine-auth/login",
        Some(JWXT_HOST) => is_home_path(url.path()) || url.path() == "/wengine-auth/token-login",
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

fn form_action(html: &str) -> Result<String> {
    for tag in Regex::new(r"(?is)<form\b[^>]*>").unwrap().find_iter(html) {
        if attr(tag.as_str(), "id").as_deref() == Some("fm1") {
            return attr(tag.as_str(), "action").ok_or_else(|| anyhow!("#fm1 缺少 action"));
        }
    }
    bail!("CAS 页面缺少 #fm1")
}

fn input_value(html: &str, name: &str) -> Result<String> {
    for tag in Regex::new(r"(?is)<input\b[^>]*>").unwrap().find_iter(html) {
        if attr(tag.as_str(), "name").as_deref() == Some(name) {
            return Ok(attr(tag.as_str(), "value").unwrap_or_default());
        }
    }
    bail!("CAS 页面缺少字段 {name}")
}

fn attr(tag: &str, name: &str) -> Option<String> {
    Regex::new(&format!(
        r#"(?is)\b{}\s*=\s*["']([^"']*)["']"#,
        regex::escape(name)
    ))
    .ok()?
    .captures(tag)?
    .get(1)
    .map(|value| value.as_str().replace("&amp;", "&"))
}

fn unix_millis() -> Result<i64> {
    i64::try_from(SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis())
        .context("系统时间超出范围")
}

#[cfg(test)]
mod tests {
    use super::*;
    use reqwest::Url;

    #[test]
    fn parses_current_form_and_captcha_rejection() {
        let html =
            r#"<form id="fm1" action="login"><input value="opaque" name="execution"></form>"#;
        assert_eq!(form_action(html).unwrap(), "login");
        assert_eq!(input_value(html, "execution").unwrap(), "opaque");
        assert!(is_captcha_rejection(
            &Url::parse("https://sso.shmtu.edu.cn/cas/login?captchaError=true").unwrap()
        ));
    }

    #[test]
    fn ocr_answer_must_be_an_integer() {
        assert_eq!(normalize_ocr_answer(" 12\n").unwrap(), "12");
        assert!(normalize_ocr_answer("答案是 12").is_err());
    }

    #[test]
    fn current_cas_routes_and_rejections() {
        let base = Url::parse("https://jwxt.shmtu.edu.cn/shmtu/home.action").unwrap();
        for target in [
            "https://ng.shmtu.edu.cn/wengine-auth/login?id=170",
            "https://jwxt.shmtu.edu.cn/wengine-auth/token-login?wengine-ticket=opaque",
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
    fn gateway_service_and_callback_must_match() {
        let mut cas = Url::parse("https://sso.shmtu.edu.cn/cas/login").unwrap();
        cas.query_pairs_mut().append_pair(
            "service",
            "https://ng.shmtu.edu.cn/wengine-auth/login?cas_login=true",
        );
        let service = validate_cas_login_url(&cas).unwrap();
        validate_ticket_callback(
            &Url::parse("https://ng.shmtu.edu.cn/wengine-auth/login?ticket=secret&cas_login=true")
                .unwrap(),
            &service,
        )
        .unwrap();
        for bad in [
            "https://ng.shmtu.edu.cn/wengine-auth/login?cas_login=false&ticket=secret",
            "https://jwxt.shmtu.edu.cn/shmtu/home.action?ticket=secret",
            "https://ng.shmtu.edu.cn/wengine-auth/login?cas_login=true&ticket=",
            "https://ng.shmtu.edu.cn/wengine-auth/login?cas_login=true&ticket=a&ticket=b",
        ] {
            assert!(validate_ticket_callback(&Url::parse(bad).unwrap(), &service).is_err());
        }
        cas.query_pairs_mut()
            .append_pair("service", "https://example.com/");
        assert!(validate_cas_login_url(&cas).is_err());
        let mut count = 0;
        for _ in 0..MAX_REDIRECTS {
            count_redirect(&mut count).unwrap();
        }
        assert!(count_redirect(&mut count).is_err());
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
    #[ignore = "requires CAS_USERNAME/CAS_PASSWORD, OCR access, and an isolated working directory"]
    async fn live_cas_login_and_cookie_restore() {
        let session = login(
            &std::env::var("CAS_USERNAME").unwrap(),
            &std::env::var("CAS_PASSWORD").unwrap(),
        )
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
