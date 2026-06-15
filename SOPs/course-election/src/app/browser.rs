use anyhow::{Context, Result, anyhow, bail};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use headless_chrome::{Browser, LaunchOptionsBuilder};
use serde_json::{Value, json};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use crate::app::cache::{load_saved_cookies, save_cookies};
use crate::app::http::Session;
use crate::app::support::{BASE_URL, DEFAULT_OCR_MODEL, DEFAULT_OLLAMA_URL, chrome_profile_path};
use crate::model::SavedCookie;

const LOGIN_WAIT_TIMEOUT: Duration = Duration::from_secs(120);

#[derive(Clone, Default)]
pub(crate) struct LoginAutofillOptions {
    pub(crate) name: Option<String>,
    pub(crate) password: Option<String>,
    pub(crate) autofill_captcha: bool,
}

struct OcrResult {
    answer: String,
}

pub(crate) fn ensure_login_with_options(opts: LoginAutofillOptions) -> Result<(Session, bool)> {
    if let Ok(saved) = load_saved_cookies() {
        if !saved.cookies.is_empty() {
            let session = Session::new(saved.cookies)?;
            if session.is_session_valid() {
                return Ok((session, false));
            }
        }
    }

    let cookies = get_cookies_via_browser(&opts).map_err(|err| anyhow!("浏览器登录失败: {err}"))?;
    if cookies.is_empty() {
        bail!("未获取到任何 Cookie");
    }
    save_cookies(&cookies)?;
    Ok((Session::new(cookies)?, true))
}

fn get_cookies_via_browser(opts: &LoginAutofillOptions) -> Result<Vec<SavedCookie>> {
    let profile_path = chrome_profile_path()?;
    println!("Chrome profile: {}", profile_path.display());
    let launch_options = LaunchOptionsBuilder::default()
        .headless(false)
        .sandbox(false)
        .idle_browser_timeout(Duration::from_secs(300))
        .user_data_dir(Some(profile_path))
        .build()
        .map_err(|err| anyhow!(err))?;
    let browser = Browser::new(launch_options).context("启动 Chrome 失败")?;
    let tab = acquire_login_tab(&browser)?;
    tab.navigate_to(&format!("{BASE_URL}/home.action"))
        .context("打开 home.action 失败")?;
    tab.wait_until_navigated().context("等待页面跳转失败")?;

    if opts.name.is_some() || opts.password.is_some() || opts.autofill_captcha {
        wait_for_cas_login_page(&tab)?;
        perform_automated_login(&tab, opts)?;
    } else {
        println!("请在浏览器中完成登录...");
    }

    wait_for_login_success(&tab)?;
    let cookies = tab.get_cookies().context("读取浏览器 Cookie 失败")?;
    let mut out = Vec::with_capacity(cookies.len());
    for cookie in cookies {
        let expires = if cookie.expires > 0.0 {
            chrono::DateTime::from_timestamp(cookie.expires.floor() as i64, 0)
                .map(|dt| dt.fixed_offset())
        } else {
            None
        };
        out.push(SavedCookie {
            name: cookie.name,
            value: cookie.value,
            domain: cookie.domain,
            path: cookie.path,
            expires,
            http_only: cookie.http_only,
            secure: cookie.secure,
        });
    }
    Ok(out)
}

fn acquire_login_tab(browser: &Browser) -> Result<Arc<headless_chrome::Tab>> {
    let deadline = Instant::now() + LOGIN_WAIT_TIMEOUT;
    while Instant::now() < deadline {
        if let Some(tab) = browser.get_tabs().lock().unwrap().first().cloned() {
            return Ok(tab);
        }
        thread::sleep(Duration::from_millis(100));
    }
    browser.new_tab().context("创建浏览器标签页失败")
}

fn wait_for_cas_login_page(tab: &headless_chrome::Tab) -> Result<()> {
    let deadline = Instant::now() + LOGIN_WAIT_TIMEOUT;
    while Instant::now() < deadline {
        ensure_browser_alive(tab)?;
        let current_url = tab.get_url();
        let ready = evaluate_bool(
            tab,
            r#"(() => {
                const img = document.querySelector('#captchaImg');
                return !!document.querySelector('#fm1') &&
                    !!document.querySelector('#username') &&
                    !!document.querySelector('#password') &&
                    !!document.querySelector('#validateCode') &&
                    !!img &&
                    !!img.complete &&
                    !!img.naturalWidth &&
                    !!img.naturalHeight;
            })()"#,
        )?;
        if current_url.to_ascii_lowercase().contains("cas/login") && ready {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(200));
    }
    bail!("等待 CAS 登录页超时");
}

fn perform_automated_login(tab: &headless_chrome::Tab, opts: &LoginAutofillOptions) -> Result<()> {
    let ocr_handle = if opts.autofill_captcha {
        let captcha_bytes = fetch_captcha_image(tab)?;
        Some(thread::spawn(move || solve_math_captcha(&captcha_bytes)))
    } else {
        None
    };

    if let Some(username) = opts.name.as_deref() {
        set_input_value(tab, "#username", username).context("用户名填写失败")?;
    }
    if let Some(password) = opts.password.as_deref() {
        set_input_value(tab, "#password", password).context("密码填写失败")?;
    }
    if opts.name.is_some() || opts.password.is_some() {
        println!("已自动填充账号密码");
    }

    let mut captcha_filled = false;
    if let Some(handle) = ocr_handle {
        let ocr = handle.join().map_err(|_| anyhow!("验证码识别线程异常"))?;
        let ocr = match ocr {
            Ok(ocr) => ocr,
            Err(err) => {
                // OCR failure should not abort warmup: username/password are already filled,
                // and the user can still type the visible captcha and submit manually.
                eprintln!("警告：验证码识别失败: {err}");
                eprintln!("请手动填写验证码并提交登录表单");
                return Ok(()); // early return, skip captcha autofill
            }
        };
        println!("验证码识别结果: {}", ocr.answer);
        set_input_value(tab, "#validateCode", &ocr.answer).context("验证码填写失败")?;
        captcha_filled = true;
    }

    if opts.name.is_some() && opts.password.is_some() && opts.autofill_captcha && captcha_filled {
        submit_login_form(tab).context("提交失败")?;
    } else {
        println!("已按参数完成自动填充，请手动补全剩余字段并提交");
    }
    Ok(())
}

fn wait_for_login_success(tab: &headless_chrome::Tab) -> Result<()> {
    let deadline = Instant::now() + LOGIN_WAIT_TIMEOUT;
    while Instant::now() < deadline {
        ensure_browser_alive(tab)?;
        let current_url = tab.get_url();
        if !current_url.to_ascii_lowercase().contains("login") {
            println!("检测到登录成功");
            return Ok(());
        }
        thread::sleep(Duration::from_millis(200));
    }
    bail!("等待登录完成超时");
}

fn ensure_browser_alive(tab: &headless_chrome::Tab) -> Result<()> {
    tab.evaluate("document.readyState", false)
        .map(|_| ())
        .map_err(|err| anyhow!("浏览器已关闭或 DevTools 连接已断开: {err}"))
}

fn fetch_captcha_image(tab: &headless_chrome::Tab) -> Result<Vec<u8>> {
    let data_url = evaluate_string(
        tab,
        r#"(() => {
            const img = document.querySelector('#captchaImg');
            if (!img || !img.complete || !img.naturalWidth || !img.naturalHeight) {
                return '';
            }
            const canvas = document.createElement('canvas');
            canvas.width = img.naturalWidth;
            canvas.height = img.naturalHeight;
            const context = canvas.getContext('2d');
            if (!context) {
                return '';
            }
            context.drawImage(img, 0, 0);
            return canvas.toDataURL('image/png');
        })()"#,
    )?;
    let prefix = "data:image/png;base64,";
    if data_url.is_empty() {
        bail!("captcha image is not ready");
    }
    if !data_url.starts_with(prefix) {
        bail!("unexpected captcha data url");
    }
    BASE64
        .decode(&data_url[prefix.len()..])
        .context("解析验证码图片失败")
}

fn set_input_value(
    tab: &headless_chrome::Tab,
    preferred_selector: &str,
    value: &str,
) -> Result<()> {
    let selector_json = serde_json::to_string(preferred_selector).context("编码 selector 失败")?;
    let value_json = serde_json::to_string(value).context("编码输入值失败")?;
    let script = format!(
        r#"(() => {{
            const usernameSelectors = [
                'input[name="username"]',
                'input[name="userName"]',
                'input[name="j_username"]',
                'input[id="username"]',
                'input[id="userName"]',
                'input[type="text"]'
            ];
            const passwordSelectors = [
                'input[name="password"]',
                'input[name="pwd"]',
                'input[name="j_password"]',
                'input[id="password"]',
                'input[id="pwd"]',
                'input[type="password"]'
            ];
            const captchaSelectors = [
                'input[name="validateCode"]',
                'input[id="validateCode"]'
            ];

            function pick(selectors, preferred) {{
                if (preferred) {{
                    const direct = document.querySelector(preferred);
                    if (direct) return direct;
                }}
                for (const selector of selectors) {{
                    const el = document.querySelector(selector);
                    if (el) return el;
                }}
                return null;
            }}

            function setValue(el, value) {{
                if (!el || value === '') return;
                el.focus();
                el.value = value;
                el.dispatchEvent(new Event('input', {{ bubbles: true }}));
                el.dispatchEvent(new Event('change', {{ bubbles: true }}));
                el.dispatchEvent(new Event('blur', {{ bubbles: true }}));
            }}

            let target = null;
            if ({selector_json} === '#username') target = pick(usernameSelectors, {selector_json});
            if ({selector_json} === '#password') target = pick(passwordSelectors, {selector_json});
            if ({selector_json} === '#validateCode') target = pick(captchaSelectors, {selector_json});
            if (!target && {selector_json}) target = document.querySelector({selector_json});
            if (!target) return false;
            setValue(target, {value_json});
            return true;
        }})()"#
    );
    if !evaluate_bool(tab, &script)? {
        bail!("未找到登录表单");
    }
    Ok(())
}

fn submit_login_form(tab: &headless_chrome::Tab) -> Result<()> {
    let ok = evaluate_bool(
        tab,
        r#"(() => {
            const form = document.querySelector('#fm1');
            const submitButton = document.querySelector('button[name="submit"]');
            if (!form) return false;
            if (typeof form.requestSubmit === 'function') {
                form.requestSubmit(submitButton || undefined);
                return true;
            }
            if (submitButton) {
                submitButton.click();
                return true;
            }
            form.submit();
            return true;
        })()"#,
    )?;
    if !ok {
        bail!("未找到登录表单");
    }
    Ok(())
}

fn solve_math_captcha(img: &[u8]) -> Result<OcrResult> {
    println!("正在识别验证码...");
    if img.is_empty() {
        bail!("empty captcha image");
    }
    let payload = json!({
        "model": DEFAULT_OCR_MODEL,
        "system": "你是一个数学题专家，专门识别和解析图片中的数学题。请仔细分析图片中的数学表达式，给出最终的计算结果。只需要返回计算结果，不要任何解释。",
        "prompt": "请解析以下图片中的数学题，并直接返回计算结果：",
        "images": [BASE64.encode(img)],
        "think": false,
        "stream": false,
        "options": {
            "temperature": 0.1,
            "num_ctx": 8192,
            "num_predict": 1024,
            "repeat_penalty": 1.1,
            "top_k": 20,
            "top_p": 0.8,
            "min_p": 0
        }
    });

    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(60))
        .build()
        .context("构建 OCR HTTP 客户端失败")?;
    let response = client
        .post(DEFAULT_OLLAMA_URL)
        .json(&payload)
        .send()
        .context("请求本地 Ollama 失败")?;
    let status = response.status();
    let body = response.text().context("读取 OCR 响应失败")?;
    if status.is_client_error() || status.is_server_error() {
        bail!("Ollama 返回错误状态 {}: {}", status.as_u16(), body.trim());
    }
    let parsed: Value = serde_json::from_str(&body).context("解析 OCR 响应 JSON 失败")?;
    if let Some(err) = parsed.get("error").and_then(Value::as_str) {
        bail!("Ollama 返回错误: {err}");
    }
    let answer = parsed
        .get("response")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .ok_or_else(|| anyhow!("OCR 未返回识别结果"))?;
    Ok(OcrResult {
        answer: answer.to_string(),
    })
}

fn evaluate_bool(tab: &headless_chrome::Tab, script: &str) -> Result<bool> {
    let object = tab.evaluate(script, false).context("执行页面脚本失败")?;
    Ok(object.value.and_then(|v| v.as_bool()).unwrap_or(false))
}

fn evaluate_string(tab: &headless_chrome::Tab, script: &str) -> Result<String> {
    let object = tab.evaluate(script, false).context("执行页面脚本失败")?;
    object
        .value
        .and_then(|value| value.as_str().map(ToOwned::to_owned))
        .ok_or_else(|| anyhow!("页面脚本未返回字符串"))
}
