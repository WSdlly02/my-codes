use anyhow::{Context, Result, anyhow, bail};
use chrono::DateTime;
use chrono_tz::Asia::Shanghai;
use reqwest::Method;
use reqwest::StatusCode;
use reqwest::Url;
use reqwest::blocking::{Client, RequestBuilder, Response};
use reqwest::cookie::{CookieStore, Jar};
use reqwest::header::{
    ACCEPT, ACCEPT_LANGUAGE, CONTENT_TYPE, HeaderMap, HeaderValue, LOCATION, ORIGIN, REFERER,
    USER_AGENT,
};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use crate::app::cache::{
    load_channel_cache, load_count_snapshot, load_mapping_cache, save_channel_cache, save_cookies,
    save_count_snapshot, save_mapping_cache,
};
use crate::app::parser::{
    build_lesson_count_snapshot, build_lesson_mapping_cache, parse_channels, parse_count_payload,
    parse_elected_ids, parse_lesson_payload, parse_unique_std_id,
};
use crate::app::support::{
    BASE_URL, DEFAULT_TIMEOUT_SECS, RETRY_ATTEMPTS, SEMESTER_ID, now_fixed, should_retry_status,
    urlencoding,
};
use crate::model::{
    ChannelCache, ChannelEntry, LessonCount, LessonCountSnapshot, LessonMappingCache, SavedCookie,
};

pub(crate) struct CourseData {
    pub(crate) mapping: LessonMappingCache,
    pub(crate) counts: Option<LessonCountSnapshot>,
    pub(crate) counts_from_cache: bool,
}

struct PersistedCookieJar {
    inner: Jar,
    store: Mutex<HashMap<String, SavedCookie>>,
}

impl PersistedCookieJar {
    fn new(cookies: &[SavedCookie], base_url: &Url) -> Self {
        let inner = Jar::default();
        let mut store = HashMap::new();
        for cookie in cookies {
            if let Some(cookie_str) = cookie_header_value(cookie) {
                inner.add_cookie_str(&cookie_str, base_url);
            }
            store.insert(
                cookie_key(&cookie.name, &cookie.domain, &cookie.path),
                cookie.clone(),
            );
        }
        Self {
            inner,
            store: Mutex::new(store),
        }
    }

    fn add_cookie_str(&self, cookie: &str, url: &Url) {
        self.inner.add_cookie_str(cookie, url);
        if let Some(update) = parse_set_cookie(cookie) {
            self.apply_cookie_update(update);
        }
    }

    fn apply_cookie_update(&self, update: CookieUpdate) {
        let mut store = self.store.lock().expect("cookie store poisoned");
        match update {
            CookieUpdate::Upsert(cookie) => {
                store.insert(
                    cookie_key(&cookie.name, &cookie.domain, &cookie.path),
                    cookie,
                );
            }
            CookieUpdate::Delete { name, domain, path } => {
                store.remove(&cookie_key(&name, &domain, &path));
            }
        }
        let cookies = store.values().cloned().collect::<Vec<_>>();
        let _ = save_cookies(&cookies);
    }
}

impl CookieStore for PersistedCookieJar {
    fn set_cookies(&self, cookie_headers: &mut dyn Iterator<Item = &HeaderValue>, url: &Url) {
        let headers = cookie_headers.cloned().collect::<Vec<_>>();
        self.inner.set_cookies(&mut headers.iter(), url);
        for header in headers {
            if let Ok(value) = header.to_str() {
                if let Some(update) = parse_set_cookie(value) {
                    self.apply_cookie_update(update);
                }
            }
        }
    }

    fn cookies(&self, url: &Url) -> Option<HeaderValue> {
        self.inner.cookies(url)
    }
}

enum CookieUpdate {
    Upsert(SavedCookie),
    Delete {
        name: String,
        domain: String,
        path: String,
    },
}

#[derive(Clone)]
pub(crate) struct Session {
    client: Client,
    jar: Arc<PersistedCookieJar>,
    pub(crate) cookies: Vec<SavedCookie>,
}

impl Session {
    pub(crate) fn new(cookies: Vec<SavedCookie>) -> Result<Self> {
        if cookies.is_empty() {
            bail!("未找到可用 Cookie");
        }
        let base_url = Url::parse(BASE_URL).context("解析 base URL 失败")?;
        let jar = Arc::new(PersistedCookieJar::new(&cookies, &base_url));
        Ok(Self {
            client: Client::builder()
                .timeout(Duration::from_secs(DEFAULT_TIMEOUT_SECS))
                .connect_timeout(Duration::from_secs(5))
                .pool_idle_timeout(Duration::from_secs(90))
                .pool_max_idle_per_host(20)
                .redirect(reqwest::redirect::Policy::none())
                .cookie_provider(jar.clone())
                .build()
                .context("构建 HTTP 客户端失败")?,
            jar,
            cookies,
        })
    }

    pub(crate) fn is_session_valid(&self) -> bool {
        match self.get_with_retry(
            &format!("{BASE_URL}/stdElectCourse.action"),
            HeaderMap::new(),
            None,
        ) {
            Ok(resp) => {
                if resp.status().as_u16() == 302 {
                    let location = resp
                        .headers()
                        .get(LOCATION)
                        .and_then(|value| value.to_str().ok())
                        .unwrap_or_default()
                        .to_ascii_lowercase();
                    if location.contains("login")
                        || location.contains("cas")
                        || location.contains("sso")
                    {
                        return false;
                    }
                }
                resp.status() == StatusCode::OK
            }
            Err(_) => false,
        }
    }

    pub(crate) fn get_with_retry(
        &self,
        url: &str,
        headers: HeaderMap,
        extra_cookies: Option<&[(&str, &str)]>,
    ) -> Result<Response> {
        let mut last_err = None;
        for attempt in 0..RETRY_ATTEMPTS {
            let resp = self
                .request(Method::GET, url, headers.clone(), extra_cookies)
                .send();
            match resp {
                Ok(resp) if !should_retry_status(resp.status().as_u16()) => return Ok(resp),
                Ok(resp) => last_err = Some(anyhow!(resp.status().to_string())),
                Err(err) => last_err = Some(err.into()),
            }
            if attempt + 1 < RETRY_ATTEMPTS {
                let backoff = 200u64 * (1u64 << attempt);
                thread::sleep(Duration::from_millis(backoff));
            }
        }
        Err(last_err.unwrap_or_else(|| anyhow!("请求失败")))
    }

    pub(crate) fn request(
        &self,
        method: Method,
        url: &str,
        headers: HeaderMap,
        extra_cookies: Option<&[(&str, &str)]>,
    ) -> RequestBuilder {
        if let Some(extra) = extra_cookies {
            if let Ok(parsed_url) = Url::parse(url) {
                for (name, value) in extra {
                    self.jar
                        .add_cookie_str(&format!("{name}={value}; Path=/"), &parsed_url);
                }
            }
        }
        self.client.request(method, url).headers(headers)
    }
}

fn parse_set_cookie(value: &str) -> Option<CookieUpdate> {
    let mut parts = value.split(';').map(str::trim);
    let first = parts.next()?;
    let (name, cookie_value) = first.split_once('=')?;
    if name.is_empty() {
        return None;
    }

    let mut domain = String::new();
    let mut path = String::new();
    let mut expires = None;
    let mut max_age = None;
    let mut http_only = false;
    let mut secure = false;

    for part in parts {
        if part.eq_ignore_ascii_case("httponly") {
            http_only = true;
            continue;
        }
        if part.eq_ignore_ascii_case("secure") {
            secure = true;
            continue;
        }
        let Some((attr, attr_value)) = part.split_once('=') else {
            continue;
        };
        match attr.trim().to_ascii_lowercase().as_str() {
            "domain" => domain = attr_value.trim().to_string(),
            "path" => path = attr_value.trim().to_string(),
            "expires" => {
                if let Ok(parsed) = DateTime::parse_from_rfc2822(attr_value.trim()) {
                    expires = Some(parsed);
                }
            }
            "max-age" => max_age = attr_value.trim().parse::<i64>().ok(),
            _ => {}
        }
    }

    if max_age.is_some_and(|age| age <= 0)
        || expires.as_ref().is_some_and(|time| *time <= now_fixed())
    {
        return Some(CookieUpdate::Delete {
            name: name.to_string(),
            domain,
            path,
        });
    }

    Some(CookieUpdate::Upsert(SavedCookie {
        name: name.to_string(),
        value: cookie_value.to_string(),
        domain,
        path,
        expires,
        http_only,
        secure,
    }))
}

fn cookie_key(name: &str, domain: &str, path: &str) -> String {
    format!("{name}\0{domain}\0{path}")
}

fn cookie_header_value(cookie: &SavedCookie) -> Option<String> {
    if cookie.name.is_empty() || cookie.value.is_empty() {
        return None;
    }
    let mut parts = vec![format!("{}={}", cookie.name, cookie.value)];
    if !cookie.domain.is_empty() {
        parts.push(format!("Domain={}", cookie.domain));
    }
    if !cookie.path.is_empty() {
        parts.push(format!("Path={}", cookie.path));
    }
    if cookie.secure {
        parts.push("Secure".to_string());
    }
    if cookie.http_only {
        parts.push("HttpOnly".to_string());
    }
    if let Some(expires) = cookie.expires.as_ref() {
        parts.push(format!(
            "Expires={}",
            expires
                .with_timezone(&chrono::Utc)
                .format("%a, %d %b %Y %H:%M:%S GMT")
        ));
    }
    Some(parts.join("; "))
}

pub(crate) fn load_or_fetch_channels(session: &Session) -> Result<Vec<ChannelEntry>> {
    match load_channel_cache() {
        Ok(cache) if !cache.channels.is_empty() => Ok(cache.channels),
        _ => fetch_and_cache_channels(session),
    }
}

pub(crate) fn fetch_and_cache_channels(session: &Session) -> Result<Vec<ChannelEntry>> {
    let response = session.get_with_retry(
        &format!("{BASE_URL}/stdElectCourse.action"),
        HeaderMap::new(),
        None,
    )?;
    if response.status().as_u16() == 302 {
        bail!("请求被重定向，登录态可能已失效");
    }
    let body = response.text().context("读取通道列表失败")?;
    let channels = parse_channels(&body)?;
    save_channel_cache(&ChannelCache {
        fetched_at: now_fixed(),
        source_url: format!("{BASE_URL}/stdElectCourse.action"),
        channels: channels.clone(),
    })?;
    Ok(channels)
}

pub(crate) fn query_course_data(session: Option<&Session>, profile_id: &str) -> Result<CourseData> {
    if let Ok(mapping) = load_mapping_cache(profile_id) {
        if !mapping.lessons.is_empty() {
            let (counts, counts_from_cache) =
                match refresh_or_load_lesson_counts(session, profile_id) {
                    Ok(value) => value,
                    Err(_) => (None, false),
                };
            return Ok(CourseData {
                mapping,
                counts,
                counts_from_cache,
            });
        }
    }

    let session = session
        .ok_or_else(|| anyhow!("课程映射缓存不存在，且当前无法在线获取 profile={profile_id}"))?;
    let response = fetch_default_page(session, profile_id)?;
    response.bytes().ok();

    let profile_id_owned = profile_id.to_string();
    let profile_id_for_lessons = profile_id_owned.clone();
    let profile_id_for_counts = profile_id_owned.clone();
    let lesson_session = session.clone();
    let count_session = session.clone();

    let lessons_handle =
        thread::spawn(move || fetch_lesson_mapping(&lesson_session, &profile_id_for_lessons));
    let counts_handle =
        thread::spawn(move || fetch_lesson_counts(&count_session, &profile_id_for_counts));

    let lessons = lessons_handle
        .join()
        .map_err(|_| anyhow!("抓取课程映射线程异常"))??;
    let mapping = build_lesson_mapping_cache(profile_id, lessons);
    save_mapping_cache(profile_id, &mapping)?;

    let mut counts = None;
    let mut counts_from_cache = false;
    let counts_result = counts_handle
        .join()
        .unwrap_or_else(|_| Err(anyhow!("抓取容量数据线程异常")));
    if let Ok(payload) = counts_result {
        let snapshot = build_lesson_count_snapshot(profile_id, payload);
        if save_count_snapshot(profile_id, &snapshot).is_ok() {
            counts = Some(snapshot);
        }
    }
    if counts.is_none()
        && let Ok((fallback_counts, fallback_from_cache)) =
            refresh_or_load_lesson_counts(None, profile_id)
    {
        counts = fallback_counts;
        counts_from_cache = fallback_from_cache;
    }

    Ok(CourseData {
        mapping,
        counts,
        counts_from_cache,
    })
}

pub(crate) fn fetch_elected_lesson_ids(
    session: &Session,
    profile_id: &str,
) -> Result<HashMap<String, bool>> {
    let body = fetch_default_page(session, profile_id)?
        .text()
        .context("读取 defaultPage 失败")?;
    Ok(parse_elected_ids(&body))
}

pub(crate) fn select_lesson(
    session: &Session,
    profile_id: &str,
    lesson_id: &str,
) -> Result<String> {
    let response = fetch_default_page(session, profile_id)?;
    let date_header = response
        .headers()
        .get("date")
        .and_then(|value| value.to_str().ok())
        .ok_or_else(|| anyhow!("defaultPage 响应头缺少 Date"))?;
    let parsed = DateTime::parse_from_rfc2822(date_header).context("解析 Date 失败")?;
    let elec_session_time = parsed
        .with_timezone(&Shanghai)
        .format("%Y%m%d%H%M%S")
        .to_string();
    batch_operate(session, profile_id, lesson_id, &elec_session_time, true)
}

pub(crate) fn drop_lesson(session: &Session, profile_id: &str, lesson_id: &str) -> Result<String> {
    batch_operate(session, profile_id, lesson_id, "undefined", false)
}

pub(crate) fn query_class_schedule_html(session: &Session) -> Result<String> {
    let entry_html = fetch_class_schedule_entry_html(session)?;
    let std_id = parse_unique_std_id(&entry_html)?;
    fetch_class_schedule_table_html(session, &std_id)
}

fn refresh_or_load_lesson_counts(
    session: Option<&Session>,
    profile_id: &str,
) -> Result<(Option<LessonCountSnapshot>, bool)> {
    if let Some(session) = session {
        if fetch_default_page(session, profile_id).is_ok() {
            if let Ok(counts) = fetch_lesson_counts(session, profile_id) {
                let snapshot = build_lesson_count_snapshot(profile_id, counts);
                if save_count_snapshot(profile_id, &snapshot).is_ok() {
                    return Ok((Some(snapshot), false));
                }
            }
        }
    }

    match load_count_snapshot(profile_id) {
        Ok(snapshot) => Ok((Some(snapshot), true)),
        Err(err) => Err(err),
    }
}

fn fetch_lesson_mapping(session: &Session, profile_id: &str) -> Result<Vec<crate::model::Lesson>> {
    let raw = fetch_payload(
        session,
        &format!("{BASE_URL}/stdElectCourse!data.action?profileId={profile_id}"),
    )?;
    parse_lesson_payload(&raw)
}

fn fetch_lesson_counts(
    session: &Session,
    profile_id: &str,
) -> Result<HashMap<String, LessonCount>> {
    let raw = fetch_payload(
        session,
        &format!("{BASE_URL}/stdElectCourse!queryStdCount.action?profileId={profile_id}"),
    )?;
    parse_count_payload(&raw)
}

fn fetch_payload(session: &Session, url: &str) -> Result<String> {
    let response = session.get_with_retry(url, HeaderMap::new(), None)?;
    if response.status().as_u16() == 302 {
        bail!("请求被重定向，登录态可能已失效: {url}");
    }
    response.text().context("读取接口响应失败")
}

fn fetch_default_page(session: &Session, profile_id: &str) -> Result<Response> {
    let encoded_profile_id = urlencoding(profile_id);
    let url = format!(
        "{BASE_URL}/stdElectCourse!defaultPage.action?electionProfile.id={encoded_profile_id}"
    );
    let response = session.get_with_retry(&url, HeaderMap::new(), None)?;
    if response.status().as_u16() == 302 {
        bail!("defaultPage 被重定向，登录态或通道状态可能无效");
    }
    Ok(response)
}

fn batch_operate(
    session: &Session,
    profile_id: &str,
    lesson_id: &str,
    elec_session_time: &str,
    select_mode: bool,
) -> Result<String> {
    let operator_value = if select_mode {
        format!("{lesson_id}:true:0")
    } else {
        format!("{lesson_id}:false")
    };
    let encoded_profile_id = urlencoding(profile_id);
    let encoded_elec_session_time = urlencoding(elec_session_time);
    let url = format!(
        "{BASE_URL}/stdElectCourse!batchOperator.action?profileId={encoded_profile_id}&elecSessionTime={encoded_elec_session_time}"
    );
    let mut headers = HeaderMap::new();
    headers.insert(
        CONTENT_TYPE,
        HeaderValue::from_static("application/x-www-form-urlencoded"),
    );
    headers.insert(
        "x-requested-with",
        HeaderValue::from_static("XMLHttpRequest"),
    );
    headers.insert(
        REFERER,
        HeaderValue::from_str(&format!(
            "{BASE_URL}/stdElectCourse!defaultPage.action?electionProfile.id={encoded_profile_id}"
        ))
        .unwrap(),
    );
    let body = format!("operator0={}", urlencoding(&operator_value));
    let response = session
        .request(Method::POST, &url, headers, None)
        .body(body)
        .send()
        .context("发送选课请求失败")?;
    response.text().context("读取选课响应失败")
}

fn fetch_class_schedule_entry_html(session: &Session) -> Result<String> {
    let mut headers = HeaderMap::new();
    headers.insert(ACCEPT, HeaderValue::from_static("text/html, */*; q=0.01"));
    headers.insert(
        ACCEPT_LANGUAGE,
        HeaderValue::from_static("en,zh-CN;q=0.9,zh;q=0.8"),
    );
    headers.insert(
        "x-requested-with",
        HeaderValue::from_static("XMLHttpRequest"),
    );
    headers.insert(
        REFERER,
        HeaderValue::from_static(
            "https://jwxt.shmtu.edu.cn/shmtu/home!childmenus.action?menu.id=10841&security.categoryId=1",
        ),
    );
    headers.insert("sec-fetch-dest", HeaderValue::from_static("empty"));
    headers.insert("sec-fetch-mode", HeaderValue::from_static("cors"));
    headers.insert("sec-fetch-site", HeaderValue::from_static("same-origin"));
    headers.insert(
        USER_AGENT,
        HeaderValue::from_static(
            "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36",
        ),
    );
    headers.insert(
        "sec-ch-ua",
        HeaderValue::from_static(
            "\"Chromium\";v=\"146\", \"Not-A.Brand\";v=\"24\", \"Google Chrome\";v=\"146\"",
        ),
    );
    headers.insert("sec-ch-ua-mobile", HeaderValue::from_static("?0"));
    headers.insert("sec-ch-ua-platform", HeaderValue::from_static("\"Linux\""));
    let response = session.get_with_retry(
        &format!("{BASE_URL}/courseTableForStd.action"),
        headers,
        Some(&[("semester.id", SEMESTER_ID)]),
    )?;
    if response.status().as_u16() == 302 {
        bail!("courseTableForStd 被重定向，登录态可能已失效");
    }
    response.text().context("读取课表入口页失败")
}

fn fetch_class_schedule_table_html(session: &Session, std_id: &str) -> Result<String> {
    let mut headers = HeaderMap::new();
    headers.insert(ACCEPT, HeaderValue::from_static("*/*"));
    headers.insert(
        ACCEPT_LANGUAGE,
        HeaderValue::from_static("en,zh-CN;q=0.9,zh;q=0.8"),
    );
    headers.insert(
        CONTENT_TYPE,
        HeaderValue::from_static("application/x-www-form-urlencoded"),
    );
    headers.insert(
        ORIGIN,
        HeaderValue::from_static("https://jwxt.shmtu.edu.cn"),
    );
    headers.insert(
        REFERER,
        HeaderValue::from_static("https://jwxt.shmtu.edu.cn/shmtu/courseTableForStd.action"),
    );
    headers.insert("sec-fetch-dest", HeaderValue::from_static("empty"));
    headers.insert("sec-fetch-mode", HeaderValue::from_static("cors"));
    headers.insert("sec-fetch-site", HeaderValue::from_static("same-origin"));
    headers.insert(
        "x-requested-with",
        HeaderValue::from_static("XMLHttpRequest"),
    );
    headers.insert(
        USER_AGENT,
        HeaderValue::from_static(
            "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36",
        ),
    );
    headers.insert(
        "sec-ch-ua",
        HeaderValue::from_static(
            "\"Chromium\";v=\"146\", \"Not-A.Brand\";v=\"24\", \"Google Chrome\";v=\"146\"",
        ),
    );
    headers.insert("sec-ch-ua-mobile", HeaderValue::from_static("?0"));
    headers.insert("sec-ch-ua-platform", HeaderValue::from_static("\"Linux\""));
    let body = format!(
        "ignoreHead=1&setting.kind=std&startWeek=1&semester.id={}&ids={}",
        SEMESTER_ID,
        urlencoding(std_id)
    );
    let response = session
        .request(
            Method::POST,
            &format!("{BASE_URL}/courseTableForStd!courseTable.action"),
            headers,
            Some(&[("semester.id", SEMESTER_ID)]),
        )
        .body(body)
        .send()
        .context("请求课表详情失败")?;
    if response.status().as_u16() == 302 {
        bail!("courseTableForStd!courseTable 被重定向，登录态可能已失效");
    }
    response.text().context("读取课表详情失败")
}
