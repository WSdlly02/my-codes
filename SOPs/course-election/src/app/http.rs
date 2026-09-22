use anyhow::{Context, Result, anyhow, bail};
use chrono::DateTime;
use reqwest::cookie::{CookieStore, Jar};
use reqwest::header::{
    ACCEPT, ACCEPT_LANGUAGE, CONTENT_TYPE, HeaderMap, HeaderValue, ORIGIN, REFERER,
};
use reqwest::{Client, Method, Response, StatusCode, Url};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use crate::app::cache::{
    load_count_snapshot, save_channel_cache, save_cookies, save_count_snapshot, save_mapping_cache,
};
use crate::app::parser::{
    build_lesson_count_snapshot, build_lesson_mapping_cache, parse_channels, parse_count_payload,
    parse_elec_session_time, parse_elected_ids, parse_lesson_payload, parse_unique_std_id,
    selection_session_expired,
};
use crate::app::support::{
    BASE_URL, DEFAULT_TIMEOUT_SECS, RETRY_ATTEMPTS, now_fixed, should_retry_status, urlencoding,
};
use crate::model::{
    ChannelCache, ChannelEntry, Lesson, LessonCount, LessonCountSnapshot, LessonMappingCache,
    SavedCookie,
};

const USER_AGENT: &str = "Mozilla/5.0 (X11; Linux x86_64; rv:154.0) Gecko/20100101 Firefox/154.0";

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
    fn new(cookies: &[SavedCookie]) -> Result<Self> {
        let inner = Jar::default();
        let mut store = HashMap::new();
        for cookie in cookies {
            let mut cookie = cookie.clone();
            if cookie.domain.is_empty() {
                cookie.domain = "jwxt.shmtu.edu.cn".into();
            }
            if cookie.path.is_empty() {
                cookie.path = "/".into();
            }
            let host = cookie.domain.trim_start_matches('.');
            let url = Url::parse(&format!("https://{host}{}", cookie.path))?;
            if let Some(value) = cookie_header_value(&cookie) {
                inner.add_cookie_str(&value, &url);
            }
            store.insert(
                cookie_key(&cookie.name, &cookie.domain, &cookie.path),
                cookie,
            );
        }
        Ok(Self {
            inner,
            store: Mutex::new(store),
        })
    }

    fn add_cookie_str(&self, cookie: &str, url: &Url) {
        self.inner.add_cookie_str(cookie, url);
        if let Some(update) = parse_set_cookie(cookie, url) {
            self.apply(update);
        }
    }

    fn apply(&self, update: CookieUpdate) {
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
    }

    fn jwxt_cookies(&self) -> Vec<SavedCookie> {
        self.store
            .lock()
            .expect("cookie store poisoned")
            .values()
            .filter(|cookie| {
                cookie
                    .domain
                    .trim_start_matches('.')
                    .ends_with("jwxt.shmtu.edu.cn")
            })
            .cloned()
            .collect()
    }
}

impl CookieStore for PersistedCookieJar {
    fn set_cookies(&self, headers: &mut dyn Iterator<Item = &HeaderValue>, url: &Url) {
        let headers = headers.cloned().collect::<Vec<_>>();
        self.inner.set_cookies(&mut headers.iter(), url);
        for header in headers {
            if let Ok(value) = header.to_str()
                && let Some(update) = parse_set_cookie(value, url)
            {
                self.apply(update);
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
}

impl Session {
    pub(crate) fn new(cookies: Vec<SavedCookie>) -> Result<Self> {
        let jar = Arc::new(PersistedCookieJar::new(&cookies)?);
        let client = Client::builder()
            .user_agent(USER_AGENT)
            .timeout(Duration::from_secs(DEFAULT_TIMEOUT_SECS))
            .connect_timeout(Duration::from_secs(5))
            .pool_idle_timeout(Duration::from_secs(60))
            .pool_max_idle_per_host(2)
            .redirect(reqwest::redirect::Policy::none())
            .cookie_provider(jar.clone())
            .build()
            .context("构建 HTTP 客户端失败")?;
        Ok(Self { client, jar })
    }

    pub(crate) fn empty() -> Result<Self> {
        Self::new(Vec::new())
    }

    pub(crate) fn client(&self) -> &Client {
        &self.client
    }

    pub(crate) fn cookies(&self) -> Vec<SavedCookie> {
        self.jar.jwxt_cookies()
    }

    pub(crate) fn persist_cookies(&self) -> Result<()> {
        let cookies = self.cookies();
        if cookies.is_empty() {
            return Ok(());
        }
        save_cookies(&cookies)
    }

    pub(crate) async fn is_session_valid(&self) -> bool {
        match self
            .get_with_retry(
                &format!("{BASE_URL}/stdElectCourse.action"),
                HeaderMap::new(),
                None,
            )
            .await
        {
            Ok(response) => {
                let valid = response.status() == StatusCode::OK;
                response.bytes().await.ok();
                valid
            }
            Err(_) => false,
        }
    }

    pub(crate) async fn get_with_retry(
        &self,
        url: &str,
        headers: HeaderMap,
        extra_cookies: Option<&[(&str, &str)]>,
    ) -> Result<Response> {
        let mut last_error = None;
        for attempt in 0..RETRY_ATTEMPTS {
            match self
                .request(Method::GET, url, headers.clone(), extra_cookies)
                .send()
                .await
            {
                Ok(response) if !should_retry_status(response.status().as_u16()) => {
                    return Ok(response);
                }
                Ok(response) => last_error = response.error_for_status().err().map(Into::into),
                Err(error) => last_error = Some(error.into()),
            }
            if attempt + 1 < RETRY_ATTEMPTS {
                tokio::time::sleep(Duration::from_millis(200 * (1 << attempt))).await;
            }
        }
        Err(last_error.unwrap_or_else(|| anyhow!("请求失败")))
    }

    pub(crate) fn request(
        &self,
        method: Method,
        url: &str,
        headers: HeaderMap,
        extra_cookies: Option<&[(&str, &str)]>,
    ) -> reqwest::RequestBuilder {
        if let Some(extra) = extra_cookies
            && let Ok(url) = Url::parse(url)
        {
            for (name, value) in extra {
                self.jar
                    .add_cookie_str(&format!("{name}={value}; Path=/"), &url);
            }
        }
        self.client.request(method, url).headers(headers)
    }
}

pub(crate) async fn fetch_and_cache_channels(session: &Session) -> Result<Vec<ChannelEntry>> {
    let response = session
        .get_with_retry(
            &format!("{BASE_URL}/stdElectCourse.action"),
            HeaderMap::new(),
            None,
        )
        .await?;
    reject_redirect(&response, "选课通道")?;
    let body = response.text().await.context("读取通道列表失败")?;
    let channels = parse_channels(&body)?;
    save_channel_cache(&ChannelCache {
        fetched_at: now_fixed(),
        source_url: format!("{BASE_URL}/stdElectCourse.action"),
        channels: channels.clone(),
    })?;
    session.persist_cookies()?;
    Ok(channels)
}

pub(crate) async fn refresh_course_data(session: &Session, profile_id: &str) -> Result<CourseData> {
    fetch_default_page(session, profile_id)
        .await?
        .bytes()
        .await
        .ok();

    let (lessons, counts) = tokio::join!(
        fetch_lesson_mapping(session, profile_id),
        fetch_lesson_counts(session, profile_id)
    );
    let mapping = build_lesson_mapping_cache(profile_id, lessons?);
    save_mapping_cache(profile_id, &mapping)?;

    let mut snapshot = None;
    if let Ok(counts) = counts {
        let value = build_lesson_count_snapshot(profile_id, counts);
        if save_count_snapshot(profile_id, &value).is_ok() {
            snapshot = Some(value);
        }
    }
    let mut from_cache = false;
    if snapshot.is_none()
        && let Ok(value) = load_count_snapshot(profile_id)
    {
        snapshot = Some(value);
        from_cache = true;
    }
    session.persist_cookies()?;
    Ok(CourseData {
        mapping,
        counts: snapshot,
        counts_from_cache: from_cache,
    })
}

pub(crate) async fn prewarm(session: &Session) -> Result<()> {
    // defaultPage changes the server-side election token, even if we cancel locally.
    let response = session
        .client()
        .get(format!("{BASE_URL}/stdElectCourse.action"))
        .timeout(Duration::from_secs(2))
        .send()
        .await?;
    reject_redirect(&response, "连接预热")?;
    response
        .error_for_status()?
        .bytes()
        .await
        .context("排空预热响应失败")?;
    Ok(())
}

/// Command-scoped server context. Never shared across profile changes or REPL commands.
pub(crate) struct ElectionContext<'a> {
    session: &'a Session,
    profile_id: &'a str,
    token: Option<String>,
    deadline: Option<tokio::time::Instant>,
}

impl<'a> ElectionContext<'a> {
    pub(crate) fn new(
        session: &'a Session,
        profile_id: &'a str,
        deadline: Option<tokio::time::Instant>,
    ) -> Self {
        Self {
            session,
            profile_id,
            token: None,
            deadline,
        }
    }

    pub(crate) async fn prepare(&mut self) -> Result<()> {
        if self.token.is_some() {
            return Ok(());
        }
        let started = Instant::now();
        let result = match self.deadline {
            Some(deadline) => tokio::time::timeout_at(
                deadline,
                fetch_election_token(self.session, self.profile_id),
            )
            .await
            .context("已达到监视截止时间，停止获取选课上下文")?,
            None => fetch_election_token(self.session, self.profile_id).await,
        };
        eprintln!(
            "计时：defaultPage 完整响应及 token 解析 {}ms（含 GET 重试）{}",
            started.elapsed().as_millis(),
            if result.is_err() { "（失败）" } else { "" }
        );
        self.token = Some(result?);
        Ok(())
    }

    pub(crate) async fn submit(&mut self, lesson_id: &str, select: bool) -> Result<String> {
        let started = Instant::now();
        if select {
            self.prepare().await?;
        }
        // Check after prepare too: a token refresh can consume the remaining budget.
        if self
            .deadline
            .is_some_and(|deadline| tokio::time::Instant::now() >= deadline)
        {
            bail!("已达到监视截止时间，未发起选课请求");
        }
        let token = if select {
            self.token.as_deref().expect("prepared above")
        } else {
            "undefined"
        };
        // Do not cancel an in-flight write merely because the watch deadline expires.
        let result = batch_operate(self.session, self.profile_id, lesson_id, token, select).await;
        if select
            && let Ok(body) = &result
            && selection_session_expired(body)
        {
            self.token = None;
        }
        eprintln!("计时：本次操作 {}ms", started.elapsed().as_millis());
        result
    }
}

pub(crate) fn transient_request_error(error: &anyhow::Error) -> bool {
    error.downcast_ref::<reqwest::Error>().is_some_and(|error| {
        error.is_timeout()
            || error.is_connect()
            || error.is_request()
            || error.is_body()
            || error
                .status()
                .is_some_and(|status| should_retry_status(status.as_u16()))
    })
}

pub(crate) async fn query_class_schedule_html(
    session: &Session,
    semester_id: &str,
) -> Result<String> {
    let entry = fetch_class_schedule_entry_html(session, semester_id).await?;
    let student_id = parse_unique_std_id(&entry)?;
    let html = fetch_class_schedule_table_html(session, semester_id, &student_id).await?;
    session.persist_cookies()?;
    Ok(html)
}

pub(crate) async fn fetch_elected_lesson_ids(
    session: &Session,
    profile_id: &str,
) -> Result<HashMap<String, bool>> {
    let body = fetch_default_page(session, profile_id)
        .await?
        .text()
        .await
        .context("读取 defaultPage 失败")?;
    Ok(parse_elected_ids(&body))
}

async fn fetch_lesson_mapping(session: &Session, profile_id: &str) -> Result<Vec<Lesson>> {
    let raw = fetch_payload(
        session,
        &format!("{BASE_URL}/stdElectCourse!data.action?profileId={profile_id}"),
    )
    .await?;
    tokio::task::spawn_blocking(move || parse_lesson_payload(&raw))
        .await
        .context("课程映射解析任务异常")?
}

pub(crate) async fn fetch_lesson_counts(
    session: &Session,
    profile_id: &str,
) -> Result<HashMap<String, LessonCount>> {
    let raw = fetch_payload(
        session,
        &format!("{BASE_URL}/stdElectCourse!queryStdCount.action?profileId={profile_id}"),
    )
    .await?;
    tokio::task::spawn_blocking(move || parse_count_payload(&raw))
        .await
        .context("容量解析任务异常")?
}

async fn fetch_payload(session: &Session, url: &str) -> Result<String> {
    let response = session.get_with_retry(url, HeaderMap::new(), None).await?;
    reject_redirect(&response, "接口")?;
    response
        .error_for_status()?
        .text()
        .await
        .context("读取接口响应失败")
}

/// 请求 `defaultPage` 并解析隐藏字段 `elecSessionTime`。
///
/// 这个 GET 同时会在服务端建立"课选上下文"：缺少该上下文时
/// `queryStdCount.action` 会抛 NullPointerException。需要长期盯名额时应先调用一次。
async fn fetch_election_token(session: &Session, profile_id: &str) -> Result<String> {
    let body = fetch_default_page(session, profile_id)
        .await?
        .error_for_status()?
        .text()
        .await
        .context("读取 defaultPage 失败")?;
    parse_elec_session_time(&body)
}

async fn fetch_default_page(session: &Session, profile_id: &str) -> Result<Response> {
    let url = format!(
        "{BASE_URL}/stdElectCourse!defaultPage.action?electionProfile.id={}",
        urlencoding(profile_id)
    );
    let response = session.get_with_retry(&url, HeaderMap::new(), None).await?;
    reject_redirect(&response, "defaultPage")?;
    Ok(response)
}

async fn batch_operate(
    session: &Session,
    profile_id: &str,
    lesson_id: &str,
    elec_session_time: &str,
    select_mode: bool,
) -> Result<String> {
    let operator = if select_mode {
        format!("{lesson_id}:true:0")
    } else {
        format!("{lesson_id}:false")
    };
    let profile_id = urlencoding(profile_id);
    let url = format!(
        "{BASE_URL}/stdElectCourse!batchOperator.action?profileId={profile_id}&elecSessionTime={}",
        urlencoding(elec_session_time)
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
            "{BASE_URL}/stdElectCourse!defaultPage.action?electionProfile.id={profile_id}"
        ))?,
    );
    let started = Instant::now();
    let result = async {
        session
            .request(Method::POST, &url, headers, None)
            .body(format!("operator0={}", urlencoding(&operator)))
            .send()
            .await
            .context("发送选课请求失败")?
            .text()
            .await
            .context("读取选课响应失败")
    }
    .await;
    eprintln!(
        "计时：POST 完整响应 {}ms{}",
        started.elapsed().as_millis(),
        if result.is_err() { "（失败）" } else { "" }
    );
    result
}

async fn fetch_class_schedule_entry_html(session: &Session, semester_id: &str) -> Result<String> {
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
    let response = session
        .get_with_retry(
            &format!("{BASE_URL}/courseTableForStd.action"),
            headers,
            Some(&[("semester.id", semester_id)]),
        )
        .await?;
    reject_redirect(&response, "课表入口")?;
    response.text().await.context("读取课表入口页失败")
}

async fn fetch_class_schedule_table_html(
    session: &Session,
    semester_id: &str,
    student_id: &str,
) -> Result<String> {
    let mut headers = HeaderMap::new();
    headers.insert(ACCEPT, HeaderValue::from_static("*/*"));
    headers.insert(
        ACCEPT_LANGUAGE,
        HeaderValue::from_static("en,zh-CN;q=0.9,zh;q=0.8"),
    );
    headers.insert(
        ORIGIN,
        HeaderValue::from_static("https://jwxt.shmtu.edu.cn"),
    );
    headers.insert(
        REFERER,
        HeaderValue::from_static("https://jwxt.shmtu.edu.cn/shmtu/courseTableForStd.action"),
    );
    headers.insert(
        "x-requested-with",
        HeaderValue::from_static("XMLHttpRequest"),
    );
    let response = session
        .request(
            Method::POST,
            &format!("{BASE_URL}/courseTableForStd!courseTable.action"),
            headers,
            Some(&[("semester.id", semester_id)]),
        )
        .form(&[
            ("ignoreHead", "1"),
            ("setting.kind", "std"),
            ("startWeek", "1"),
            ("semester.id", semester_id),
            ("ids", student_id),
        ])
        .send()
        .await
        .context("请求课表详情失败")?;
    reject_redirect(&response, "课表详情")?;
    response.text().await.context("读取课表详情失败")
}

fn reject_redirect(response: &Response, operation: &str) -> Result<()> {
    if response.status().is_redirection() {
        bail!("{operation} 被重定向，登录态或通道状态可能无效");
    }
    Ok(())
}

fn parse_set_cookie(value: &str, url: &Url) -> Option<CookieUpdate> {
    let mut parts = value.split(';').map(str::trim);
    let (name, value) = parts.next()?.split_once('=')?;
    if name.is_empty() {
        return None;
    }
    let mut cookie = SavedCookie {
        name: name.to_string(),
        value: value.to_string(),
        domain: url.host_str().unwrap_or_default().to_string(),
        path: default_cookie_path(url),
        ..SavedCookie::default()
    };
    let mut max_age = None;
    for part in parts {
        if part.eq_ignore_ascii_case("httponly") {
            cookie.http_only = true;
        } else if part.eq_ignore_ascii_case("secure") {
            cookie.secure = true;
        } else if let Some((name, value)) = part.split_once('=') {
            match name.trim().to_ascii_lowercase().as_str() {
                "domain" => cookie.domain = value.trim().to_string(),
                "path" => cookie.path = value.trim().to_string(),
                "expires" => cookie.expires = DateTime::parse_from_rfc2822(value.trim()).ok(),
                "max-age" => max_age = value.trim().parse::<i64>().ok(),
                _ => {}
            }
        }
    }
    if max_age.is_some_and(|age| age <= 0)
        || cookie
            .expires
            .as_ref()
            .is_some_and(|expires| *expires <= now_fixed())
    {
        return Some(CookieUpdate::Delete {
            name: cookie.name,
            domain: cookie.domain,
            path: cookie.path,
        });
    }
    Some(CookieUpdate::Upsert(cookie))
}

fn cookie_key(name: &str, domain: &str, path: &str) -> String {
    format!("{name}\0{}\0{path}", domain.trim_start_matches('.'))
}

fn default_cookie_path(url: &Url) -> String {
    url.path()
        .rsplit_once('/')
        .map(|(parent, _)| if parent.is_empty() { "/" } else { parent })
        .unwrap_or("/")
        .to_string()
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
        parts.push("Secure".into());
    }
    if cookie.http_only {
        parts.push("HttpOnly".into());
    }
    if let Some(expires) = cookie.expires {
        parts.push(format!(
            "Expires={}",
            expires
                .with_timezone(&chrono::Utc)
                .format("%a, %d %b %Y %H:%M:%S GMT")
        ));
    }
    Some(parts.join("; "))
}

#[cfg(test)]
mod tests {
    use super::parse_set_cookie;
    use reqwest::Url;

    #[test]
    fn host_only_cookie_keeps_response_host() {
        let url = Url::parse("https://jwxt.shmtu.edu.cn/shmtu/home.action").unwrap();
        let update = parse_set_cookie("JSESSIONID=value; Path=/shmtu; HttpOnly", &url).unwrap();
        let super::CookieUpdate::Upsert(cookie) = update else {
            panic!("unexpected deletion")
        };
        assert_eq!(cookie.domain, "jwxt.shmtu.edu.cn");
        assert_eq!(cookie.path, "/shmtu");
        assert!(cookie.http_only);
    }
}
