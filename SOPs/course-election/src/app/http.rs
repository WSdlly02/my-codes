use anyhow::{Context, Result, anyhow, bail};
use chrono::DateTime;
use chrono_tz::Asia::Shanghai;
use reqwest::cookie::{CookieStore, Jar};
use reqwest::header::{
    ACCEPT, ACCEPT_LANGUAGE, CONTENT_TYPE, HeaderMap, HeaderValue, ORIGIN, REFERER,
};
use reqwest::{Client, Method, Response, StatusCode, Url};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::app::cache::{
    load_count_snapshot, load_mapping_cache, save_channel_cache, save_cookies, save_count_snapshot,
    save_mapping_cache,
};
use crate::app::parser::{
    build_lesson_count_snapshot, build_lesson_mapping_cache, parse_channels, parse_count_payload,
    parse_elected_ids, parse_lesson_payload, parse_unique_std_id,
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
                Ok(response) => last_error = Some(anyhow!(response.status().to_string())),
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

pub(crate) async fn query_course_data(
    session: Option<&Session>,
    profile_id: &str,
) -> Result<CourseData> {
    if let Ok(mapping) = load_mapping_cache(profile_id)
        && !mapping.lessons.is_empty()
    {
        let (counts, counts_from_cache) = refresh_or_load_counts(session, profile_id)
            .await
            .unwrap_or((None, false));
        return Ok(CourseData {
            mapping,
            counts,
            counts_from_cache,
        });
    }

    let session = session
        .ok_or_else(|| anyhow!("课程映射缓存不存在，且当前无法在线获取 profile={profile_id}"))?;
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

pub(crate) async fn prewarm(session: &Session, profile_id: &str) -> Result<()> {
    let channels_url = format!("{BASE_URL}/stdElectCourse.action");
    let channels = async {
        let response = session
            .get_with_retry(&channels_url, HeaderMap::new(), None)
            .await?;
        reject_redirect(&response, "连接预热")?;
        response.bytes().await.context("排空预热响应失败")?;
        Result::<()>::Ok(())
    };
    let default_page = async {
        fetch_default_page(session, profile_id)
            .await?
            .bytes()
            .await
            .context("排空 defaultPage 预热响应失败")?;
        Result::<()>::Ok(())
    };
    tokio::try_join!(channels, default_page)?;
    Ok(())
}

pub(crate) async fn select_lesson(
    session: &Session,
    profile_id: &str,
    lesson_id: &str,
) -> Result<String> {
    let response = fetch_default_page(session, profile_id).await?;
    let date = response
        .headers()
        .get("date")
        .and_then(|value| value.to_str().ok())
        .ok_or_else(|| anyhow!("defaultPage 响应头缺少 Date"))?
        .to_string();
    let drain = tokio::spawn(async move { response.bytes().await });
    let parsed = DateTime::parse_from_rfc2822(&date).context("解析 Date 失败")?;
    let elec_session_time = parsed
        .with_timezone(&Shanghai)
        .format("%Y%m%d%H%M%S")
        .to_string();
    let result = batch_operate(session, profile_id, lesson_id, &elec_session_time, true).await;
    let _ = drain.await;
    if result.is_ok() {
        session.persist_cookies()?;
    }
    result
}

pub(crate) async fn drop_lesson(
    session: &Session,
    profile_id: &str,
    lesson_id: &str,
) -> Result<String> {
    let result = batch_operate(session, profile_id, lesson_id, "undefined", false).await;
    if result.is_ok() {
        session.persist_cookies()?;
    }
    result
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

async fn refresh_or_load_counts(
    session: Option<&Session>,
    profile_id: &str,
) -> Result<(Option<LessonCountSnapshot>, bool)> {
    if let Some(session) = session
        && fetch_default_page(session, profile_id).await.is_ok()
        && let Ok(counts) = fetch_lesson_counts(session, profile_id).await
    {
        let snapshot = build_lesson_count_snapshot(profile_id, counts);
        if save_count_snapshot(profile_id, &snapshot).is_ok() {
            return Ok((Some(snapshot), false));
        }
    }
    load_count_snapshot(profile_id).map(|snapshot| (Some(snapshot), true))
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

async fn fetch_lesson_counts(
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
    response.text().await.context("读取接口响应失败")
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
