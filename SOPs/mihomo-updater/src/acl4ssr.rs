//! Parser and renderer for the ACL4SSR subset used by this resolver.
//!
//! It understands `ruleset` and `custom_proxy_group`; proxy protocol fields are out of scope.

use std::{collections::HashMap, fs, path::Path};

use anyhow::{Context, Result, bail};
use futures::{StreamExt, TryStreamExt, stream};
use regex::Regex;
use reqwest::Client;
use serde::Serialize;
use tracing::info;
use url::Url;

use crate::remote_cache::RemoteFileCache;

const RULE_FETCH_CONCURRENCY: usize = 8;
const CLASH_RULE_TYPES: [&str; 12] = [
    "DOMAIN",
    "DOMAIN-SUFFIX",
    "DOMAIN-KEYWORD",
    "IP-CIDR",
    "SRC-IP-CIDR",
    "GEOIP",
    "MATCH",
    "FINAL",
    "IP-CIDR6",
    "SRC-PORT",
    "DST-PORT",
    "PROCESS-NAME",
];

/// The structure of ACL4SSR_Online_Full.ini
///
/// Constructing from lines of the file
#[derive(Debug)]
pub struct Acl4SsrConfig {
    rulesets: Box<[RulesetSpec]>,
    groups: Box<[GroupSpec]>,
}

#[derive(Debug, Clone)]
struct RulesetSpec {
    policy: String,
    source: RulesetSource,
}

#[derive(Debug, Clone)]
enum RulesetSource {
    Inline(String),
    Url(Url),
}

#[derive(Debug)]
struct GroupSpec {
    name: String,
    kind: GroupKind,
    members: Box<[GroupMember]>,
}

#[derive(Debug)]
enum GroupKind {
    Select,
    UrlTest {
        url: String,
        interval: u64,
        tolerance: Option<u64>,
    },
}

#[derive(Debug)]
enum GroupMember {
    Literal(String),
    Pattern(Regex),
}

#[derive(Debug, Serialize)]
pub struct RenderedProxyGroup {
    name: String,
    #[serde(rename = "type")]
    kind: &'static str,
    proxies: Box<[String]>,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    interval: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tolerance: Option<u64>,
}

impl Acl4SsrConfig {
    pub fn load(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("failed to read ACL4SSR config: {}", path.display()))?;
        Self::parse(&content)
            .with_context(|| format!("failed to parse ACL4SSR config: {}", path.display()))
    }

    fn parse(content: &str) -> Result<Self> {
        let mut rulesets = Vec::new();
        let mut groups = Vec::new();

        for (index, raw_line) in content.lines().enumerate() {
            let line_number = index + 1;
            let line = raw_line.trim();
            if line.is_empty()
                || line.starts_with(';')
                || line.starts_with('#')
                || line.starts_with('[')
            {
                continue;
            }

            if let Some(value) = line.strip_prefix("ruleset=") {
                rulesets.push(parse_ruleset(value, line_number)?);
            } else if let Some(value) = line.strip_prefix("custom_proxy_group=") {
                groups.push(parse_group(value, line_number)?);
            }
        }

        if rulesets.is_empty() {
            bail!("ACL4SSR config does not contain any ruleset entries");
        }
        if groups.is_empty() {
            bail!("ACL4SSR config does not contain any custom_proxy_group entries");
        }

        Ok(Self {
            rulesets: rulesets.into_boxed_slice(),
            groups: groups.into_boxed_slice(),
        })
    }

    pub fn render_groups(
        &self,
        airport_node_names: &[String],
        local_group_map: HashMap<String, Box<[String]>>,
    ) -> Result<Box<[RenderedProxyGroup]>> {
        let mut groups = self
            .groups
            .iter()
            .map(|spec| render_group(spec, airport_node_names))
            .collect::<Vec<_>>();

        // TOML group matchers add local VPS nodes after ACL groups have matched airport names.
        let mut local_groups = local_group_map.into_iter().collect::<Vec<_>>();
        local_groups.sort_by(|left, right| left.0.cmp(&right.0));

        for (keyword, nodes) in local_groups {
            let matcher = Regex::new(&keyword)
                .with_context(|| format!("invalid VPS group matcher: {keyword}"))?;
            for group in &mut groups {
                if matcher.is_match(&group.name) {
                    let mut merged = nodes.to_vec();
                    merged.extend(
                        group
                            .proxies
                            .iter()
                            .filter(|name| !nodes.contains(name))
                            .cloned(),
                    );
                    group.proxies = merged.into_boxed_slice();
                }
            }
        }

        Ok(groups.into_boxed_slice())
    }

    pub async fn render_rules(
        &self,
        client: &Client,
        cache: &RemoteFileCache,
    ) -> Result<Box<[String]>> {
        let chunks = stream::iter(self.rulesets.iter().cloned())
            .map(|spec| async move {
                match spec.source {
                    RulesetSource::Inline(rule) => {
                        Ok(vec![render_rule(&rule, &spec.policy)].into_boxed_slice())
                    }
                    RulesetSource::Url(url) => {
                        // Validate downloaded content before allowing it to update the cache.
                        let upstream = fetch_ruleset(client, &url).await.and_then(|bytes| {
                            let rules = expand_ruleset(&bytes, &spec.policy)?;
                            if rules.is_empty() {
                                bail!("ruleset did not contain any supported Clash rules: {url}");
                            }
                            Ok(bytes)
                        });
                        let bytes = cache
                            .resolve("rules", "list", url.as_str(), upstream)
                            .await?;
                        let rules = expand_ruleset(&bytes, &spec.policy)?;
                        if rules.is_empty() {
                            bail!("cached ruleset did not contain supported Clash rules: {url}");
                        }
                        Ok(rules)
                    }
                }
            })
            // Download concurrently while preserving the ruleset order from the INI file.
            .buffered(RULE_FETCH_CONCURRENCY)
            .try_collect::<Vec<Box<[String]>>>()
            .await?;

        let rules = chunks.into_iter().flatten().collect::<Box<[_]>>();
        info!("rendered {} ACL4SSR rules", rules.len());
        Ok(rules)
    }
}

fn parse_ruleset(value: &str, line_number: usize) -> Result<RulesetSpec> {
    let (policy, source) = value
        .split_once(',')
        .with_context(|| format!("invalid ruleset at line {line_number}"))?;
    let policy = policy.trim();
    let source = source.trim();
    if policy.is_empty() || source.is_empty() {
        bail!("empty ruleset policy or source at line {line_number}");
    }

    let source = if let Some(rule) = source.strip_prefix("[]") {
        RulesetSource::Inline(rule.to_string())
    } else {
        RulesetSource::Url(
            Url::parse(source)
                .with_context(|| format!("invalid ruleset URL at line {line_number}: {source}"))?,
        )
    };

    Ok(RulesetSpec {
        policy: policy.to_string(),
        source,
    })
}

fn parse_group(value: &str, line_number: usize) -> Result<GroupSpec> {
    let fields = value.split('`').collect::<Vec<_>>();
    if fields.len() < 3 {
        bail!("invalid custom_proxy_group at line {line_number}");
    }

    let name = fields[0].trim();
    if name.is_empty() {
        bail!("empty proxy group name at line {line_number}");
    }

    let (kind, member_fields) = match fields[1] {
        "select" => (GroupKind::Select, &fields[2..]),
        "url-test" => {
            if fields.len() < 5 {
                bail!("invalid url-test group at line {line_number}");
            }
            let timing = parse_group_timing(fields[fields.len() - 1], line_number)?;
            (
                GroupKind::UrlTest {
                    url: fields[fields.len() - 2].to_string(),
                    interval: timing.0,
                    tolerance: timing.1,
                },
                &fields[2..fields.len() - 2],
            )
        }
        kind => bail!("unsupported proxy group type `{kind}` at line {line_number}"),
    };

    let members = member_fields
        .iter()
        .map(|member| {
            if let Some(literal) = member.strip_prefix("[]") {
                if literal.is_empty() {
                    bail!("empty literal group member at line {line_number}");
                }
                Ok(GroupMember::Literal(literal.to_string()))
            } else {
                let regex = Regex::new(member).with_context(|| {
                    format!("invalid node matcher `{member}` at line {line_number}")
                })?;
                Ok(GroupMember::Pattern(regex))
            }
        })
        .collect::<Result<Vec<_>>>()?;

    if members.is_empty() {
        bail!("proxy group has no members at line {line_number}");
    }

    Ok(GroupSpec {
        name: name.to_string(),
        kind,
        members: members.into_boxed_slice(),
    })
}

fn parse_group_timing(value: &str, line_number: usize) -> Result<(u64, Option<u64>)> {
    let fields = value.split(',').collect::<Vec<_>>();
    let interval = fields
        .first()
        .context("missing group interval")?
        .parse::<u64>()
        .with_context(|| format!("invalid group interval at line {line_number}"))?;
    let tolerance = fields
        .get(2)
        .filter(|value| !value.is_empty())
        .map(|value| {
            value
                .parse::<u64>()
                .with_context(|| format!("invalid group tolerance at line {line_number}"))
        })
        .transpose()?;
    Ok((interval, tolerance))
}

fn render_group(spec: &GroupSpec, airport_node_names: &[String]) -> RenderedProxyGroup {
    let mut proxies = Vec::new();
    for member in &spec.members {
        match member {
            GroupMember::Literal(name) => proxies.push(name.clone()),
            GroupMember::Pattern(regex) => {
                for name in airport_node_names {
                    if regex.is_match(name) && !proxies.contains(name) {
                        proxies.push(name.clone());
                    }
                }
            }
        }
    }
    if proxies.is_empty() {
        proxies.push("DIRECT".to_string());
    }

    match &spec.kind {
        GroupKind::Select => RenderedProxyGroup {
            name: spec.name.clone(),
            kind: "select",
            proxies: proxies.into_boxed_slice(),
            url: None,
            interval: None,
            tolerance: None,
        },
        GroupKind::UrlTest {
            url,
            interval,
            tolerance,
        } => RenderedProxyGroup {
            name: spec.name.clone(),
            kind: "url-test",
            proxies: proxies.into_boxed_slice(),
            url: Some(url.clone()),
            interval: Some(*interval),
            tolerance: *tolerance,
        },
    }
}

async fn fetch_ruleset(client: &Client, url: &Url) -> Result<Vec<u8>> {
    let response = client
        .get(url.clone())
        .header(reqwest::header::USER_AGENT, "Clash/Meta")
        .send()
        .await
        .with_context(|| format!("ruleset request failed: {url}"))?;
    let status = response.status();
    if !status.is_success() {
        bail!("http error {status} while fetching ruleset {url}");
    }
    response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .with_context(|| format!("failed to read ruleset response: {url}"))
}

fn expand_ruleset(content: &[u8], policy: &str) -> Result<Box<[String]>> {
    let content = std::str::from_utf8(content).context("ruleset is not valid UTF-8")?;
    let rules = content
        .lines()
        .filter_map(|raw_line| {
            let line = raw_line.trim();
            if line.is_empty()
                || line.starts_with(';')
                || line.starts_with('#')
                || line.starts_with("//")
                || !CLASH_RULE_TYPES
                    .iter()
                    .any(|rule_type| line.starts_with(rule_type))
            {
                return None;
            }

            let line = line.split_once("//").map_or(line, |(rule, _)| rule.trim());
            Some(render_rule(line, policy))
        })
        .collect();
    Ok(rules)
}

fn render_rule(rule: &str, policy: &str) -> String {
    let mut fields = rule.split(',');
    let mut rule_type = fields.next().unwrap_or_default();
    if rule_type == "FINAL" {
        rule_type = "MATCH";
    }

    let Some(value) = fields.next() else {
        return format!("{rule_type},{policy}");
    };

    let mut rendered = format!("{rule_type},{value},{policy}");
    if let Some(option) = fields.next() {
        rendered.push(',');
        rendered.push_str(option);
    }
    rendered
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{Acl4SsrConfig, expand_ruleset};

    const CONFIG: &str = r#"
[custom]
ruleset=DIRECT,https://example.com/direct.list
ruleset=Fallback,[]FINAL
custom_proxy_group=Proxy`select`[]Auto`.*
custom_proxy_group=Auto`url-test`(HK|Hong Kong)`https://example.com/204`300,,50
"#;

    #[test]
    fn parses_and_renders_groups_without_matching_local_nodes_implicitly() {
        let config = Acl4SsrConfig::parse(CONFIG).unwrap();
        let airport_nodes = vec!["HK Airport".to_string(), "US Airport".to_string()];
        let local_groups = HashMap::from([(
            "Auto".to_string(),
            vec!["JP Local VPS".to_string()].into_boxed_slice(),
        )]);

        let groups = config.render_groups(&airport_nodes, local_groups).unwrap();
        let json = serde_json::to_value(&groups).unwrap();

        assert_eq!(
            json[0]["proxies"],
            serde_json::json!(["Auto", "HK Airport", "US Airport"])
        );
        assert_eq!(
            json[1]["proxies"],
            serde_json::json!(["JP Local VPS", "HK Airport"])
        );
        assert_eq!(json[1]["tolerance"], 50);
    }

    #[test]
    fn expands_supported_rules_and_preserves_no_resolve() {
        let content = br#"
# comment
DOMAIN-SUFFIX,example.com
IP-CIDR,192.0.2.0/24,no-resolve
URL-REGEX,^https://example.com
"#;
        let rules = expand_ruleset(content, "Proxy").unwrap();

        assert_eq!(
            rules.as_ref(),
            [
                "DOMAIN-SUFFIX,example.com,Proxy",
                "IP-CIDR,192.0.2.0/24,Proxy,no-resolve",
            ]
        );
    }

    #[test]
    fn converts_inline_final_to_match() {
        let config = Acl4SsrConfig::parse(CONFIG).unwrap();
        let inline = &config.rulesets[1];
        let rule = match &inline.source {
            super::RulesetSource::Inline(rule) => super::render_rule(rule, &inline.policy),
            _ => panic!("expected inline rule"),
        };
        assert_eq!(rule, "MATCH,Fallback");
    }
}
