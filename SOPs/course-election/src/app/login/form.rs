use anyhow::{Result, anyhow};
use reqwest::Url;
use scraper::{Html, Selector};
use std::{
    collections::BTreeMap,
    time::{SystemTime, UNIX_EPOCH},
};
use totp_rs::{Algorithm, Builder, Secret, Totp};

#[derive(Debug, PartialEq)]
pub(super) enum LoginStep {
    Password,
    SelectTotp,
    Totp,
    TrustDevice,
}

pub(super) struct LoginForm {
    pub(super) step: LoginStep,
    pub(super) action: Url,
    pub(super) fields: BTreeMap<String, String>,
}
pub(super) fn parse_form(page_url: &Url, html: &str) -> Result<LoginForm> {
    let page = Html::parse_document(html);
    let forms = Selector::parse("form").expect("constant selector");
    let inputs = Selector::parse("input[name]").expect("constant selector");
    for form in page.select(&forms) {
        if form.value().id() == Some("fm-confirm-reg-dialog") {
            return Err(anyhow!(
                "bind an authenticator in CAS and configure TOTP_BASE32 before login",
            ));
        }
        let mut fields = form
            .select(&inputs)
            .filter(|input| !input.value().attrs().any(|(name, _)| name == "disabled"))
            .filter(|input| {
                !matches!(
                    input.value().attr("type"),
                    Some("checkbox" | "radio" | "submit" | "button")
                )
            })
            .map(|input| {
                (
                    input.value().attr("name").unwrap().to_owned(),
                    input.value().attr("value").unwrap_or("").to_owned(),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let step = if form.value().id() == Some("trustedDeviceForm") {
            fields.insert("_eventId".into(), "skip".into());
            LoginStep::TrustDevice
        } else if fields
            .get("mfaProvider")
            .is_some_and(|value| value == "mfa-gauth")
        {
            LoginStep::SelectTotp
        } else if fields.contains_key("token") && fields.contains_key("accountId") {
            fields.insert("accountId".into(), script_account_id(&page)?);
            LoginStep::Totp
        } else if fields.contains_key("username") && fields.contains_key("password") {
            LoginStep::Password
        } else {
            continue;
        };
        let action = page_url
            .join(form.value().attr("action").unwrap_or(""))
            .map_err(|_| anyhow!("CAS form action"))?;
        if action.as_str() != "https://sso.shmtu.edu.cn/cas/login" {
            return Err(anyhow!("unexpected CAS form action"));
        }
        if !fields.contains_key("execution") {
            return Err(anyhow!("CAS execution field missing"));
        }
        return Ok(LoginForm {
            step,
            action,
            fields,
        });
    }
    Err(anyhow!(
        "CAS login returned an unsupported authentication page",
    ))
}

/// The school's TOTP page fills its hidden accountId from this numeric JS constant.
fn script_account_id(page: &Html) -> Result<String> {
    let selector = Selector::parse("script:not([src])").expect("constant selector");
    page.select(&selector)
        .find_map(|script| {
            let source = script.text().collect::<String>();
            let assignment = source
                .split_once("const accountId")?
                .1
                .trim_start()
                .strip_prefix('=')?;
            let id = assignment.split_once(';')?.0.trim().parse::<u64>().ok()?;
            Some(id.to_string())
        })
        .ok_or_else(|| anyhow!("CAS TOTP accountId missing"))
}

/// School provisioning uses SHA-1, six digits and a 30-second time step.
pub(super) fn totp(base32: &str) -> Result<Totp> {
    let secret =
        Secret::try_from_base32(base32).map_err(|_| anyhow!("invalid TOTP_BASE32 encoding"))?;
    Builder::new()
        .with_algorithm(Algorithm::SHA1)
        .with_digits(6)
        .with_step_duration(30)
        .with_secret(secret)
        .build()
        .map_err(|_| anyhow!("invalid TOTP_BASE32 secret length"))
}

/// Submit the current code immediately; a retry waits until its time window changes.
pub(super) async fn totp_answer(
    generator: &Totp,
    previous_step: &mut Option<u64>,
) -> Result<String> {
    loop {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|_| anyhow!("system clock is before Unix epoch"))?
            .as_secs();
        let step = now / 30;
        let remaining = 30 - now % 30;
        if *previous_step == Some(step) {
            tokio::time::sleep(std::time::Duration::from_secs(remaining)).await;
            continue;
        }
        *previous_step = Some(step);
        return Ok(generator.generate(now).to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn mfa_fixture() -> serde_json::Value {
        serde_json::from_str(include_str!("../../../tests/fixtures/cas-mfa.json")).unwrap()
    }

    #[test]
    fn observed_cas_forms_preserve_execution_and_select_authenticator() {
        let fixture = mfa_fixture();
        let url = Url::parse("https://sso.shmtu.edu.cn/cas/login").unwrap();
        for (name, step, execution) in [
            ("password", LoginStep::Password, "fixture-password"),
            ("mfa_choice", LoginStep::SelectTotp, "fixture-choice"),
            ("totp", LoginStep::Totp, "fixture-totp"),
            ("trust_device", LoginStep::TrustDevice, "fixture-trust"),
        ] {
            let form = parse_form(&url, fixture["pages"][name].as_str().unwrap()).unwrap();
            assert_eq!(form.step, step);
            assert_eq!(form.action, url);
            assert_eq!(form.fields["execution"], execution);
            match step {
                LoginStep::SelectTotp => assert_eq!(form.fields["mfaProvider"], "mfa-gauth"),
                LoginStep::Totp => {
                    assert_eq!(form.fields["accountId"], "424242");
                    assert_eq!(form.fields["_eventId"], "submit");
                }
                LoginStep::TrustDevice => {
                    assert_eq!(form.fields["_eventId"], "skip");
                    assert!(!form.fields.contains_key("trustDevice"));
                }
                LoginStep::Password => {}
            }
        }
    }

    #[test]
    fn mfa_form_origins_and_device_ids_are_validated() {
        let fixture = mfa_fixture();
        let url = Url::parse("https://sso.shmtu.edu.cn/cas/login").unwrap();
        let page = fixture["pages"]["totp"].as_str().unwrap();
        for malformed in [
            page.replace("/cas/login", "https://other.test/cas/login"),
            page.replace(
                "const accountId = 424242;",
                "const accountId = getAccount();",
            ),
            page.replace("name=\"execution\"", "name=\"missing\""),
        ] {
            assert!(parse_form(&url, &malformed).is_err());
        }
        let error = parse_form(&url, fixture["pages"]["enrollment"].as_str().unwrap())
            .err()
            .unwrap();
        assert!(error.to_string().contains("bind an authenticator"));
    }

    #[test]
    fn totp_matches_rfc_vectors_with_school_parameters() {
        let generator = totp("GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ").unwrap();
        for vector in mfa_fixture()["totp_vectors"].as_array().unwrap() {
            assert_eq!(
                generator.generate(vector[0].as_u64().unwrap()).to_string(),
                vector[1].as_str().unwrap()
            );
        }
        for secret in ["", "not-base32!", "MY"] {
            assert!(totp(secret).is_err());
        }
    }
}
