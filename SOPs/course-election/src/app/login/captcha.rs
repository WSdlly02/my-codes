use anyhow::{Context, Result, anyhow, ensure};
use base64::{Engine, engine::general_purpose::STANDARD};
use reqwest::{Client, redirect::Policy};
use serde::Deserialize;
use serde_json::{Value, json};
use std::time::Duration;

const OCR_PROMPT: &str =
    "识别整数算式(+-*)，不计算。只输出JSON：{\"e\":\"1+2\"}，不含等号；看不清则e为null。";

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Expression {
    e: Option<String>,
}

pub(super) struct CaptchaSolver {
    client: Client,
    key: String,
}

impl CaptchaSolver {
    pub(super) fn new(key: String) -> Result<Self> {
        Ok(Self {
            key,
            client: Client::builder()
                .redirect(Policy::none())
                .timeout(Duration::from_secs(180))
                .build()?,
        })
    }

    pub(super) async fn solve(&self, image: &str) -> Result<String> {
        let encoded = image
            .strip_prefix("data:image/png;base64,")
            .context("验证码缺少 PNG data URL")?;
        let png = STANDARD.decode(encoded).context("验证码 base64 无效")?;
        ensure!(png.starts_with(b"\x89PNG\r\n\x1a\n"), "验证码 PNG 签名无效");
        let path = super::write_temp_file("captcha", "png", &png)?;
        eprintln!("OCR: {}", path.display());
        let response = self
            .client
            .post("https://api.deepseek.com/v1/chat/completions")
            .bearer_auth(self.key.trim())
            .json(&request_body("deepseek-flash", encoded))
            .send()
            .await
            .context("请求 DeepSeek 验证码识别失败")?;
        ensure!(
            response.status().is_success(),
            "DeepSeek 返回 {}",
            response.status()
        );
        let answer = response_result(&response.json().await.context("解析 DeepSeek 响应失败")?)?;
        eprintln!("识别结果: {answer}");
        Ok(answer.to_string())
    }
}

fn request_body(model: &str, png_base64: &str) -> Value {
    json!({
        "model": model,
        "stream": false,
        "thinking": {"type": "disabled"},
        "messages": [{"role": "user", "content": [
            {"type": "text", "text": OCR_PROMPT},
            {"type": "image_url", "image_url": {
                "url": format!("data:image/png;base64,{png_base64}")
            }}
        ]}],
        "response_format": {"type": "json_object"},
        "max_tokens": 64
    })
}

fn response_result(raw: &Value) -> Result<i64> {
    if raw
        .pointer("/choices/0/finish_reason")
        .and_then(Value::as_str)
        != Some("stop")
    {
        return Err(anyhow!("OCR response did not finish normally"));
    }
    let text = raw
        .pointer("/choices/0/message/content")
        .and_then(Value::as_str)
        .ok_or_else(|| anyhow!("OCR response missing text"))?;
    let expression: Expression =
        serde_json::from_str(text).map_err(|_| anyhow!("OCR response requires expression JSON"))?;
    let expression = expression
        .e
        .ok_or_else(|| anyhow!("OCR could not recognize expression"))?;
    calculate(&expression).map_err(anyhow::Error::msg)
}

/// 仅支持两个整数的加减乘，结果始终为 i64。
fn calculate(expression: &str) -> std::result::Result<i64, &'static str> {
    if expression.len() > 128 {
        return Err("算式过长");
    }
    let expression: String = expression.chars().filter(|c| !c.is_whitespace()).collect();
    // 跳过首字符的正负号；右操作数的正负号由整数解析处理。
    let (index, op) = expression
        .char_indices()
        .skip(1)
        .find(|(_, c)| matches!(c, '+' | '-' | '*'))
        .ok_or("缺少加减乘运算符")?;
    let a: i64 = expression[..index]
        .parse()
        .map_err(|_| "左操作数不是有效整数")?;
    let b: i64 = expression[index + 1..]
        .parse()
        .map_err(|_| "右操作数不是有效整数")?;
    match op {
        '+' => a.checked_add(b),
        '-' => a.checked_sub(b),
        '*' => a.checked_mul(b),
        _ => unreachable!(),
    }
    .ok_or("计算溢出")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chat_request_contains_image_and_json_contract() {
        let body = request_body("deepseek-flash", "iVBORw0KGgo=");
        assert_eq!(body["model"], "deepseek-flash");
        assert_eq!(
            body["messages"][0]["content"][1]["image_url"]["url"],
            "data:image/png;base64,iVBORw0KGgo="
        );
        assert_eq!(body["response_format"]["type"], "json_object");
        assert_eq!(body["thinking"]["type"], "disabled");
        assert_eq!(body["stream"], false);
        assert_eq!(body["max_tokens"], 64);
    }

    #[test]
    fn arithmetic() {
        for (expression, expected) in [
            ("1+6", 7),
            ("3-8", -5),
            ("12*3", 36),
            (" -2 - -3 ", 1),
            ("-2*-3", 6),
            ("0*123", 0),
        ] {
            assert_eq!(calculate(expression).unwrap(), expected);
        }
    }

    #[test]
    fn rejects_invalid_and_overflow() {
        for expression in [
            "",
            "1+2=",
            "1+2+3",
            "1/0",
            "1;exit()",
            "1.5+2",
            "9223372036854775807+1",
            "-9223372036854775808-1",
            "9223372036854775807*2",
            "7/2",
            "12÷4",
            "12×3",
        ] {
            assert!(calculate(expression).is_err(), "{expression}");
        }
    }

    #[test]
    fn strict_json_and_abstention() {
        assert!(serde_json::from_str::<Expression>(r#"{"e":12}"#).is_err());
        assert!(serde_json::from_str::<Expression>(r#"{"e":"1+2","extra":0}"#).is_err());
        assert!(
            serde_json::from_str::<Expression>(r#"{"e":null}"#)
                .unwrap()
                .e
                .is_none()
        );
    }

    #[test]
    fn validates_response_before_calculating() {
        let response = |reason: &str, content: Value| {
            json!({"choices": [{
                "finish_reason": reason, "message": {"content": content}
            }]})
        };
        assert_eq!(
            response_result(&response("stop", json!(r#"{"e":"4+8"}"#))).unwrap(),
            12
        );
        for raw in [
            response("length", json!(r#"{"e":"4+8"}"#)),
            response("stop", json!(r#"{"e":null}"#)),
            response("stop", json!("   ")),
            response("stop", Value::Null),
            response("stop", json!(r#"{"e":"4/2"}"#)),
            json!({"choices": []}),
            json!({"error": {"message": "failure"}}),
        ] {
            assert!(response_result(&raw).is_err());
        }
    }
}
