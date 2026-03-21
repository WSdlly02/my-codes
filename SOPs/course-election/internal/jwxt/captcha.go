package jwxt

import (
	"bytes"
	"encoding/base64"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"

	"github.com/bytedance/sonic"
)

func solveMathCaptcha(img []byte) ocrResult {
	fmt.Println("正在识别验证码...")
	if len(img) == 0 {
		return ocrResult{err: errors.New("empty captcha image")}
	}
	base64Image := base64.StdEncoding.EncodeToString(img)
	payload := map[string]any{
		"model":  defaultOcrModel,
		"system": "你是一个数学题专家，专门识别和解析图片中的数学题。请仔细分析图片中的数学表达式，给出最终的计算结果。只需要返回计算结果，不要任何解释。",
		"prompt": "请解析以下图片中的数学题，并直接返回计算结果：",
		"images": []string{base64Image},
		"think":  false,
		"stream": false,
		"options": map[string]any{
			"temperature":    0.1,
			"num_ctx":        8192,
			"num_predict":    1024,
			"repeat_penalty": 1.1,
			"top_k":          20,
			"top_p":          0.8,
			"min_p":          0,
		},
	}

	body, err := sonic.Marshal(payload)
	if err != nil {
		return ocrResult{err: fmt.Errorf("编码 OCR 请求失败: %w", err)}
	}

	resp, err := http.Post(defaultOllamaURL, "application/json", bytes.NewReader(body))
	if err != nil {
		return ocrResult{err: fmt.Errorf("请求本地 Ollama 失败: %w", err)}
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return ocrResult{err: fmt.Errorf("读取 OCR 响应失败: %w", err)}
	}
	if resp.StatusCode >= http.StatusBadRequest {
		return ocrResult{err: fmt.Errorf("Ollama 返回错误状态 %d: %s",
			resp.StatusCode,
			strings.TrimSpace(string(respBody)))}
	}

	var result struct {
		Response string `json:"response"`
		Error    string `json:"error"`
	}
	if err := sonic.Unmarshal(respBody, &result); err != nil {
		return ocrResult{err: fmt.Errorf("解析 OCR 响应 JSON 失败: %w", err)}
	}
	if result.Error != "" {
		return ocrResult{err: fmt.Errorf("Ollama 返回错误: %s", result.Error)}
	}
	return ocrResult{answer: strings.TrimSpace(result.Response)}
}
