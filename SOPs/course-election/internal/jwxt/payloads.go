package jwxt

import (
	"bytes"
	"errors"
	"regexp"
	"strings"

	"github.com/bytedance/sonic"
)

var (
	reBlockComment = regexp.MustCompile(`(?s)/\*.*?\*/`)
	reVarPrefix    = regexp.MustCompile(`^\s*var\s+lessonJSONs\s*=`)
	reCountPrefix  = regexp.MustCompile(`^\s*window\.lessonId2Counts\s*=`)
	reBareKey      = regexp.MustCompile(`([{\[,]\s*)([A-Za-z_][A-Za-z0-9_]*)(\s*:)`)
)

func parseLessonPayload(raw []byte) ([]Lesson, error) {
	jsonLike, err := normalizeJSLiteral(raw, reVarPrefix)
	if err != nil {
		return nil, err
	}

	var lessons []Lesson
	if err := sonic.Unmarshal(jsonLike, &lessons); err != nil {
		return nil, err
	}
	return lessons, nil
}

func parseCountPayload(raw []byte) (map[string]LessonCount, error) {
	jsonLike, err := normalizeJSLiteral(raw, reCountPrefix)
	if err != nil {
		return nil, err
	}

	var counts map[string]LessonCount
	if err := sonic.Unmarshal(jsonLike, &counts); err != nil {
		return nil, err
	}
	return counts, nil
}

func normalizeJSLiteral(raw []byte, prefix *regexp.Regexp) ([]byte, error) {
	text := strings.TrimSpace(string(raw))
	if strings.HasPrefix(strings.ToLower(text), "<!doctype html") || strings.HasPrefix(strings.ToLower(text), "<html") {
		return nil, errors.New("接口返回了 HTML 页面而不是课程数据，通常表示当前 profile 尚未建立上下文、未开放，或服务端返回了错误页")
	}
	text = reBlockComment.ReplaceAllString(text, "")
	text = strings.TrimSpace(prefix.ReplaceAllString(text, ""))
	text = strings.TrimSuffix(text, ";")
	text = strings.TrimSpace(text)
	if text == "" {
		return nil, errors.New("空响应体")
	}

	quoted, err := jsSingleQuoteToJSON(text)
	if err != nil {
		return nil, err
	}

	prev := ""
	for prev != quoted {
		prev = quoted
		quoted = reBareKey.ReplaceAllString(quoted, `$1"$2"$3`)
	}
	return []byte(quoted), nil
}

func jsSingleQuoteToJSON(s string) (string, error) {
	var out bytes.Buffer
	inSingle := false
	inDouble := false
	escape := false

	for i := 0; i < len(s); i++ {
		ch := s[i]

		if inSingle {
			switch {
			case escape:
				switch ch {
				case '\\', '\'':
					out.WriteByte(ch)
				case 'n':
					out.WriteByte('\n')
				case 'r':
					out.WriteByte('\r')
				case 't':
					out.WriteByte('\t')
				default:
					out.WriteByte('\\')
					out.WriteByte(ch)
				}
				escape = false
			case ch == '\\':
				escape = true
			case ch == '\'':
				inSingle = false
				out.WriteByte('"')
			case ch == '"':
				out.WriteString(`\"`)
			default:
				out.WriteByte(ch)
			}
			continue
		}

		if inDouble {
			out.WriteByte(ch)
			if escape {
				escape = false
				continue
			}
			switch ch {
			case '\\':
				escape = true
			case '"':
				inDouble = false
			}
			continue
		}

		switch ch {
		case '\'':
			inSingle = true
			out.WriteByte('"')
		case '"':
			inDouble = true
			out.WriteByte(ch)
		default:
			out.WriteByte(ch)
		}
	}

	if inSingle {
		return "", errors.New("单引号字符串未闭合")
	}
	return out.String(), nil
}
