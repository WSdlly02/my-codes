package jwxt

const (
	baseURL          = "https://jwxt.shmtu.edu.cn/shmtu"
	defaultTimeout   = 10
	defaultOcrModel  = "qwen3-vl:8b-instruct"
	defaultOllamaURL = "http://localhost:11434/api/generate"
	cacheDir         = "cache"
	cookieFile       = cacheDir + "/cookies.json"
	chromeProfile    = "./chrome-profile"
	semesterID       = "396"
)
