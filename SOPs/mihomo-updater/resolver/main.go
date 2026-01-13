package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/joho/godotenv"
)

func init() {
	_ = godotenv.Load()
}

type VpsConfig struct {
	Name      string
	UUID      string
	IP        string
	Port      int
	PublicKey string
	Domain    string
}

// Global Variables
var (
	AirportURL       string
	OriginConfigPath string
	SubconverterHost string
	Port             string
	RulesURL         string
	CustomProxies    []VpsConfig
	CustomRules      []string
	AutoGroupMap     map[string][]string
)

func loadConfig() {
	// Global Configs
	AirportURL = getEnvWithFatal("AIRPORT_URL")
	OriginConfigPath = getEnvWithFatal("ORIGIN_CONFIG_PATH")
	SubconverterHost = getEnvWithFatal("SUBCONVERTER_HOST", "http://127.0.0.1:25500")
	Port = getEnvWithFatal("RESOLVER_PORT", "8088")
	RulesURL = getEnvWithFatal("RULES_URL", "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini")
	// VPS Configs
	jpVPSNode := VpsConfig{
		Name:      "🇯🇵 日本 ByteVirt VPS",
		UUID:      getEnvWithFatal("JP_BYTEVIRT_VPS_UUID"),
		IP:        getEnvWithFatal("JP_BYTEVIRT_VPS_IP"),
		Port:      strToIntWithFatal(getEnvWithFatal("JP_BYTEVIRT_VPS_PORT")),
		PublicKey: getEnvWithFatal("JP_BYTEVIRT_VPS_PUBKEY"),
		Domain:    getEnvWithFatal("JP_BYTEVIRT_VPS_DOMAIN"),
	}
	// otherVPSNode := VpsConfig{}

	// Combining Configs
	CustomProxies = []VpsConfig{jpVPSNode}
	CustomRules = []string{
		"IP-CIDR,100.64.0.0/10,DIRECT,no-resolve",
		"DOMAIN-SUFFIX,tailscale.com,DIRECT",
		fmt.Sprintf("IP-CIDR,%s/32,DIRECT,no-resolve", jpVPSNode.IP),
		fmt.Sprintf("DOMAIN,%s,DIRECT", jpVPSNode.Domain),
	}
	AutoGroupMap = map[string][]string{
		"日本": {jpVPSNode.Name /*,otherVPSNode.Name */},
		"自动": {jpVPSNode.Name},
		"手动": {jpVPSNode.Name},
	}
}

func getEnvWithFatal(key string, defaultVal ...string) string {
	if val, ok := os.LookupEnv(key); ok {
		return val
	}
	if len(defaultVal) > 0 {
		return defaultVal[0]
	}
	log.Fatalf("Missing required environment variable: %s", key)
	return ""
}

func strToIntWithFatal(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		log.Fatalf("Failed to parse port: %v", err)
	}
	return i
}

func renderProxies(configs []VpsConfig) string {
	// 定义单个节点的模板
	const nodeTemplate = `
  {
    "name":               "%s",
    "type":               "vless",
    "uuid":               "%s",
    "server":             "%s",
    "port":               %d,
    "flow":               "xtls-rprx-vision",
    "udp":                true,
    "tls":                true,
    "servername":         "www.microsoft.com",
    "client-fingerprint": "chrome",
    "reality-opts": {
      "public-key": "%s",
      "short-id":   ""
    }
  }`
	var parts []string
	for _, c := range configs {
		// 渲染每一个节点
		part := fmt.Sprintf(nodeTemplate,
			c.Name,
			c.UUID,
			c.IP,
			c.Port,
			c.PublicKey,
		)
		parts = append(parts, part)
	}
	// 用逗号连接所有节点，并在最外面加上 []，已经 json fmt
	return fmt.Sprintf("[%s]", strings.Join(parts, ","))
}

func buildSubconverterURL() string {
	params := url.Values{}
	params.Set("target", "clash")
	params.Set("url", AirportURL)
	params.Set("config", RulesURL)
	params.Set("insert", "true")
	params.Set("emoji", "true")
	params.Set("list", "false")
	params.Set("tfo", "true")
	params.Set("scv", "false")
	params.Set("fdn", "true")
	params.Set("expand", "true")
	params.Set("sort", "false")
	params.Set("udp", "true")
	params.Set("new_name", "true")

	return fmt.Sprintf("%s/sub?%s", SubconverterHost, params.Encode()) // url is alphabetically sorted
}

func fetchURL(targetURL string) ([]byte, error) {
	client := &http.Client{Timeout: 30 * time.Second}
	req, err := http.NewRequest("GET", targetURL, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("User-Agent", "Clash/Meta")
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("HTTP error: %d", resp.StatusCode)
	}
	return io.ReadAll(resp.Body)
}

func runYq(input []byte, expression string, args ...string) ([]byte, error) {
	// args can be files
	yqArgs := append([]string{"eval", "-o=json" /*use json fmt*/, expression}, args...)
	cmd := exec.Command("yq", yqArgs...)
	cmd.Stdin = bytes.NewReader(input)
	cmd.Stderr = os.Stderr // Pipe stderr to parent for debug

	return cmd.Output()
}

func generateConfig() ([]byte, error) {
	subURL := buildSubconverterURL()
	log.Printf("Fetching from Subconverter: %s", subURL)
	data, err := fetchURL(subURL)
	if err != nil {
		return nil, err
	}
	if len(data) < 100 {
		return nil, fmt.Errorf("response too short")
	}

	// Prepare data for injection
	customProxiesJSON := renderProxies(CustomProxies) // already json fmt
	customRulesJSON, _ := json.Marshal(CustomRules)

	// Build yq filter
	// 1. Inject/Update Proxies
	// 2. Rules
	// 3. Groups

	// Filter Logic:
	// Proxies:
	//   load new proxies from config
	//   filter out existing proxies with same name
	//   add new proxies
	// Rules:
	//   load new rules from config
	//   prepend to .rules
	// Groups:
	//   iterate known map.
	//   map:
	//     "日本": [NAME]
	//     "自动": [NAME]
	//     "手动": [NAME]

	filterParts := []string{
		// Proxies
		fmt.Sprintf(`%s as $new | .proxies |= (map(select([.name] - ($new | map(.name)) | length > 0)) + $new)`, customProxiesJSON),
		// Rules
		fmt.Sprintf(`.rules = %s + .rules`, string(customRulesJSON)),
	}

	// Mapping Nodes to Groups
	for keyword, nodes := range AutoGroupMap {
		nodesJSON, _ := json.Marshal(nodes) // "...", "...", nodes is not a standard JSON array
		nodesStr := string(nodesJSON)       // ["...", "..."] fmt
		nodesStr = strings.ReplaceAll(nodesStr, "'", `'"'"'`)

		part := fmt.Sprintf(`.["proxy-groups"][] |= (select(.name | test("%s")) | .proxies = %s + (.proxies - %s))`, keyword, nodesStr, nodesStr)
		filterParts = append(filterParts, part)
	}

	fullFilter := strings.Join(filterParts, " | ")

	return runYq(data, fullFilter, "-")
	// pass data as stdin ("-")
}

func handleMinimal(w http.ResponseWriter, r *http.Request) {
	if !validateUserAgent(r) {
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	}
	log.Printf("Handling minimal config request from %s", r.RemoteAddr)

	config, err := generateConfig()
	if err != nil {
		log.Printf("Error generating config: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/x-yaml; charset=utf-8")
	w.Write(config)
}

func handleFull(w http.ResponseWriter, r *http.Request) {
	if !validateUserAgent(r) {
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	}
	log.Printf("Handling full config request from %s", r.RemoteAddr)

	generatedConfig, err := generateConfig()
	if err != nil {
		log.Printf("Error generating config: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Merge with file
	// yq eval-all 'select(fileIndex == 0) as $origin | select(fileIndex == 1) as $gen | ...' origin.yaml -

	// Logic:
	// origin_data["proxies"] = config.get("proxies", [])
	// origin_data["proxy-groups"] = config.get("proxy-groups", [])
	// origin_data["rules"] = config.get("rules", [])

	filter := `select(fileIndex == 0) as $origin | select(fileIndex == 1) as $gen | $origin | .proxies = $gen.proxies | .["proxy-groups"] = $gen.["proxy-groups"] | .rules = $gen.rules`

	merged, err := runYq(generatedConfig, filter, OriginConfigPath, "-")
	// pass generatedConfig as stdin ("-"), and OriginConfigPath as first arg.
	// So fileIndex 0 is OriginConfigPath, fileIndex 1 is stdin.

	if err != nil {
		log.Printf("Error merging config: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/x-yaml; charset=utf-8")
	w.Write(merged)
}

func validateUserAgent(r *http.Request) bool {
	ua := r.UserAgent()
	allowed := []string{"Clash", "ClashMeta", "mihomo"}
	for _, a := range allowed {
		if strings.Contains(ua, a) {
			return true
		}
	}
	return false
}

func main() {
	loadConfig()

	mux := http.NewServeMux()
	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("OK"))
	})
	mux.HandleFunc("/config/minimal", handleMinimal)
	mux.HandleFunc("/config/full", handleFull)
	srv := &http.Server{
		Addr:         ":" + Port,
		Handler:      mux,
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 30 * time.Second,
		IdleTimeout:  30 * time.Second,
	}

	blockCtx, cancelBlockCtx := signal.NotifyContext(context.Background(),
		os.Interrupt, syscall.SIGTERM, syscall.SIGQUIT)
	defer cancelBlockCtx()

	go func() {
		log.Printf("Server listening on :%s (IPv4 + IPv6)", Port)
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("HTTP server failed: %v", err)
		}
	}()

	<-blockCtx.Done() // Block main thread until signal received
	log.Println("Received shutdown signal, shutting down...")

	shutdownCtx, cancelShutdownCtx := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancelShutdownCtx()

	if err := srv.Shutdown(shutdownCtx); err != nil {
		log.Printf("Graceful shutdown failed: %v", err)
		os.Exit(1)
	}

	log.Println("Server stopped")
}
