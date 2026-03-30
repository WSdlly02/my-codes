package main

import (
	"encoding/json"
	"net/http"
	"os"
	"runtime"
	"sync"
	"time"

	"github.com/gorilla/websocket"
)

type wsRequest struct {
	ID      string          `json:"id"`
	Cmd     string          `json:"cmd"`
	Payload json.RawMessage `json:"payload,omitempty"`
}

type wsResponse struct {
	ID     string `json:"id,omitempty"`
	Result any    `json:"result,omitempty"`
	Error  string `json:"error,omitempty"`
	Event  string `json:"event,omitempty"`
	Data   any    `json:"data,omitempty"`
}

type systemInfo struct {
	Timestamp     string  `json:"timestamp"`
	Hostname      string  `json:"hostname"`
	OS            string  `json:"os"`
	Arch          string  `json:"arch"`
	NumCPU        int     `json:"numCpu"`
	Goroutines    int     `json:"goroutines"`
	MemoryAllocMB float64 `json:"memoryAllocMb"`
	AppUptimeSec  int64   `json:"appUptimeSec"`
}

type wsClient struct {
	conn *websocket.Conn
	mu   sync.Mutex
}

type deckServer struct {
	token     string
	clients   map[*wsClient]struct{}
	startedAt time.Time
	shutdown  func()
	mu        sync.Mutex
}

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return r.Host == r.URL.Host || r.URL.Host == ""
	},
}

func newDeckServer(token string, shutdown func()) *deckServer {
	server := &deckServer{
		token:     token,
		clients:   make(map[*wsClient]struct{}),
		startedAt: time.Now(),
		shutdown:  shutdown,
	}

	go server.streamSystemInfo()

	return server
}

func (c *wsClient) writeJSON(message wsResponse) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.conn.WriteJSON(message)
}

func (s *deckServer) streamSystemInfo() {
	ticker := time.NewTicker(time.Second)
	defer ticker.Stop()

	for range ticker.C {
		s.broadcast(wsResponse{
			Event: "system_info_updated",
			Data:  s.snapshotSystemInfo(),
		})
	}
}

func (s *deckServer) handlePing(w http.ResponseWriter, r *http.Request) {
	if !s.authorized(r) {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"ok": true,
	})
}

func (s *deckServer) handleCapabilities(w http.ResponseWriter, r *http.Request) {
	if !s.authorized(r) {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"filesystem": map[string]any{
			"read":  false,
			"write": false,
			"watch": false,
		},
		"realtime": map[string]any{
			"events": true,
		},
		"os": map[string]any{
			"supported": true,
		},
		"app": map[string]any{
			"quit": s.shutdown != nil,
		},
		"demo": map[string]any{
			"systemInfoStream": true,
		},
	})
}

func (s *deckServer) handleWS(w http.ResponseWriter, r *http.Request) {
	if !s.authorized(r) {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		return
	}

	client := &wsClient{conn: conn}

	s.mu.Lock()
	s.clients[client] = struct{}{}
	s.mu.Unlock()

	_ = client.writeJSON(wsResponse{
		Event: "system_info_updated",
		Data:  s.snapshotSystemInfo(),
	})

	for {
		var req wsRequest
		if err := conn.ReadJSON(&req); err != nil {
			break
		}

		resp := s.dispatch(req)
		if err := client.writeJSON(resp); err != nil {
			break
		}
	}

	s.mu.Lock()
	delete(s.clients, client)
	s.mu.Unlock()
	_ = conn.Close()
}

func (s *deckServer) dispatch(req wsRequest) wsResponse {
	switch req.Cmd {
	case "ping":
		return wsResponse{
			ID: req.ID,
			Result: map[string]any{
				"ok": true,
			},
		}
	case "get_system_info":
		return wsResponse{
			ID:     req.ID,
			Result: s.snapshotSystemInfo(),
		}
	case "quit_app":
		if s.shutdown == nil {
			return wsResponse{ID: req.ID, Error: "quit not supported"}
		}

		go func() {
			time.Sleep(100 * time.Millisecond)
			s.shutdown()
		}()

		return wsResponse{
			ID: req.ID,
			Result: map[string]any{
				"ok": true,
			},
		}
	default:
		return wsResponse{ID: req.ID, Error: "unknown command"}
	}
}

func (s *deckServer) broadcast(message wsResponse) {
	s.mu.Lock()
	clients := make([]*wsClient, 0, len(s.clients))
	for client := range s.clients {
		clients = append(clients, client)
	}
	s.mu.Unlock()

	for _, client := range clients {
		if err := client.writeJSON(message); err != nil {
			s.mu.Lock()
			delete(s.clients, client)
			s.mu.Unlock()
			_ = client.conn.Close()
		}
	}
}

func (s *deckServer) snapshotSystemInfo() systemInfo {
	hostname, err := os.Hostname()
	if err != nil {
		hostname = "unknown"
	}

	var memStats runtime.MemStats
	runtime.ReadMemStats(&memStats)

	return systemInfo{
		Timestamp:     time.Now().Format(time.RFC3339),
		Hostname:      hostname,
		OS:            runtime.GOOS,
		Arch:          runtime.GOARCH,
		NumCPU:        runtime.NumCPU(),
		Goroutines:    runtime.NumGoroutine(),
		MemoryAllocMB: float64(memStats.Alloc) / 1024 / 1024,
		AppUptimeSec:  int64(time.Since(s.startedAt).Seconds()),
	}
}

func (s *deckServer) authorized(r *http.Request) bool {
	return r.URL.Query().Get("token") == s.token
}

func writeJSON(w http.ResponseWriter, status int, value any) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(value)
}
