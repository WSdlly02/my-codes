package main

import (
	"encoding/json"
	"errors"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
)

type Slide struct {
	Label string `json:"label,omitempty"`
	Kind  string `json:"kind"`
}

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

type slideChangedPayload struct {
	Index int `json:"index"`
}

type deckServer struct {
	token   string
	slides  []Slide
	clients map[*websocket.Conn]struct{}
	current int
	mu      sync.Mutex
}

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return r.Host == r.URL.Host || r.URL.Host == ""
	},
}

func newDeckServer(token string) *deckServer {
	return &deckServer{
		token: token,
		slides: []Slide{
			{Kind: "hero"},
			{Label: "为什么", Kind: "why"},
			{Label: "架构", Kind: "architecture"},
			{Label: "对比", Kind: "comparison"},
			{Label: "开始", Kind: "getting-started"},
			{Kind: "ending"},
		},
		clients: make(map[*websocket.Conn]struct{}),
	}
}

func (s *deckServer) handlePing(w http.ResponseWriter, r *http.Request) {
	if !s.authorized(r) {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"ok":      true,
		"current": s.current,
		"slides":  len(s.slides),
	})
}

func (s *deckServer) handleSlides(w http.ResponseWriter, r *http.Request) {
	if !s.authorized(r) {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	writeJSON(w, http.StatusOK, s.slides)
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

	s.mu.Lock()
	s.clients[conn] = struct{}{}
	current := s.current
	s.mu.Unlock()

	_ = conn.WriteJSON(wsResponse{
		Event: "slide_changed",
		Data:  slideChangedPayload{Index: current},
	})

	for {
		var req wsRequest
		if err := conn.ReadJSON(&req); err != nil {
			break
		}

		resp := s.dispatch(req)
		if err := conn.WriteJSON(resp); err != nil {
			break
		}
	}

	s.mu.Lock()
	delete(s.clients, conn)
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
	case "get_slides":
		return wsResponse{
			ID:     req.ID,
			Result: s.slides,
		}
	case "set_current_slide":
		var payload slideChangedPayload
		if err := json.Unmarshal(req.Payload, &payload); err != nil {
			return wsResponse{ID: req.ID, Error: "invalid payload"}
		}

		if err := s.setCurrent(payload.Index); err != nil {
			return wsResponse{ID: req.ID, Error: err.Error()}
		}

		s.broadcast(wsResponse{
			Event: "slide_changed",
			Data:  slideChangedPayload{Index: payload.Index},
		})

		return wsResponse{
			ID: req.ID,
			Result: map[string]any{
				"ok":    true,
				"index": payload.Index,
			},
		}
	default:
		return wsResponse{ID: req.ID, Error: "unknown command"}
	}
}

func (s *deckServer) setCurrent(index int) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if index < 0 || index >= len(s.slides) {
		return errors.New("slide index out of range")
	}

	s.current = index
	return nil
}

func (s *deckServer) broadcast(message wsResponse) {
	s.mu.Lock()
	clients := make([]*websocket.Conn, 0, len(s.clients))
	for conn := range s.clients {
		clients = append(clients, conn)
	}
	s.mu.Unlock()

	for _, conn := range clients {
		if err := conn.WriteJSON(message); err != nil {
			s.mu.Lock()
			delete(s.clients, conn)
			s.mu.Unlock()
			_ = conn.Close()
		}
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
