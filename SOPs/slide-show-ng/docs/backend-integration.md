# Backend Integration Guide

This backend exists to expose capabilities that browser code cannot safely or conveniently implement on its own.

## Responsibility Boundary

The backend is responsible for:

- filesystem read, write, and watch
- realtime data ingestion and push
- operating-system integrations
- local privileged orchestration

The backend is not responsible for:

- current slide index
- slide arrays or deck state machines
- navigation validation
- keyboard, touch, or fullscreen behavior
- present button semantics
- animation timing and transition sequencing

## Anti-Patterns

Do not add these patterns to the backend:

- a Go-owned current page number
- an API call on every slide transition
- backend approval for `next()` or `previous()`
- animation state tracked in Go

## Transport Surface

### REST

Use REST for request/response capabilities that do not need streaming.

Current endpoints:

- `GET /api/ping`
- `GET /api/capabilities`

### WebSocket

Use WebSocket for:

- server push
- realtime telemetry
- watch events
- command/reply interactions that should share one long-lived connection

Current WebSocket endpoint:

- `WS /ws`

## Authentication Model

The Go entrypoint generates a random `token` and appends it to the browser URL.

- REST requests must include `?token=...`
- WebSocket requests must connect to `/ws?token=...`

This is a local-session guard, not a general remote authentication system.

## Message Contract

Client request:

```json
{
  "id": "uuid",
  "cmd": "get_system_info",
  "payload": {}
}
```

Server response:

```json
{
  "id": "uuid",
  "result": {}
}
```

Server error:

```json
{
  "id": "uuid",
  "error": "unknown command"
}
```

Server event:

```json
{
  "event": "system_info_updated",
  "data": {}
}
```

## Command Design Rules

- Commands should expose capabilities, not presentation decisions.
- Command names should be verb-oriented, for example `read_file` or `watch_path`.
- Event names should describe facts that happened, for example `file_changed` or `system_info_updated`.
- Payloads should stay domain-specific and avoid slide terms unless the backend is truly operating on slide assets on disk.

## Capability Declaration

Expose high-level backend features through `GET /api/capabilities`.

This allows frontend code and generated UI to detect whether a capability is available before using it.

Current declared capabilities include:

- realtime events
- OS-backed features
- app quit support
- demo system-info stream

## Demo Capability: Realtime System Info

The repository includes one concrete backend example:

- WebSocket command: `get_system_info`
- WebSocket event: `system_info_updated`
- WebSocket command: `quit_app`

The payload includes:

- timestamp
- hostname
- OS and architecture
- CPU count
- goroutine count
- memory allocation
- app uptime

This example exists to demonstrate the intended pattern:

1. Go gathers privileged or local runtime information.
2. Go pushes updates through WebSocket.
3. The frontend renders the data, but still owns page state and presentation flow.

## Adding New Backend Capabilities

When adding a feature, prefer this sequence:

1. Decide whether the browser truly cannot or should not do it.
2. Add a narrow REST endpoint or WebSocket command/event.
3. Declare the feature in `GET /api/capabilities` when relevant.
4. Keep the frontend in charge of interaction and rendering.

Good examples:

- watch a local directory and push file change events
- read a local JSON file and return parsed data
- poll a local service and broadcast updates
- open an OS-native dialog or interact with local processes
- quit the local app process when the frontend explicitly requests it

Bad examples:

- `set_current_slide`
- `can_navigate_to`
- `begin_transition`
- `finish_transition`
- `enter_fullscreen`
- `toggle_present_mode`
