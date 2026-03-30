# Slide Show NG

Browser-hosted slide show powered by Go + React + TypeScript.

This repository is intended to be a framework, not a fixed deck template. The frontend owns presentation state and animation flow. The backend only exposes capabilities that the browser sandbox cannot provide directly.

## Structure

- `main.go`: local server entry
- `handlers.go`: minimal REST / WebSocket capability server
- `frontend/`: Vite frontend project
- `frontend/src/framework/`: reusable frontend runtime primitives
- `frontend/dist/`: embedded build output
- `docs/ai-tooling.md`: integration guide for AI/web design tools
- `docs/backend-integration.md`: backend capability design and protocol guide

## Development

```bash
cd frontend
npm install
npm run build

cd ..
go run .
```

## Build

```bash
cd frontend && npm run build
go build
```

## Migration

- Standard React migration guide: `docs/migrating-standard-react.md`

## Slideshow Flow

- App launch opens a normal app window, not fullscreen.
- The first page is a preview page and is part of the slide sequence.
- The first page may expose a centered `开始放映` button for the preview experience.
- Only `开始放映` on the preview page should both enter fullscreen and jump to page 2.
- Fullscreen and presentation are related but not identical concepts.
- Outside the preview-page start action, entering or leaving fullscreen must never change pages.
- The global `present` button exists only on presentation pages and only reflects fullscreen state.
- The global `present` button may enter or leave fullscreen, but must not implicitly navigate.
- Left and right navigation must always mean page navigation only.
- `F` must always mean fullscreen toggle only.
- The ending slide may contain both `退出程序` and the global `present` button, and they must remain semantically distinct.
- `退出程序` is an application action, not a presentation action. It should call the backend app-quit capability.

## Architecture Rules

Frontend owns:

- current slide index and navigation rules
- keyboard, gesture, and fullscreen behavior
- the semantic distinction between preview, presentation, and fullscreen
- present button behavior and placement
- quit button behavior in the presentation UI
- transition timing and animation state
- deck-specific content structure and rendering

Backend owns:

- filesystem read/write/watch
- realtime data fetch and push
- operating-system integrations

Avoid these anti-patterns:

- do not POST to Go on every slide turn
- do not let Go approve or reject navigation
- do not model the slide state machine in Go

## Frontend Runtime

The reusable frontend layer lives in `frontend/src/framework/`:

- `useDeckState`: local deck state and navigation helpers
- `useKeyboardBindings`: reusable key binding hook
- `useFullscreen`: fullscreen state and toggles
- `usePresentationRuntime`: unified slideshow runtime for preview, fullscreen, and app quit semantics
- `useBackendEvent` / `useBackendQuery` / `useAppQuit`: optional thin backend helper hooks
- `backend`: generic REST/WebSocket bridge with no slide-specific assumptions

## Backend Surface

- `GET /api/ping`: liveness check
- `GET /api/capabilities`: declared backend capability surface
- `WS /ws`: generic request/response and event push channel

## Demo Capability

The demo deck includes a realtime system information slide driven by backend push:

- WS command: `get_system_info`
- WS event: `system_info_updated`
- WS command: `quit_app`

This is the intended pattern for backend features in this framework: Go gathers privileged or local data, then the frontend decides how to render and animate it.

## Docs

- Frontend/AI tooling integration: `docs/ai-tooling.md`
- Standard React migration guide: `docs/migrating-standard-react.md`
- Backend capability integration: `docs/backend-integration.md`
