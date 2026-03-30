# Slide Show NG

Browser-hosted slide show powered by Go + React + TypeScript.

## Structure

- `main.go`: local server entry
- `handlers.go`: REST / WebSocket handlers
- `frontend/`: Vite frontend project
- `frontend/dist/`: embedded build output

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
