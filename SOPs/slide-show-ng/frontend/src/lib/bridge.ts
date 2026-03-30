const search = new URLSearchParams(window.location.search);
const token = search.get('token') ?? '';
const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const ws = new WebSocket(`${wsProtocol}//${window.location.host}/ws?token=${encodeURIComponent(token)}`);

export function call<T>(cmd: string, payload?: unknown): Promise<T> {
  const id = crypto.randomUUID();
  return new Promise((resolve, reject) => {
    ws.send(JSON.stringify({ id, cmd, payload }));

    const handler = (e: MessageEvent) => {
      const msg = JSON.parse(e.data);
      if (msg.id === id) {
        ws.removeEventListener("message", handler);
        if (msg.error) {
          reject(new Error(msg.error));
          return;
        }
        resolve(msg.result);
      }
    };
    ws.addEventListener("message", handler);
  });
}

export function on<T>(event: string, cb: (data: T) => void) {
  ws.addEventListener('message', (e) => {
    const msg = JSON.parse(e.data);
    if (msg.event === event) cb(msg.data);
  });
}

export async function getJSON<T>(path: string): Promise<T> {
  const separator = path.includes('?') ? '&' : '?';
  const response = await fetch(`${path}${separator}token=${encodeURIComponent(token)}`);

  if (!response.ok) {
    throw new Error(`request failed: ${response.status}`);
  }

  return response.json() as Promise<T>;
}
