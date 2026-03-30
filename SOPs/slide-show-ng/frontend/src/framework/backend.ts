type BackendMessage = {
  id?: string;
  result?: unknown;
  error?: string;
  event?: string;
  data?: unknown;
};

type EventHandler<T> = (data: T) => void;

function resolveToken(searchParam = 'token') {
  const search = new URLSearchParams(window.location.search);
  return search.get(searchParam) ?? '';
}

export function createBackendClient(searchParam = 'token') {
  const token = resolveToken(searchParam);
  const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  const ws = new WebSocket(
    `${wsProtocol}//${window.location.host}/ws?token=${encodeURIComponent(token)}`,
  );
  const waitUntilOpen = new Promise<void>((resolve, reject) => {
    if (ws.readyState === WebSocket.OPEN) {
      resolve();
      return;
    }

    ws.addEventListener('open', () => resolve(), { once: true });
    ws.addEventListener('error', () => reject(new Error('websocket connection failed')), {
      once: true,
    });
  });
  const eventHandlers = new Map<string, Set<EventHandler<unknown>>>();

  ws.addEventListener('message', (event) => {
    const message = JSON.parse(event.data) as BackendMessage;
    if (!message.event) {
      return;
    }

    const handlers = eventHandlers.get(message.event);
    if (!handlers) {
      return;
    }

    for (const handler of handlers) {
      handler(message.data);
    }
  });

  const call = async <T>(cmd: string, payload?: unknown): Promise<T> => {
    await waitUntilOpen;

    const id = crypto.randomUUID();

    return new Promise<T>((resolve, reject) => {
      const handleMessage = (event: MessageEvent<string>) => {
        const message = JSON.parse(event.data) as BackendMessage;
        if (message.id !== id) {
          return;
        }

        ws.removeEventListener('message', handleMessage as EventListener);

        if (message.error) {
          reject(new Error(message.error));
          return;
        }

        resolve(message.result as T);
      };

      ws.addEventListener('message', handleMessage as EventListener);
      ws.send(JSON.stringify({ id, cmd, payload }));
    });
  };

  const on = <T>(event: string, handler: EventHandler<T>) => {
    const currentHandlers = eventHandlers.get(event) ?? new Set<EventHandler<unknown>>();
    currentHandlers.add(handler as EventHandler<unknown>);
    eventHandlers.set(event, currentHandlers);

    return () => {
      const nextHandlers = eventHandlers.get(event);
      if (!nextHandlers) {
        return;
      }

      nextHandlers.delete(handler as EventHandler<unknown>);
      if (nextHandlers.size === 0) {
        eventHandlers.delete(event);
      }
    };
  };

  const getJSON = async <T>(path: string): Promise<T> => {
    const separator = path.includes('?') ? '&' : '?';
    const response = await fetch(`${path}${separator}token=${encodeURIComponent(token)}`);

    if (!response.ok) {
      throw new Error(`request failed: ${response.status}`);
    }

    return response.json() as Promise<T>;
  };

  return {
    token,
    call,
    on,
    getJSON,
  };
}

export const backend = createBackendClient();
