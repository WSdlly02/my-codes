import { backend } from './framework';

export type BackendCapabilities = {
  filesystem: {
    read: boolean;
    write: boolean;
    watch: boolean;
  };
  realtime: {
    events: boolean;
  };
  os: {
    supported: boolean;
  };
};

export async function bootstrapBackendCapabilities(): Promise<BackendCapabilities> {
  return backend.getJSON<BackendCapabilities>('/api/capabilities');
}

export function bindBackendEvent<T>(event: string, onEvent: (data: T) => void) {
  return backend.on<T>(event, onEvent);
}
