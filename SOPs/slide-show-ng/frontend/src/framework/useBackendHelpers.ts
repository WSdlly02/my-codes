import { useEffect, useState } from 'react';
import { backend } from './backend';

export function useBackendEvent<T>(event: string, onEvent: (data: T) => void) {
  useEffect(() => backend.on<T>(event, onEvent), [event, onEvent]);
}

export function useBackendQuery<T>(cmd: string, payload?: unknown) {
  const [data, setData] = useState<T | null>(null);
  const [error, setError] = useState<Error | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    let cancelled = false;

    setLoading(true);
    setError(null);

    void backend.call<T>(cmd, payload)
      .then((result) => {
        if (cancelled) {
          return;
        }

        setData(result);
        setLoading(false);
      })
      .catch((err: Error) => {
        if (cancelled) {
          return;
        }

        setError(err);
        setLoading(false);
      });

    return () => {
      cancelled = true;
    };
  }, [cmd, payload]);

  return { data, error, loading };
}

export function useAppQuit() {
  const [isQuitting, setIsQuitting] = useState(false);

  const quit = async () => {
    if (isQuitting) {
      return;
    }

    setIsQuitting(true);

    try {
      await backend.call<{ ok: boolean }>('quit_app');
      window.close();
    } catch {
      setIsQuitting(false);
    }
  };

  return { quit, isQuitting };
}

export function useBackendCapabilities() {
  const [data, setData] = useState<{
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
    app?: {
      quit?: boolean;
    };
    demo?: {
      systemInfoStream?: boolean;
    };
  } | null>(null);
  const [error, setError] = useState<Error | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    let cancelled = false;

    setLoading(true);
    setError(null);

    void backend.getJSON<NonNullable<typeof data>>('/api/capabilities')
      .then((result) => {
        if (cancelled) {
          return;
        }

        setData(result);
        setLoading(false);
      })
      .catch((err: Error) => {
        if (cancelled) {
          return;
        }

        setError(err);
        setLoading(false);
      });

    return () => {
      cancelled = true;
    };
  }, []);

  return { data, error, loading };
}
