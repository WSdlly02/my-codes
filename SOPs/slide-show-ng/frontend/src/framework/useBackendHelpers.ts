import { useEffect, useEffectEvent, useMemo, useState } from "react";
import { backend } from "./backend";

export function useBackendEvent<T>(event: string, onEvent: (data: T) => void) {
  const handleEvent = useEffectEvent((data: T) => {
    onEvent(data);
  });

  useEffect(() => backend.on<T>(event, handleEvent), [event, handleEvent]);
}

type BackendQueryOptions = {
  enabled?: boolean;
  key?: string;
};

function resolveQueryKey(payload: unknown, explicitKey?: string) {
  if (explicitKey) {
    return explicitKey;
  }

  if (typeof payload === "undefined") {
    return "__undefined__";
  }

  if (
    typeof payload === "string" ||
    typeof payload === "number" ||
    typeof payload === "boolean"
  ) {
    return String(payload);
  }

  throw new Error(
    'useBackendQuery requires an explicit "key" in options when providing a complex payload to avoid unstable object serialization issues.',
  );
}

export function useBackendQuery<T>(
  cmd: string,
  payload?: unknown,
  options: BackendQueryOptions = {},
) {
  const [data, setData] = useState<T | null>(null);
  const [error, setError] = useState<Error | null>(null);
  const [loading, setLoading] = useState(true);
  const queryKey = useMemo(
    () => resolveQueryKey(payload, options.key),
    [options.key, payload],
  );
  const enabled = options.enabled ?? true;

  useEffect(() => {
    if (!enabled) {
      setLoading(false);
      return;
    }

    let cancelled = false;

    setLoading(true);
    setError(null);

    void backend
      .call<T>(cmd, payload)
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
  }, [cmd, enabled, payload, queryKey]);

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
      await backend.call<{ ok: boolean }>("quit_app");
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

    void backend
      .getJSON<NonNullable<typeof data>>("/api/capabilities")
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
