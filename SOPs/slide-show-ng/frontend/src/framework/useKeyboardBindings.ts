import { useEffect, useEffectEvent } from 'react';

export type KeyboardBinding = {
  keys: string[];
  onKey: (event: KeyboardEvent) => void;
  preventDefault?: boolean;
};

type UseKeyboardBindingsOptions = {
  enabled?: boolean;
  target?: Document | HTMLElement | null;
};

export function useKeyboardBindings(
  bindings: KeyboardBinding[],
  options: UseKeyboardBindingsOptions = {},
) {
  const { enabled = true, target } = options;
  const handleKeyDown = useEffectEvent((event: KeyboardEvent) => {
    for (const binding of bindings) {
      if (!binding.keys.includes(event.key)) {
        continue;
      }

      if (binding.preventDefault !== false) {
        event.preventDefault();
      }

      binding.onKey(event);
      break;
    }
  });

  useEffect(() => {
    if (!enabled) {
      return;
    }

    const eventTarget = target ?? document;
    eventTarget.addEventListener('keydown', handleKeyDown as EventListener);

    return () => {
      eventTarget.removeEventListener('keydown', handleKeyDown as EventListener);
    };
  }, [enabled, handleKeyDown, target]);
}
