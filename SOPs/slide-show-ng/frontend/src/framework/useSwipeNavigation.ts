import { TouchEvent as ReactTouchEvent, useEffectEvent, useRef } from "react";

type UseSwipeNavigationOptions = {
  onNext: () => void;
  onPrevious: () => void;
  enabled?: boolean;
  threshold?: number;
};

export function useSwipeNavigation({
  onNext,
  onPrevious,
  enabled = true,
  threshold = 50,
}: UseSwipeNavigationOptions) {
  const touchStartX = useRef<number | null>(null);

  const handleTouchStart = useEffectEvent((event: ReactTouchEvent) => {
    if (!enabled) return;
    touchStartX.current = event.touches[0]?.clientX ?? null;
  });

  const handleTouchEnd = useEffectEvent((event: ReactTouchEvent) => {
    if (!enabled || touchStartX.current === null) {
      return;
    }

    const endX = event.changedTouches[0]?.clientX;
    if (typeof endX !== "number") {
      touchStartX.current = null;
      return;
    }

    const deltaX = endX - touchStartX.current;
    touchStartX.current = null;

    if (Math.abs(deltaX) <= threshold) {
      return;
    }

    if (deltaX < 0) {
      onNext();
      return;
    }

    onPrevious();
  });

  return {
    handlers: {
      onTouchStart: handleTouchStart,
      onTouchEnd: handleTouchEnd,
    },
  };
}
