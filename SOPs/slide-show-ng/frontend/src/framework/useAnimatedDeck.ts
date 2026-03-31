import { useEffect, useRef, useState } from "react";

type UseAnimatedDeckOptions = {
  currentIndex: number;
  total: number;
  goTo: (index: number) => void;
  exitDurationMs?: number;
  enterDurationMs?: number;
};

export function useAnimatedDeck({
  currentIndex,
  total,
  goTo,
  exitDurationMs = 280,
  enterDurationMs = 620,
}: UseAnimatedDeckOptions) {
  const [exitingIndex, setExitingIndex] = useState<number | null>(null);
  const [isAnimating, setIsAnimating] = useState(false);
  const exitTimerRef = useRef<number | null>(null);
  const enterTimerRef = useRef<number | null>(null);

  useEffect(() => {
    return () => {
      window.clearTimeout(exitTimerRef.current!);
      window.clearTimeout(enterTimerRef.current!);
    };
  }, []);

  const animateTo = (nextIndex: number) => {
    if (
      isAnimating ||
      nextIndex === currentIndex ||
      nextIndex < 0 ||
      nextIndex >= total
    ) {
      return;
    }

    window.clearTimeout(exitTimerRef.current!);
    window.clearTimeout(enterTimerRef.current!);

    setIsAnimating(true);
    setExitingIndex(currentIndex);

    exitTimerRef.current = window.setTimeout(() => {
      goTo(nextIndex);

      enterTimerRef.current = window.setTimeout(() => {
        setExitingIndex(null);
        setIsAnimating(false);
        enterTimerRef.current = null;
      }, enterDurationMs);

      exitTimerRef.current = null;
    }, exitDurationMs);
  };

  const next = () => {
    animateTo(currentIndex + 1);
  };

  const previous = () => {
    animateTo(currentIndex - 1);
  };

  return {
    exitingIndex,
    isAnimating,
    animateTo,
    next,
    previous,
  };
}
