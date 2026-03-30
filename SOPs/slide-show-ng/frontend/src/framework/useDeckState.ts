import { useState } from 'react';

type UseDeckStateOptions = {
  initialIndex?: number;
};

function clampIndex(index: number, total: number) {
  if (total <= 0) {
    return 0;
  }

  return Math.min(Math.max(index, 0), total - 1);
}

export function useDeckState<T>(slides: T[], options: UseDeckStateOptions = {}) {
  const total = slides.length;
  const [currentIndex, setCurrentIndex] = useState(() =>
    clampIndex(options.initialIndex ?? 0, total),
  );

  const goTo = (nextIndex: number) => {
    setCurrentIndex((previousIndex) => {
      const resolvedIndex = clampIndex(nextIndex, total);
      return resolvedIndex === previousIndex ? previousIndex : resolvedIndex;
    });
  };

  const next = () => {
    setCurrentIndex((previousIndex) => clampIndex(previousIndex + 1, total));
  };

  const previous = () => {
    setCurrentIndex((previousIndex) => clampIndex(previousIndex - 1, total));
  };

  const currentSlide = slides[currentIndex];

  return {
    currentIndex,
    currentSlide,
    total,
    canGoNext: currentIndex < total - 1,
    canGoPrevious: currentIndex > 0,
    goTo,
    next,
    previous,
  };
}
