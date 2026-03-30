import { useAppQuit } from './useBackendHelpers';
import { useDeckState } from './useDeckState';
import { useFullscreen } from './useFullscreen';

type UsePresentationRuntimeOptions = {
  previewIndex?: number;
  startIndex?: number;
};

export function usePresentationRuntime<T>(
  slides: T[],
  options: UsePresentationRuntimeOptions = {},
) {
  const previewIndex = options.previewIndex ?? 0;
  const startIndex = options.startIndex ?? 1;
  const { currentIndex, currentSlide, total, canGoNext, canGoPrevious, goTo, next, previous } =
    useDeckState(slides);
  const { isFullscreen, enter, exit } = useFullscreen();
  const { quit, isQuitting } = useAppQuit();
  const isPreviewPage = currentIndex === previewIndex;

  const startShow = async () => {
    await enter();
    goTo(startIndex);
  };

  const toggleFullscreenMode = async () => {
    if (isFullscreen) {
      await exit();
      return;
    }

    await enter();
  };

  return {
    currentIndex,
    currentSlide,
    total,
    canGoNext,
    canGoPrevious,
    isFullscreen,
    isPreviewPage,
    isQuitting,
    presentButtonLabel: isFullscreen ? '结束放映' : '进入放映',
    goTo,
    next,
    previous,
    startShow,
    toggleFullscreenMode,
    quitApp: quit,
  };
}
