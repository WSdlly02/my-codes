import { useEffect, useState } from 'react';

export function useFullscreen(target: Element = document.documentElement) {
  const [isFullscreen, setIsFullscreen] = useState(() => Boolean(document.fullscreenElement));

  useEffect(() => {
    const syncState = () => {
      setIsFullscreen(Boolean(document.fullscreenElement));
    };

    document.addEventListener('fullscreenchange', syncState);
    return () => {
      document.removeEventListener('fullscreenchange', syncState);
    };
  }, []);

  const enter = async () => {
    if (document.fullscreenElement) {
      return;
    }

    await target.requestFullscreen?.();
  };

  const exit = async () => {
    if (!document.fullscreenElement) {
      return;
    }

    await document.exitFullscreen?.();
  };

  const toggle = async () => {
    if (document.fullscreenElement) {
      await exit();
      return;
    }

    await enter();
  };

  return {
    isFullscreen,
    enter,
    exit,
    toggle,
  };
}
