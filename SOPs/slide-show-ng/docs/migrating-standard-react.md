# Migrating A Standard React Page

This framework is intended to keep migration friction low for normal React and Vite pages.

## Goal

Keep as much standard frontend code unchanged as possible.

- Preserve layout, styling, and component structure.
- Add a thin presentation runtime wrapper.
- Use the Go backend only for non-browser capabilities.

## What Usually Stays The Same

- JSX structure
- CSS and animations
- design-system components
- page-specific content data
- local component state unrelated to slide navigation

## What Usually Changes

- wrap page switching with `usePresentationRuntime()`
- connect left and right navigation through the runtime
- connect fullscreen behavior through the runtime
- wire preview-page `开始放映`
- wire ending-page `退出程序`

## Minimal Migration Pattern

```tsx
import {
  useKeyboardBindings,
  usePresentationRuntime,
} from './framework';

function DeckShell({ slides }: { slides: Slide[] }) {
  const runtime = usePresentationRuntime(slides);

  useKeyboardBindings([
    { keys: ['ArrowRight'], onKey: runtime.next },
    { keys: ['ArrowLeft'], onKey: runtime.previous },
    { keys: ['f', 'F'], onKey: () => void runtime.toggleFullscreenMode() },
  ]);

  return (
    <GeneratedDeck
      currentIndex={runtime.currentIndex}
      isFullscreen={runtime.isFullscreen}
      onJump={runtime.goTo}
      onStartShow={() => void runtime.startShow()}
      onTogglePresent={() => void runtime.toggleFullscreenMode()}
      onQuit={() => void runtime.quitApp()}
    />
  );
}
```

## Semantic Rules To Preserve

- The first page may be a preview page and still count as a real slide.
- Only the preview-page `开始放映` action should both enter fullscreen and jump to the next slide.
- `F` should only toggle fullscreen.
- Left and right arrows should only navigate.
- The global `present` button should only reflect and toggle fullscreen state on presentation pages.
- `退出程序` is an app action, not a slideshow state action.

## When To Use The Backend

Use Go when the frontend needs:

- file reads or writes
- directory or file watching
- local process or OS integration
- realtime data push from local sources
- explicit application quit

Do not use Go for:

- current slide ownership
- navigation approval
- animation orchestration
- fullscreen control logic

## Optional Thin Hooks

The framework also includes optional frontend-side helpers for backend access:

- `useBackendEvent`
- `useBackendQuery`
- `useAppQuit`

These are convenience hooks only. They are not required for migration.
