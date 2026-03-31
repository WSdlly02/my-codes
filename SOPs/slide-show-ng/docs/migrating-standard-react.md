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
- optionally wrap navigation animation with `useAnimatedDeck()`
- connect left and right navigation through the runtime
- connect fullscreen behavior through the runtime
- wire preview-page `开始放映`
- wire ending-page `退出程序`

## Minimal Migration Pattern

```tsx
import {
  useAnimatedDeck,
  useKeyboardBindings,
  usePresentationRuntime,
} from './framework';

function ProductDeck({ slides }: { slides: Slide[] }) {
  const runtime = usePresentationRuntime(slides);
  const animatedDeck = useAnimatedDeck({
    currentIndex: runtime.currentIndex,
    total: runtime.total,
    goTo: runtime.goTo,
  });

  useKeyboardBindings([
    { keys: ['ArrowRight'], onKey: animatedDeck.next },
    { keys: ['ArrowLeft'], onKey: animatedDeck.previous },
    { keys: ['f', 'F'], onKey: () => void runtime.toggleFullscreenMode() },
  ]);

  return (
    <main>
      {slides.map((slide, index) => (
        <section key={index}>
          {/* your existing layout */}
        </section>
      ))}

      <button onClick={() => void runtime.startShow()} type="button">
        开始放映
      </button>

      <button onClick={() => void runtime.toggleFullscreenMode()} type="button">
        {runtime.presentButtonLabel}
      </button>

      <button onClick={() => void runtime.quitApp()} type="button">
        退出程序
      </button>
    </main>
  );
}
```

## File Structure Recommendation

- `frontend/src/framework/`: framework-owned hooks only
- `frontend/src/decks/<deck-name>/`: deck content, deck CSS, deck-specific controls
- `frontend/src/DeckApp.tsx`: exports the currently active deck

This keeps future deck swaps and backports localized to content files instead of framework files.

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
- `useBackendCapabilities`

These are convenience hooks only. They are not required for migration.

`useBackendQuery` treats `options.key` as the query identity. If the payload is an object or array, provide an explicit key instead of relying on implicit serialization:

```ts
const { data } = useBackendQuery('read_file', { path: currentPath }, { key: currentPath });
```

This rule exists to keep refetch behavior predictable and query identity stable. It is not a backend type-safety feature.
