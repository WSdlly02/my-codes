# AI Tooling Integration Guide

This framework is designed to work with tools that generate standard web projects, such as Google AI Studio, Google Stitch, and similar HTML/CSS/React-oriented design systems.

## Core Principle

Treat AI-generated UI as presentation code only.

- Let generated components own markup, layout, and styling.
- Keep slide index, animation flow, and interaction policy in the frontend runtime.
- Use the Go backend only for capabilities that require local or privileged access.

## Recommended Integration Model

1. Generate or import standard web UI into `frontend/src/decks/<deck-name>/`.
2. Keep the generated components mostly intact.
3. Use hooks from `frontend/src/framework/` to add slideshow/runtime behavior.
4. Connect privileged backend features through the generic `backend` client or thin backend hooks.
5. Point `frontend/src/DeckApp.tsx` at the active deck entry.

## What To Put In Generated Frontend Code

- slide layouts
- typography, colors, spacing, and motion
- local navigation UI
- present button placement and control-bar layout
- presentation-specific data models
- animation and transition choreography

## What To Keep Out Of Generated Frontend Code

- assumptions that the backend stores the current page
- network round-trips for every navigation action
- backend-confirmed animation sequencing
- Go-owned slide arrays or deck state machines

## File Placement

Use this split to keep framework and content decoupled:

- `frontend/src/framework/`: reusable hooks and low-level helpers only
- `frontend/src/decks/<deck-name>/`: deck-specific React components, CSS, and assets
- `frontend/src/DeckApp.tsx`: active deck selector or direct deck export

Avoid moving deck-specific controls or theme styles into `frontend/src/framework/`.

## Fullscreen Semantics

Do not conflate fullscreen with presentation flow.

- The preview page is still a normal slide and should remain in the deck order.
- The preview page may include a centered `开始放映` button.
- Starting presentation from the preview page is a special action: it enters fullscreen and jumps to the next slide.
- Outside that one action, fullscreen toggles must not change the current slide.
- A global `present` button should exist only on presentation pages, not on the preview page.
- That global `present` button may reflect fullscreen state, but it should not silently own navigation rules.
- On presentation pages, `present` means enter or leave fullscreen only.
- On the ending page, `present` and `退出程序` are different actions and must remain separate controls.
- Left and right navigation should stay independent from fullscreen entry and exit.
- `F` should toggle fullscreen only.
- Left and right arrows should navigate only.

## Framework APIs

### `usePresentationRuntime`

Use this hook as the primary slideshow runtime boundary:

```ts
const runtime = usePresentationRuntime(slides);
```

It centralizes:

- current slide index
- preview-page detection
- fullscreen state
- preview-page `startShow()`
- generic `toggleFullscreenMode()`
- application-level `quitApp()`

### `useDeckState`

Use this hook to own deck state inside the browser:

```ts
const { currentIndex, total, goTo, next, previous } = useDeckState(slides);
```

This is where page count, bounds clamping, and current slide selection should live.

### `useKeyboardBindings`

Use this hook to map keys to frontend actions:

```ts
useKeyboardBindings([
  { keys: ['ArrowRight'], onKey: () => next() },
  { keys: ['ArrowLeft'], onKey: () => previous() },
  { keys: ['f', 'F'], onKey: () => void fullscreen.toggle() },
]);
```

### `useFullscreen`

Use this hook to manage fullscreen purely in the browser:

```ts
const { isFullscreen, enter, exit } = useFullscreen();
```

Prefer explicit semantic wrappers over wiring fullscreen directly into navigation. For example:

```ts
async function startShow() {
  await enter();
  goTo(1);
}

async function toggleFullscreenMode() {
  if (isFullscreen) {
    await exit();
    return;
  }

  await enter();
}
```

Here `startShow()` is the preview-page action, while `toggleFullscreenMode()` is the generic present action.

### `backend`

Use the backend bridge for privileged capabilities only:

```ts
import { backend } from './framework';

const health = await backend.getJSON('/api/ping');
const result = await backend.call('ping');
const unsubscribe = backend.on('data_updated', (payload) => {
  console.log(payload);
});
```

### Optional Thin Hooks

The framework also includes optional thin helpers:

```ts
const { data, loading, error } = useBackendQuery('get_system_info');
useBackendEvent('system_info_updated', (payload) => {
  console.log(payload);
});
const { quit, isQuitting } = useAppQuit();
```

When `useBackendQuery` receives a complex payload, pass `options.key` explicitly:

```ts
const { data } = useBackendQuery(
  'read_file',
  { path: currentPath },
  { key: currentPath },
);
```

This key is the query identity used by the hook. The rule is about stable refetch semantics, not backend type safety.

## Backend Contract

The backend should be used for:

- filesystem read/write/watch
- realtime data ingestion and push
- local OS integration

The backend should not be used for:

- current slide ownership
- navigation validation
- keyboard handling
- touch handling
- animation timing

## Practical Pattern

A generated page can stay visually untouched while you add a thin runtime wrapper:

```tsx
function GeneratedDeckPage({ slides }: { slides: Slide[] }) {
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
    <>
      {slides.map((slide, index) => {
        const className = [
          'slide',
          runtime.currentIndex === index ? 'active' : '',
          animatedDeck.exitingIndex === index ? 'exit' : '',
        ]
          .filter(Boolean)
          .join(' ');

        return (
          <section className={className} key={index}>
            {/* generated content goes here */}
          </section>
        );
      })}

      <button onClick={() => void runtime.startShow()} type="button">
        开始放映
      </button>

      <button onClick={() => void runtime.toggleFullscreenMode()} type="button">
        {runtime.presentButtonLabel}
      </button>
    </>
  );
}
```

This keeps AI-generated output easy to replace while preserving a stable runtime contract.
