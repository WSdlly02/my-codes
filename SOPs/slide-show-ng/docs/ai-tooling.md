# AI Tooling Integration Guide

This framework is designed to work with tools that generate standard web projects, such as Google AI Studio, Google Stitch, and similar HTML/CSS/React-oriented design systems.

## Core Principle

Treat AI-generated UI as presentation code only.

- Let generated components own markup, layout, and styling.
- Keep slide index, animation flow, and interaction policy in the frontend runtime.
- Use the Go backend only for capabilities that require local or privileged access.

## Recommended Integration Model

1. Generate or import standard web UI into `frontend/src/`.
2. Keep the generated components mostly intact.
3. Wrap them with the framework hooks from `frontend/src/framework/`.
4. Connect privileged backend features through the generic `backend` client.

## What To Put In Generated Frontend Code

- slide layouts
- typography, colors, spacing, and motion
- local navigation UI
- presentation-specific data models
- animation and transition choreography

## What To Keep Out Of Generated Frontend Code

- assumptions that the backend stores the current page
- network round-trips for every navigation action
- backend-confirmed animation sequencing
- Go-owned slide arrays or deck state machines

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
function DeckShell() {
  const deck = useDeckState(slides);
  const { isFullscreen, enter, exit } = useFullscreen();

  async function startShow() {
    await enter();
    deck.goTo(1);
  }

  async function toggleFullscreenMode() {
    if (isFullscreen) {
      await exit();
      return;
    }

    await enter();
  }

  useKeyboardBindings([
    { keys: ['ArrowRight'], onKey: deck.next },
    { keys: ['ArrowLeft'], onKey: deck.previous },
    { keys: ['f', 'F'], onKey: () => void toggleFullscreenMode() },
  ]);

  return (
    <GeneratedDeck
      currentIndex={deck.currentIndex}
      onJump={deck.goTo}
      onStartShow={() => void startShow()}
      onTogglePresent={() => void toggleFullscreenMode()}
    />
  );
}
```

This keeps AI-generated output easy to replace while preserving a stable runtime contract.
