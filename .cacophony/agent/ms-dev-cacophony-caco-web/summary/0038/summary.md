# Session summary — bd-34730a: scroll-top button uses IntersectionObserver, click rewired

## Goal

Continue the caco-web frontend perf/visual/UX polish loop. Started
as a perf cleanup of a per-event scroll handler; discovered the
entire feature has been broken because the body has
`overflow: hidden` and the page does not scroll at the window
level. Fixed both perf and functionality in one slice.

## Bead(s)

- `bd-34730a` — [caco-web] scroll-top button uses per-frame scroll handler instead of IntersectionObserver

## Before state

`app.js:183` (init):

```js
window.addEventListener('scroll', () => {
    const btn = el('scroll-top-btn');
    if (btn) btn.classList.toggle('visible', window.scrollY > 300);
}, { passive: true });
```

`index.html:1130`:

```html
<button class="scroll-top-btn" id="scroll-top-btn"
        onclick="window.scrollTo({top:0,behavior:'smooth'})"
        data-tooltip="Scroll to top" aria-label="Scroll to top">
```

Two latent bugs:

1. **Per-event scroll handler** -- ran 60-120 times/sec during
   continuous scroll on a fast device, doing a getElementById +
   classList.toggle on every tick.
2. **AND it was dead code**. style.css:131 sets
   `html, body { overflow: hidden; }`. window.scrollY is
   ALWAYS 0. The button never became visible via this handler.
   The inline onclick="window.scrollTo(...)" was also a no-op for
   the same reason -- the actual scroll container is #content,
   which has its own `overflow-y: auto`.

The scroll-to-top affordance has been broken since the page
adopted the overflow:hidden layout.

## After state

- New sentinel inside `<main id="content">`:
  `<div id="scroll-top-sentinel" aria-hidden="true"></div>`.
- New CSS rule pins the sentinel at `position: absolute; top:
  300px; width: 1px; height: 1px; pointer-events: none;`
  relative to `#content` (which already has `position: relative`).
- New `setupScrollTopButton()` (called during init) uses
  `IntersectionObserver` to watch the sentinel; toggles `.visible`
  on the button only at threshold transitions, off the main
  thread. Returns early if IO is supported.
- Click handler rewired to scroll `#content` (the REAL scroll
  container) with `document.scrollingElement` / `documentElement`
  fallback chain. Inline onclick removed (was dead).
- Fallback for browsers without IntersectionObserver listens on
  `#content` (not window) with a single cached element reference
  and rAF-debounced toggle.
- The button now actually works AND is much cheaper.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/index.html` -- new sentinel inside <main id="content">; inline onclick removed; both blocks carry inline rationale comments.
  - `crates/caco-web/static/style.css` -- new #scroll-top-sentinel rule next to .scroll-top-btn.
  - `crates/caco-web/static/app.js` -- dead per-event scroll handler removed; new setupScrollTopButton() with IO + click-target retarget + IO fallback.
  - `crates/caco-web/src/tests.rs` -- regression test pinning sentinel HTML, CSS rule, dead-code removal, IO setup, click handler retarget, and fallback path.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 444 -> 445; 11 pre-existing failures on main unchanged.

## Operator-takeaway

The scroll-to-top button **now works** -- previously it was
silently broken because the body has overflow:hidden and the
handler watched the wrong container. Visibility is also cheaper:
IntersectionObserver fires off-main-thread only at threshold
transitions instead of a getElementById + classList.toggle on
every scroll event tick. Sighted UX is unchanged when scrolled
past 300px from the top of #content.
