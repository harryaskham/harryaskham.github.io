# Session summary — bd-0bf8cb: 100vh -> 100dvh progressive enhancement for iOS Safari

## Goal

Continue the caco-web mobile UX polish loop. The codebase
already uses the modern viewport-unit progressive-enhancement
pattern at one site, but three more had been missed.

## Bead(s)

- `bd-0bf8cb` — [caco-web] extend 100vh -> 100dvh progressive enhancement to 3 missed sites (iOS Safari viewport)

## Before state

| Line | Selector | Issue on iOS Safari |
|------|----------|---------------------|
| L6844 | `.agent-tty-pane.tty-fullscreen` height | Cut off under URL bar |
| L6845 | `.agent-tty-pane.tty-fullscreen` max-height | Cut off under URL bar |
| L7525 | `body` min-height | Forces tiny scroll when chrome shows |

`100vh` on iOS Safari represents the *largest* viewport (when
browser chrome is hidden). `100dvh` (dynamic viewport height)
tracks the actual visible viewport, shrinking/growing with the
URL bar and bottom toolbar.

## After state

Each site now uses the same progressive-enhancement pattern as
the existing L177-178 block:

```css
height: 100vh;            /* fallback for older browsers */
height: 100dvh;           /* iOS Safari + modern browsers */
```

For the `!important` lines in `.agent-tty-pane.tty-fullscreen`,
the override line carries `!important` too so cascade order is
preserved.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- dvh fallback added to .agent-tty-pane.tty-fullscreen (height + max-height) and body (min-height), each with a bd-0bf8cb rationale comment.
  - `crates/caco-web/src/tests.rs` -- regression test asserts existing L177-178 pattern preserved, all 3 new declarations present in exact shape, and total `height: 100vh` count equals total `height: 100dvh` count so future blanket rewrites can't drop the pairing.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 452 -> 453; 11 pre-existing failures on main unchanged.

## Operator-takeaway

iOS Safari users now get the full visible viewport for the
fullscreen TTY pane (no cut-off under the URL bar) and body no
longer requires a tiny scroll just to reach the bottom when
browser chrome is showing. Desktop and Android Chrome see no
change (those browsers don't have dynamic viewport chrome that
makes 100vh vs 100dvh different).
