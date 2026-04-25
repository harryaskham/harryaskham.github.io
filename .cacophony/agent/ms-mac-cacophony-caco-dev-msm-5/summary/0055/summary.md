# Session summary — Web summaries load-more count clamp

## Goal

Continue web summaries long-list polish by making the load-more control robust against stale or lagging daemon totals.

## Bead(s)

- `bd-b0948d` — Web summaries: clamp load-more remaining count
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The web summaries list rendered a load-more button when loaded rows were below the daemon-reported total.
- The button text and aria-label used `STATE.total - STATE.items.length` directly.
- If list totals ever lagged appended items, the rendered remaining count could become confusing.

## After state

- The load-more remaining count is clamped with `Math.max(0, STATE.total - STATE.items.length)`.
- Button text and `aria-label` now share the safe non-negative count.
- Existing pagination behavior remains unchanged.

## Diff summary

- Commits: current `bd-b0948d` implementation commit
- Files touched:
  - `crates/caco-web/static/summaries.js`
- Tests:
  - `node --check crates/caco-web/static/summaries.js` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: web summaries no longer risk displaying negative remaining counts in the load-more control.

## Operator-takeaway

This is a small resilience polish for long histories: the web summaries pagination control now stays tidy even if daemon totals and appended list length drift briefly.
