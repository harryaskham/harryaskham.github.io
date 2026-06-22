# Session summary — bd-e03e60: per-view scroll-position memory

## Goal
Real UX win (pivot from pattern (x) saturation): preserve scroll position when switching between views.

## Bead
- `bd-e03e60`

## Problem
Scrolling down on agents view (e.g. scrollTop=2400), switching to beads, then back — scroll resets to 0. User has to scroll back to where they were. Real UX pain on long-list views.

Verified: no scroll-preservation logic existed in app.js (`grep scrollPositions|saveScroll|restoreScroll` → 0 matches).

## Fix
Session-scoped per-view scroll memory.

**Storage**: `window._viewScrollPositions` — Map keyed by view-name string. **Session-scoped only** (no sessionStorage/localStorage) to avoid stale-offset drift against a different list size after data churn.

**Save**: in `_switchViewImpl()`, before `state.currentView` mutates, capture outgoing `#content.scrollTop`. Save guard requires `state.currentView !== view` (skip no-op switches).

**Restore**: after view-specific renders dispatch, restore via `requestAnimationFrame` so the new view's renderers have committed their DOM. First visit (no saved entry) defaults to 0.

## Edge case
Logs/chat views auto-scroll-to-bottom on render. If their auto-scroll runs in the same tick as the restore, the rAF ordering means the saved offset wins — desired behavior for "go back to where you were". For first visit (no saved entry), the auto-scroll wins.

## Tests (5 forward-guards)
1. `window._viewScrollPositions` lazy Map init.
2. Save outgoing view scrollTop BEFORE `state.currentView` mutates.
3. Save guard requires `state.currentView !== view`.
4. Restore inside `requestAnimationFrame` with `saved = map.get(view) || 0`.
5. Storage discipline: NO sessionStorage / localStorage in block.

## Operator-visible effect
- Switching views and returning preserves scroll position.
- Reduces fatigue on long-list views (agents, beads, feed, logs).
- No flash/jump (rAF wraps restoration so layout settles first).
- Reload still resets all views to top (avoids stale offset).

## Diff summary
- `crates/caco-web/static/app.js` -- save/restore logic in _switchViewImpl().
- `crates/caco-web/src/tests.rs` -- 5 forward-guard assertions (~55 lines).
- Net pass: 550 -> 551; 0 failures.

## Operator-takeaway
41 cycles, 84 wins. Pivot back to user-visible UX wins after 5 pattern (x) instances. Catalog: 21 entries.
