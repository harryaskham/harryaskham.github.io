# Session summary — bd-8ac00b: app-owned scroll restoration

## Goal
Eliminate browser/app race on history navigation scroll restoration.

## Bead
- `bd-8ac00b`

## Audit
- _viewScrollPositions handles per-view scroll.
- Default 'auto' restored scroll BEFORE setActiveView() RAF could.

## Fix
- DOMContentLoaded init sets history.scrollRestoration = 'manual' (feature-detect guarded).

## Why
- App becomes single source of truth.
- Eliminates back/forward scroll-jump flash.

## Regression test (~25 lines)
- Feature-detect literal + assignment literal both required.

## Operator-visible effect
- Smoother back/forward navigation; no scroll-jump.

## Diff summary
- `crates/caco-web/static/app.js` -- DOMContentLoaded init prelude.
- `crates/caco-web/src/tests.rs` -- new bd-8ac00b forward-guard (~25 lines).
- Net pass: 593 -> 594; 0 failures.

## Operator-takeaway
86 cycles, 128 wins. Pattern (s) app-owned scroll restoration. Pattern catalog: 27 entries.
