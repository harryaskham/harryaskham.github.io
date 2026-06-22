# Session summary — bd-9b7346: scrollbar-gutter stable on main scroller

## Goal
Eliminate horizontal layout shift from scrollbar appearance/disappearance on the main #content scroller.

## Bead
- `bd-9b7346`

## Audit
- grep scrollbar-gutter -> 0 hits.
- #content is canonical main scroller.

## Fix
- style.css #content: + `scrollbar-gutter: stable;` with rationale comment.

## Why
- Scrollbar appearing/disappearing causes ~12-15px horizontal shift.
- Triggers CLS on every dynamic content height change.
- Backward-compatible; older browsers ignore.

## Scope
- Only #content (main scroller); per-view containers don't toggle overflow.

## Regression test (~35 lines)
- Marker-anchored block: back-window for "#content {" + forward-window for "scrollbar-gutter: stable;".

## Test calibration
- Initial 400-byte forward window failed because rationale comment was ~450 chars; increased to 800.

## Operator-visible effect
- No horizontal jitter when toasts/badges/lists change.

## Diff summary
- `crates/caco-web/static/style.css` -- #content scrollbar-gutter: stable.
- `crates/caco-web/src/tests.rs` -- new bd-9b7346 forward-guard (~35 lines).
- Net pass: 588 -> 589; 0 failures.

## Operator-takeaway
81 cycles, 123 wins. Pattern (m) CLS reservation for scroll containers. Pattern catalog: 23 entries.
