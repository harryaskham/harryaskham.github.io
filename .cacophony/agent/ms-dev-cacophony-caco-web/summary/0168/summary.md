# Session summary — bd-24f084: viewport-fit=cover parity

## Goal
Pattern (m) viewport-fit=cover parity: notifications.html viewport meta missed `viewport-fit=cover` (3 of 4 entry HTML files had it).

## Bead
- `bd-24f084`

## Audit
- 4 entry HTML viewport meta inventory: 3 had viewport-fit=cover, 1 missing.

## Fix
- notifications.html viewport meta + `viewport-fit=cover`.

## Why
- Enables `env(safe-area-inset-*)` non-zero offsets on iOS notched devices.
- Background extends edge-to-edge instead of white/dark bars.

## Regression test (~30 lines)
- Per file: find viewport meta, bound to >, assert viewport-fit=cover.
- Iterates all 4 entry HTML files.

## Operator-visible effect
- iPhone/iPad Notifications page uses full screen on landscape.

## Diff summary
- `crates/caco-web/static/notifications.html` -- viewport meta + viewport-fit=cover.
- `crates/caco-web/src/tests.rs` -- new bd-24f084 forward-guard (~30 lines).
- Net pass: 584 -> 585; 0 failures.

## Operator-takeaway
76 cycles, 119 wins. Pattern (m) viewport parity. Pattern catalog: 22 entries.
