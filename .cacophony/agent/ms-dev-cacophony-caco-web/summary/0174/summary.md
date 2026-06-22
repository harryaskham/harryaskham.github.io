# Session summary — bd-458ea6: terminal.html WCAG zoom-enable fix

## Goal
Pattern (m) WCAG zoom-enable parity: terminal.html violated WCAG 1.4.4/2.5.5 by disabling pinch-zoom.

## Bead
- `bd-458ea6`

## Audit
- grep "maximum-scale|user-scalable" across HTML.
- Only terminal.html declared either flag.

## Fix
- terminal.html viewport: removed `maximum-scale=1, user-scalable=no`.
- Resulting: `width=device-width, initial-scale=1, viewport-fit=cover` (parity).
- Replaced inline with rationale comment.

## Why
- WCAG 1.4.4 (Resize text) + 2.5.5 (Target Size) violation.
- xterm.js respects browser zoom; system zoom is correct a11y escape.
- iOS Safari/Android Chrome ignore these tokens since 2019.

## Regression test (~40 lines)
- 4 entry HTML × 2 banned tokens (user-scalable=no, maximum-scale=1).

## Operator-visible effect
- iOS/Android operators can pinch-zoom the agent terminal.

## Diff summary
- `crates/caco-web/static/terminal.html` -- removed zoom-blocking tokens + rationale.
- `crates/caco-web/src/tests.rs` -- new bd-458ea6 forward-guard (~40 lines).
- Net pass: 589 -> 590; 0 failures.

## Operator-takeaway
82 cycles, 124 wins. Pattern (m) WCAG zoom-enable parity. Pattern catalog: 24 entries.
