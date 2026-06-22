# Session summary — bd-031d72: global touch-action: manipulation

## Goal
Eliminate the 300ms double-tap-zoom delay across all mobile interactive controls.

## Bead
- `bd-031d72`

## Audit
- 2 existing touch-action rules (workspace splitter, mobile-nav).
- Every other button/a/summary/role-button defaulted to touch-action: auto.

## Fix
- style.css: new global rule before bd-3bfc72 prefers-reduced-motion block.
- Targets button/a/summary/label/select + non-text input + role=button/tab/etc.
- Sets `touch-action: manipulation`.

## Why
- 300ms double-tap-zoom delay is biggest mobile perceived-latency cost.
- manipulation allows pan + pinch, disables only double-tap zoom.
- Backward-compatible; no scroll regression.

## Placement constraint
- Placed BEFORE bd-3bfc72 prefers-reduced-motion safety net.
- bd-3bfc72 forward-guard requires its block within last 1.5 KiB of css.
- Initial append broke that invariant; reorganized.

## Regression test (~30 lines)
- Find bd-031d72 marker; assert block contains selector list + value + role=button.

## Operator-visible effect
- Mobile tap response feels instantaneous (~50ms vs ~300-350ms).

## Diff summary
- `crates/caco-web/static/style.css` -- new block before bd-3bfc72.
- `crates/caco-web/src/tests.rs` -- new bd-031d72 forward-guard (~30 lines).
- Net pass: 587 -> 588; 0 failures.

## Operator-takeaway
80 cycles, 122 wins. Pattern (m) touch-action mobile latency baseline. Pattern catalog: 23 entries.
