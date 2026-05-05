# bd-460495: skip modal suppression segment checks for non-overlapping panels

## What changed

- `BorderIntegration::suppress_for_modals()` now checks whether a panel's outer rect overlaps any modal rect before walking its border segments and title decoration.
- Panels that cannot overlap the modal skip all segment/decor overlap checks and keep their cached surface-key state and registered surfaces.
- Added regression coverage proving a non-overlapping panel remains registered while an overlapping panel is still suppressed.

## Why

Modal suppression is a correctness backstop that de-draws base-layer bitmap borders behind overlays. On dense layouts, walking every non-modal panel's eight border segments against every modal rect is unnecessary when the panel's outer rect is disjoint from all overlays. The preflight preserves de-draw correctness while trimming overlay-frame work and avoiding needless cache/bookkeeping churn.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_460495"` — `tj-cb0abbc1`, passed
