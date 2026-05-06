# bd-9f0d9a: reuse modal overlap subset for TUI title suppression

## What changed

- `BorderIntegration::suppress_for_modals()` now identifies the first modal whose rect overlaps the panel outer rect, then materializes only the overlapping modal subset for that panel.
- Border segment and title-decoration overlap checks reuse `overlapping_modals` instead of scanning the full `modal_areas` list again.
- The previous outer-rect preflight remains: panels with no overlapping modal are skipped before segment/title work.
- Added source-shape coverage that segment/title checks iterate `&overlapping_modals` and do not loop over full `modal_areas`.

## Why

Modal suppression is a correctness backstop to de-draw bitmap borders behind overlays. It can run for every tracked panel while modal overlays are visible. Reusing a per-panel overlapping-modal subset avoids repeated scans over unrelated overlay rectangles for segment and title-decor checks, while preserving the same de-draw behavior for panels that do overlap a modal.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9f0d9a"` — `tj-698c63b5`, passed
