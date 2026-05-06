# bd-4829bd: partially select TUI upload budgets before sorting

## What changed

- `pending_native_animation_uploads()` now uses `select_nth_unstable_by()` when more candidates exist than `max_uploads_per_frame`, truncates to the selected budget subset, then sorts only that subset for deterministic emission order.
- `pending_uploads_with_summary()` applies the same partial-selection pattern for regular bitmap uploads.
- Existing priority/key comparators are unchanged, so emitted upload ordering remains deterministic.
- Added regression coverage that both native and regular upload collectors select the budget partition before sorting.

## Why

Graphics bursts can create many pending surfaces while the configured per-frame upload budget emits only a subset. Fully sorting every candidate before truncation is unnecessary. Partial selection reduces hot-path sort work to choose the budget subset, then sorts only the small emitted subset.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_4829bd"` — `tj-71b656a5`, passed
