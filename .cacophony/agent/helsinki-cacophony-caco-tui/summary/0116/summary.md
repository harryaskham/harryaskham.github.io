# bd-e05c0e: use unstable sort for TUI upload candidates

## What changed

- `SurfaceManager::pending_native_animation_uploads()` now uses `sort_unstable_by()` for upload candidates.
- `SurfaceManager::pending_uploads_with_summary()` now uses `sort_unstable_by()` for regular upload candidates.
- Both sorts retain the unique surface key as the final tiebreaker, so upload-budget ordering remains deterministic.
- Added regression coverage that both hot-path candidate sorts are unstable and still include the key tiebreaker.

## Why

The upload collectors only sort to enforce deterministic priority before truncating to the per-frame upload budget. Candidate keys are unique and already included as the final comparator, so stable sorting is unnecessary overhead on graphics-heavy frames. Using unstable sort preserves deterministic priority/key ordering while reducing sort cost.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e05c0e"` — `tj-a1e11607`, passed
