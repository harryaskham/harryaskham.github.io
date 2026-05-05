# bd-500769: avoid sort-time surface map lookups in TUI Kitty pending uploads

## What changed

- `SurfaceManager::pending_uploads()` now carries upload-priority metadata from the initial surface-map walk:
  - `ever_uploaded`,
  - precomputed `surface_upload_rank`.
- `SurfaceManager::pending_native_animation_uploads()` now carries:
  - `ever_uploaded`,
  - missing displayed-image state,
  - precomputed `surface_upload_rank`.
- Both sort comparators use the collected metadata instead of re-querying `self.surfaces` by key inside comparator calls.
- Return shapes stay unchanged after sorting/budgeting by stripping internal sort metadata before returning.
- Added a regression test guarding against the old comparator-time surface lookup/rank recomputation shape.

## Why

The Kitty upload pass already walks the surface map to collect eligible pending uploads. Sorting the collected candidates then did additional `HashMap` lookups for each comparator invocation to recover metadata that was available during collection. Carrying that metadata forward removes redundant map probes/rank recomputation while preserving upload priority, stable ordering, and upload-budget behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_500769"` — `tj-9655b58a`, passed
