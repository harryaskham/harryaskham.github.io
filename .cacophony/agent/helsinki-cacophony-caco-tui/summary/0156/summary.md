# bd-e8bcfa: cache active placement emptiness for orphan cleanup

## What changed

- Added `SurfaceManager::has_active_image_placement()`.
- `cleanup_orphaned_image_inner()` and `release_displayed_surface_image()` now use the helper instead of directly probing `active_image_placements`.
- The helper checks `active_image_placements.is_empty()` before `contains_key()`, skipping guaranteed-miss HashMap probes in sessions/frames with no active retained/shared placements.
- Added source/runtime coverage proving orphan cleanup and release paths use the helper and preserving empty-map behavior.

## Why

Graphics retire/orphan cleanup paths are correctness-critical and can run often while switching views, tabs, workspaces, and modal overlays. In ordinary non-retained or cold graphics paths the active-placement map is frequently empty, so direct `contains_key()` probes are guaranteed misses. The helper keeps behavior identical while avoiding those probes on the common empty-map path.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e8bcfa"` — `tj-b81d2e0b`, warning run before mut cleanup; `tj-8dfe4dcc`, passed clean after fix
- `caco test run --wait --command "cargo test -p caco-tui active_placement"` — `tj-09f04af6`, warning run before mut cleanup; `tj-3c0cd7bb`, passed clean after fix
