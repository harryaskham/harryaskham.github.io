# bd-d74ec7: collapse unchanged border live checks

## What changed

- Collapsed the unchanged-panel border fast path in `BorderIntegration::register_panel()` so `SurfaceManager::mark_live()` is the liveness check for each cached segment.
- Removed the unchanged-path pre-scan through `cached_panel_surfaces_live()` before the `mark_live()` loop.
- The path still falls back to full registration if any required cached segment is missing.
- Title-decoration correctness is preserved: when a title decoration is required but missing, the path falls back; otherwise the guarded reuse helper handles existing decoration mark-live/retire work.
- Added source-shape coverage to keep the unchanged fast path from reintroducing a liveness pre-scan.

## Why

On stable graphics frames, unchanged borders are the common case. The old path first probed all required segment surfaces with `get()`, then iterated the same segments again with `mark_live()`. This doubled hash-map probes for unchanged borders. The new path uses `mark_live()` as both the presence check and live marker, preserving stale/de-draw correctness with fewer per-frame map lookups.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d74ec7"` — `tj-0bcccdb4`, passed with one warning from an unused assignment.
- Removed the unused assignment.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_d74ec7"` — `tj-8e790ace`, passed cleanly.
