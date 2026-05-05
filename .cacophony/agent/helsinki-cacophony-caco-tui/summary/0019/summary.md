# bd-69c830: maintain O(1) active animated-surface count

## What changed

- Added `SurfaceManager::active_animated_surface_count`.
- `SurfaceManager::has_active_animated_surfaces()` now returns from that count instead of scanning all registered surfaces.
- Updated surface state transitions to maintain the count when surfaces switch between static, redraw-driven animated, daemon-image, native-animation, refresh, retire, and clear paths.
- Added regression coverage that terminal-native animations do not keep the redraw-driven activity gate active, and that switching back to redraw-driven animation restores it.

## Why

bd-c65cfa parked the high-frequency animation ticker when no animated surfaces are visible. However, publishing the activity signal still called `has_active_animated_surfaces()`, which scanned the full surface map after redraws. Maintaining the count makes that gate O(1), removing another repeated graphics-mode scan while preserving behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_69c830"` — `tj-36cc3ba5`, passed

Earlier validation `tj-fc942671` caught a test-only `NativeAnimation` field mismatch (`delay_ms` / `loop_count` vs `frame_delay_ms`), fixed before the passing retry.
