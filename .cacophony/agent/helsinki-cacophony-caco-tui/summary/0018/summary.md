# bd-ce3a25: use cached animation activity signal for TUI animation frame gate

## What changed

- `App::AnimationFrame` now reads the cached `animation_activity_tx` watch value when available instead of scanning `SurfaceManager` for active animated surfaces every animation tick.
- It falls back to `surfaces.has_active_animated_surfaces()` in pre-run/test contexts where no activity signal has been installed.
- Added regression coverage that cached `active=true` gates animation frames without requiring a surface scan, while preserving the existing visual phase gate.

## Why

After bd-c65cfa the live TUI already maintains an activity signal for whether any visible kitty surface is animated. Re-scanning the surface map on every animation tick repeated that work. Using the cached signal makes the per-tick gate O(1), shaving more graphics-mode overhead while keeping the same rendered behavior and phase quantization.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_ce3a25"` — `tj-d15fcddc`, passed

Earlier validation attempts documented useful test corrections:
- `tj-ff67c4ca`: failed because the initial test forgot that the visual phase gate can legitimately suppress an active animation tick.
- `tj-c51c9b43`: failed because the test tried to access a private `BorderIntegration` field; fixed by using public animation speed/frame setters and waiting for a phase boundary.
