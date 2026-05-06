# bd-4c147c: skip inactive background retain on empty cache

## What changed

- Added `BackgroundRenderer::retain_surfaces_empty()` for the app to detect whether there is any active per-surface/shared background retain state that needs per-frame retain/GC bookkeeping.
- `retain_surfaces()` now returns immediately when both the active set and retained state are empty.
- `App::flush_graphics_requests()` skips calling `background_renderer.retain_surfaces(...)` when there are no active background surfaces and no retain state.
- Added regression coverage for the app guard and renderer empty-state helper.

## Why

`flush_graphics_requests()` runs on graphics-capable frames even when the current view emits no background surfaces. The old code still entered `BackgroundRenderer::retain_surfaces()`, which performed retain/GC checks. The new guard avoids that empty-frame overhead while still preserving inactive promotion caches and running retain/GC when active or retained state exists.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_4c147c"` — `tj-25cd9df8`, failed because the test incorrectly expected inactive promotion caches to require retain/GC.
- Corrected the assertion to distinguish active retain state from inactive promotion caches.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_4c147c"` — `tj-84a2d508`, passed.
