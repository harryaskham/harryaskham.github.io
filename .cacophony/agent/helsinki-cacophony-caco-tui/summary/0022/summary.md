# bd-416b33: count app-level background fast-path hits in benchmark telemetry

## What changed

- `App::flush_graphics_requests()` now records background cache hits when `cached_background_surface_keys()` reuses already-live background surfaces.
- Added regression coverage that a clean app-level background fast-path reuse increments `background_cache_hit_count`.

## Why

Real-dashboard benchmark JSON derives `background_cache_hit_rate` from graphics perf counters. Renderer-level background cache hits were counted, but the app-level dirty-tracking fast path bypasses `render_graphics_background()` entirely and only marks cached surfaces live. That made benchmark cache telemetry under-report successful background cache reuse even though the fast path was working. Counting those hits makes benchmark evidence less misleading while preserving rendering behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_416b33"` — `tj-e8078183`, passed

Earlier validation:
- `tj-a49acca1`: compile failure because the test used a non-existent graphics request helper; fixed by using `record_graphics_panel`.
- `tj-1424bf9b`: test setup missed the graphics-active override, so no request was recorded; fixed before passing retry.
