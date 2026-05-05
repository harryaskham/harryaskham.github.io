# bd-9e5334: avoid duplicate native background rendering and report frame stats

## What changed

- Moved `render_graphics_background()` native-background detection ahead of the static/current-frame render path.
- Native Kitty background animation now pre-renders only the native frame set it actually registers instead of first rendering and discarding an unused current-frame surface.
- Native frame pre-rendering now aggregates per-frame background cache stats and records raster/encode duration after the frame loop, so benchmark telemetry reflects real native pre-render work.
- Per-native-surface stats are returned with the `NativeAnimation` asset, so the existing `flush_graphics_requests()` telemetry path records accurate background cache hits/misses for native animations.
- Added regression coverage for a 3-frame native background: it returns a native asset, reports 3 cache misses for the generated frames, and records one raster/encode call.

## Why

The previous flow made graphics benchmarks both slower and less truthful for native animated backgrounds: an unused static render was paid before native frame generation, while the actual native frame-loop work was not included in the raster/cache stats. This trims avoidable work and makes benchmark evidence harder to fool.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9e5334"` — `tj-eacae358`, passed
