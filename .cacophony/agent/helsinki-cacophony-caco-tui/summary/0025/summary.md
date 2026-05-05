# bd-2652c5: report multi-layer background cache hits in benchmark telemetry

## What changed

- Added `BackgroundRenderer::render_composite_cached_with_stats_ref()`.
- Added `render_composite_cached_split_with_stats_ref()` so split/grouped multi-layer backgrounds return per-surface cache stats with the PNG `Arc`.
- Updated `App::render_graphics_background()` to aggregate and record those stats for composite/multi-layer backgrounds instead of defaulting to zero stats and always recording raster time for `layers.len() > 1`.
- Added regression coverage that composite render-with-stats reports a miss followed by a hit while reusing the cached PNG `Arc`.

## Why

Single-layer background telemetry was fixed in bd-40f387, but multi-layer/composite backgrounds still under-reported cache hits and over-attributed raster work. Returning stats from the actual composite render lookup keeps benchmark evidence honest for richer themes without changing rendered output.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2652c5"` — `tj-5187fc47`, passed

Earlier broad filter `tj-3bed3904` compiled successfully but matched zero tests; a bead-specific regression test was then added and passed.
