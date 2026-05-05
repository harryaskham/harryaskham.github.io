# bd-40f387: avoid duplicate background cache lookups in TUI graphics flush

## What changed

- Added `BackgroundRenderer::render_cached_with_stats_ref()` for single-layer backgrounds.
- Updated `App::render_graphics_background()` to use that combined render+stats path for single-layer backgrounds.
- The app now avoids calling `stats_for_cached()` and then `render_composite_cached_split()` for the same single-layer background, eliminating duplicate cache key construction/hash lookup in the common path.
- Added regression coverage that the new render-with-stats path reports miss/hit telemetry while reusing the cached PNG `Arc`.

## Why

Graphics benchmark telemetry needs cache hit/miss counters, but collecting them by probing before rendering made steady cached graphics frames do extra work. Returning the PNG and stats from one lookup preserves telemetry and output while trimming CPU overhead in the background fill path.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_40f387"` — `tj-9ea17b51`, passed
