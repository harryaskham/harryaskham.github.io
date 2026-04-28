# Session summary — Scaled background cache buckets

## Goal

Implement the operator-requested TUI performance optimization for scalable bitmap backgrounds: allow procedural backgrounds to render into a small set of aspect-ratio cache buckets, then rely on terminal graphics scaling instead of producing a distinct PNG for every exact panel width/height.

## Bead(s)

- `bd-fdc050` — Quantize scaled background graphics cache sizes

## Before state

- Failing tests: none; this was a performance/cache-efficiency request.
- Relevant metrics: not benchmarked in this slice.
- Context: `BackgroundRenderer` cache keys included exact `Rect` geometry for all procedural backgrounds, so near-identical panel sizes missed the shared PNG cache even when the visual asset could be scaled.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: focused tests demonstrate bucketed gradient sizes share one cached asset when enabled, exact sizing remains distinct when disabled, and image backgrounds keep exact geometry.
- Context: `GraphicsConfig` now has `scaled_background_cache` and `scaled_background_cache_max_px` settings. `App` propagates those settings into `BackgroundRenderer`, which quantizes non-image scalable background cache keys to a small fixed aspect-ratio ladder.

## Diff summary

- Commits: `400d70ab0`
- Files touched: `crates/caco-config/src/model.rs`, `crates/caco-tui/src/background_renderer.rs`, `crates/caco-tui/src/app.rs`
- Tests: focused background renderer and config compile checks passed
- Behavioural delta: default behavior is unchanged because `scaled_background_cache` defaults to `false`; enabling it trades exact per-geometry procedural PNGs for reusable scaled cache buckets.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui background_renderer --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-config scaled_background --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-config graphics_config --lib`

## Operator-takeaway

Scaled background caching is now a config-gated TUI performance path: gradients/glows/scanlines can reuse bucketed high-resolution PNGs across nearby panel sizes, while image backgrounds remain exact for positioning/cropping correctness.
