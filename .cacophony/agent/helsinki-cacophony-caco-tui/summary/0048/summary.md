# bd-8e74f3: report cached TUI graphics frames in scene benchmark telemetry

## What changed

- Real-dashboard benchmark frames now capture per-frame renderer-cache deltas from `GraphicsPerfTracker`.
- Scene summaries expose background, border, and decoration cache hit/miss counters.
- `frames_with_graphics` now counts frames with renderer-cache fast-path work as well as upload/retained/failure work.
- Delete-only cleanup remains excluded from `frames_with_graphics`.
- Top-level `frames_with_graphics` uses scene-local telemetry when available, with the legacy aggregate counter as fallback.
- Updated SPEC/README/docs to describe cache-aware scene graphics telemetry.

## Why

Once graphics caching succeeds, many frames should do no Kitty upload/delete work: they only mark cached background, border, and decoration surfaces live. The prior scene telemetry could report zero graphics frames in that optimized steady state, which makes benchmarks lie in the opposite direction by hiding that the graphics path was active. This keeps benchmark evidence truthful for both slow upload-heavy runs and fast cached runs.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-8e74f3.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_8e74f3"`
  - `tj-42608e4b` failed due to a test initializer missing newly added fields.
  - `tj-6183f0d8` passed after adding `..Default::default()`.
