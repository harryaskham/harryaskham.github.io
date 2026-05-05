# bd-e1f72b: reuse PendingGraphicsWork to skip upload-collection scans

## What changed

- Live Kitty upload pass now uses the already-computed `PendingGraphicsWork` summary to avoid work collection scans when the summary says there is no work of that kind:
  - `pending_fetches()` only runs when `pending_graphics_work.fetches` is true.
  - `pending_native_animation_uploads()` only runs when `pending_graphics_work.native_animation_uploads` is true.
  - `pending_uploads()` only runs when `pending_graphics_work.regular_uploads` is true; this now applies on animation redraws too.
- Benchmark Kitty upload path mirrors the same gating so benchmark costs stay aligned with live behavior.
- Backoff ticking is preserved when a pass runs for mixed fetch/native/delete work but no regular uploads.
- Added regression assertions for live and benchmark paths.

## Why

`PendingGraphicsWork` is the single classification pass for the post-render graphics upload path. Before this slice, live/benchmark upload paths still performed additional full collection scans on frames where the summary had already proven there was no fetch/native/regular upload work. Animation redraws were especially wasteful because they could force native/regular collection scans even when the summary said no such work existed.

This removes avoidable scan churn from cached/steady graphics frames, keeping the graphics path closer to ASCII/text cost when no terminal upload work is actually pending.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e1f72b"`
  - initial run `tj-76dec9f8` failed because source assertions matched their own strings
  - fixed tests to scope assertions to upload-path bodies
  - rerun `tj-b1b607b7` passed
