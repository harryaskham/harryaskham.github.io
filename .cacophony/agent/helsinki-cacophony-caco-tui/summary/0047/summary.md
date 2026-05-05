# bd-67632d: do not count delete-only frames as TUI benchmark graphics frames

## What changed

- Real-dashboard benchmark upload telemetry now sets per-frame `graphics_frame` only after upload, upload-failure, or retained-redisplay work is known.
- Delete-only cleanup still records delete counters, but no longer increments `frames_with_graphics` or `GraphicsCounters::graphics_frames`.
- Updated SPEC/README/docs wording to state that scene-local graphics-frame counts exclude delete-only cleanup.
- Added regression coverage for a graphics-capable delete-only cleanup pass.

## Why

A cleanup-only frame can run through the graphics-capable upload pass and send Kitty delete commands without drawing/re-displaying a graphics placement. Counting that as `frames_with_graphics` made scene telemetry look like a real graphics workload even when the benchmark was only cleaning stale placements. This keeps benchmark evidence aligned with actual graphics placement/upload work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-67632d.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_67632d"` — `tj-35e828d2`, passed
