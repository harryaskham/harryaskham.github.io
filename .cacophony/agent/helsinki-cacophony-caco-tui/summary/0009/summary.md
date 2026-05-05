# bd-f6d9e4: require observed graphics work in FPS benchmark

## What changed

- Added `graphics_work_observed` to real-dashboard benchmark JSON.
- The field is true only when the measured window observes actual kitty work: graphics upload-pass frames, successful/failed uploads, deletes, or retained redisplays.
- `scripts/tui-fps-bench.sh --require-graphics` now requires all of:
  - terminal capability is not `None`
  - `graphics_effective_enabled=true`
  - `graphics_work_observed=true`
- The benchmark summary line now prints graphics capability, effective enablement, and observed work.
- Updated `SPEC.md`, `README.md`, and `docs/tui.html` so future graphics evidence cannot be satisfied by a Kitty terminal that emitted no measured graphics work.

## Why

For graphics-vs-text parity work, a run that merely detects Kitty but does not upload/delete/display any kitty surfaces is not valid graphics evidence. This closes another benchmark truthfulness gap.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-268c81a3`, passed
