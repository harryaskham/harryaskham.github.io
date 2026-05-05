# bd-8fd8a9: do not let graphics-frame counters satisfy TUI benchmark work evidence

## What changed

- Removed `graphics_frames > 0` from the `graphics_work_observed` predicate.
- Updated `scripts/tui-fps-bench.sh` and `scripts/tui-fps-compare.sh` legacy fallbacks so `frames_with_graphics` alone does not satisfy graphics work evidence.
- Preserved the corrected behavior from bd-e84287: uploads, upload failures, retained redisplays, and renderer-cache hit/miss telemetry still count as observed graphics activity.
- Updated SPEC/README/AGENTS/docs wording to clarify that delete-only cleanup and graphics-frame counters by themselves are not enough proof.
- Added regression coverage for a graphics-capable cleanup pass that increments `frames_with_graphics` but performs no upload/retained/cache work.

## Why

`benchmark_upload_pending()` can record a graphics frame for a graphics-capable cleanup pass. If `graphics_work_observed` accepts that counter by itself, a stale-cleanup/delete-only run can still pass `--require-graphics` and make graphics/text benchmark comparisons look valid when they are not. This keeps the benchmark evidence tied to actual upload, retained display, or renderer-cache graphics work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `bash -n scripts/tui-fps-compare.sh`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-8fd8a9.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_8fd8a9"` — `tj-693b42cb`, passed
