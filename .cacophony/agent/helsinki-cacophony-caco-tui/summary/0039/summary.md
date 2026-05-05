# bd-2e82e1: do not let delete-only TUI graphics benchmarks satisfy observed work

## What changed

- `graphics_work_observed` in real TUI benchmark results no longer counts `delete_count` by itself.
- Script fallback predicates in `scripts/tui-fps-bench.sh` and `scripts/tui-fps-compare.sh` now require graphics frames, uploads, upload failures, or retained redisplays; `deletes_sent` remains reported separately but does not prove graphics placement/upload work.
- Updated SPEC/README/AGENTS/docs wording to clarify that delete-only cleanup is insufficient evidence for graphics work.
- Added regression coverage for delete-only counters.

## Why

A stale cleanup/delete-only benchmark could previously satisfy `--require-graphics` even if no graphics surface was uploaded, redisplayed, or rendered during the measurement. That made benchmark evidence easier to misread. Delete commands are useful telemetry, but they should not prove active graphics placement/upload behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-bench.sh --help`
- `scripts/tui-fps-compare.sh --help`
- Synthetic jq predicate check: JSON with only `deletes_sent=5` returns observed work `false`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-2e82e1.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2e82e1"` — `tj-b021755a`, passed
