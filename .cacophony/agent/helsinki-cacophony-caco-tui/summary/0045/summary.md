# bd-e84287: count cached TUI graphics fast paths as observed benchmark work

## What changed

- `graphics_work_observed` now includes background, border, and decoration renderer cache hits/misses.
- Delete-only cleanup remains insufficient graphics evidence (bd-2e82e1 behavior preserved).
- Updated SPEC/README/AGENTS/docs wording to clarify that renderer-cache fast-path reuse counts as graphics activity.
- Added regression coverage for fully cached graphics fast-path counters.

## Why

After the graphics pipeline is warm, the optimal state is often zero uploads/deletes: cached background/border/decoration fast paths mark existing graphics surfaces live and avoid Kitty churn. The benchmark should still recognize that as real graphics activity, otherwise `--require-graphics` can fail precisely when caching succeeds.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `bash -n scripts/tui-fps-compare.sh`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-e84287.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e84287"` — `tj-5b84e8d6`, passed
