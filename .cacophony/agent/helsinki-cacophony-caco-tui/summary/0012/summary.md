# bd-05e58e: force detailed graphics telemetry in real TUI benchmarks

## What changed

- Added `force_benchmark_detailed_graphics_perf()` in the real-dashboard benchmark path.
- Real TUI benchmark construction now forces `graphics.detailed_perf_telemetry = true` for diagnostic benchmark runs.
- Live TUI behavior is unchanged: explicit `detailedPerfTelemetry: false` remains honored outside benchmark mode.
- Updated `SPEC.md` and `docs/tui.html` to state that benchmark modes force cache/traffic telemetry on, while live TUI can still disable it.

## Why

The default enterprise theme imports `perf.yaml`, which sets `detailed_perf_telemetry: false`. That is useful for live low-overhead operation, but it makes benchmark JSON report zero cache hit rates even when caches are active. For graphics-vs-text parity audits, benchmark evidence must include cache/traffic counters by default.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_05e58e"` — `tj-9c6e3e81`, passed
