# bd-c0bf9c: per-scene TUI graphics I/O telemetry

## What changed

- Real-dashboard benchmark debug scene summaries now include scene-local graphics I/O counters:
  - `frames_with_graphics`
  - `uploads_succeeded`
  - `uploads_failed`
  - `deletes_sent`
  - `delete_failures`
  - `upload_bytes`
  - `upload_wire_bytes`
  - `retained_redisplays`
- Benchmark frame upload handling returns per-frame I/O stats, which are accumulated into the active scene during the measurement window.
- `SPEC.md` and `docs/tui.html` now require/describe scene-local graphics I/O metrics alongside scene-local work FPS.

## Why

The benchmark now has honest uncapped/headroom and scene-local FPS, but without scene-local upload/delete/wire counts it is still difficult to identify which view or transition causes Kitty overhead. These counters make graphics/text comparisons actionable by pointing at the scene where PNG upload/delete churn occurs.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_ed3b98"` — `tj-251ad8b5`, passed
- `caco test run --wait --command "cargo test -p caco-tui real_benchmark_result_reports_"` — `tj-9af91f92`, passed
