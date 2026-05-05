# bd-995c20: report terminal-inclusive FPS in graphics benchmarks

## What changed

- Real TUI benchmark JSON now includes:
  - `terminal_inclusive_work_secs`
  - `terminal_inclusive_work_fps`
  - `avg_terminal_inclusive_work_frame_ms`
- These include terminal-sync barrier time when `--terminal-sync` is enabled and fall back to app-side work metrics when sync is disabled.
- `scripts/tui-fps-compare.sh` now uses terminal-inclusive work FPS/frame-ms for text-vs-graphics ratios when both runs have terminal sync enabled.
- Compare summary JSON keeps `raw_work_fps` and `terminal_inclusive_work_fps` for both text and graphics runs and marks ratios with `terminal_inclusive: true` when applicable.
- Added regression coverage for terminal-inclusive metric calculation.

## Why

Before this slice, `--terminal-sync` measured and reported terminal barrier time separately, but `work_fps` still covered only app-side frame work. A graphics run could therefore look at parity when app-side rendering was fast even if Kitty/Ghostty command processing was slow. Terminal-inclusive metrics make synced benchmark comparisons reflect terminal-side processing cost directly.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `bash -n scripts/tui-fps-compare.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_995c20"` — `tj-2a78262e`, passed
