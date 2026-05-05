# bd-ea5678: warn when TUI graphics benchmarks omit terminal sync

## What changed

- Added `benchmark_warnings` to real TUI benchmark JSON output.
- Benchmark results now warn when:
  - `avg_fps` is paced/target-capped, so it is not valid graphics/text parity evidence by itself.
  - effective graphics work was observed without `--terminal-sync`, so Kitty/Ghostty terminal-side command processing may be outside the measured `work_fps`.
- `scripts/tui-fps-bench.sh` now prints any JSON `benchmark_warnings` alongside its existing truth-check messages.
- Added a regression test covering the graphics-without-terminal-sync warning.

## Why

Operators reported cases where Kitty graphics felt dramatically slower than text/ASCII mode while the benchmark summary showed parity. One way that can happen is when the benchmark measures app/backend work but does not wait for the terminal to process graphics commands. The warning makes that caveat explicit in both direct JSON output and the wrapper's human-readable output, steering parity claims toward `--terminal-sync` and text-vs-graphics compare runs.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `bash -n scripts/tui-fps-bench.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_ea5678"` — `tj-596b3ddd`, passed
