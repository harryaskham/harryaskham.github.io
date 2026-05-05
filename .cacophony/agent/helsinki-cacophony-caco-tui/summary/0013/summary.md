# bd-42b74f: terminal-synchronized TUI graphics benchmark mode

## What changed

- Added `RealTuiBenchmarkConfig::terminal_sync` and CLI `--terminal-sync` for `caco tui dashboard-benchmark` / `benchmark` and `caco tui fps-benchmark --real`.
- Real-dashboard benchmark now optionally performs a terminal status-response barrier after each frame's ratatui draw and kitty upload pass.
- Benchmark JSON reports:
  - `terminal_sync_enabled`
  - `terminal_syncs`
  - `avg_terminal_sync_ms`
  - `p95_terminal_sync_ms`
- `scripts/tui-fps-bench.sh` forwards `--terminal-sync`, annotates harness metadata, and prints terminal-sync metrics.
- `scripts/tui-fps-compare.sh` enables terminal sync by default for same-terminal text-vs-graphics comparison evidence, with `--no-terminal-sync` as the legacy pty-only escape hatch.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, and `docs/tui.html`.

## Why

The operator observed that kitty graphics can feel much slower than text while benchmark output reports parity. One source of benchmark optimism is that the benchmark frame timer stopped after Cacophony wrote to the pty; the terminal emulator could still be parsing/compositing queued graphics commands. Terminal sync gives us a benchmark/audit mode that waits for terminal-side command processing before ending the frame.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs crates/caco-cli/src/lib.rs`
- `bash -n scripts/tui-fps-bench.sh`
- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-bench.sh --help`
- `scripts/tui-fps-compare.sh --help`
- `git diff --check`
- `docs/validate-pages.sh`
- `caco test run --wait --command "cargo test -p caco-tui bd_42b74f"` — `tj-41243f92`, passed
- `caco test run --wait --command "cargo test -p caco-cli tui_fps_benchmark_command_spec_exists"` — `tj-6a367114`, passed
- `caco test run --wait --command "cargo test -p caco-cli tui_benchmark_command_spec_exists"` — `tj-a93813b6`, passed

One earlier caco-cli validation attempt (`tj-e6b8c17a`) used invalid cargo syntax for two test filters and failed before running tests; it was retried as the two passing queued jobs above.
