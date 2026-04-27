# Session summary — No-TTY TUI benchmark diagnostics

## Goal

Continue the TUI improvement loop by improving the real TUI benchmark's operator-facing error when it is launched from a non-interactive shell without a controlling terminal.

## Bead(s)

- `bd-3283ff` — TUI benchmark should explain no-TTY Device not configured errors

## Before state

- Failing tests: none existing for this diagnostic path.
- Relevant metrics: running `target/debug/caco tui benchmark --duration 1 --warmup 0 --debug` from the non-interactive agent shell failed with only `real tui benchmark error: Device not configured (os error 6)`.
- Context: the same benchmark works from a tmux PTY, but the raw ENXIO message did not tell agents/operators that a PTY is required or that `--output` is the clean way to capture JSON without terminal escape traffic.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: the no-TTY smoke now reports: `real tui benchmark error: Device not configured (os error 6); the real TUI benchmark needs a controlling terminal/PTY. Run it inside tmux/tmux-cli or another real terminal, and use --output <path> if you need clean JSON separate from terminal escape traffic.`
- Context: both `caco tui benchmark` and `caco tui fps-benchmark --real` now share the clearer error mapping for macOS ENXIO / Device-not-configured failures.

## Diff summary

- Commits: `51b505044`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: no successful benchmark behaviour changes; only the failure message for no-TTY real benchmark launches is more actionable.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-cli real_tui_benchmark_error_explains_missing_tty --lib`
  - `CARGO_BUILD_JOBS=2 cargo build -j2 -p caco`
  - `target/debug/caco tui benchmark --duration 1 --warmup 0 --debug` from the non-interactive shell now emits the improved diagnostic.

## Operator-takeaway

If a future worker runs the real TUI benchmark from the wrong context, the CLI now tells them exactly to switch into a PTY/tmux and how to capture clean JSON, instead of leaving them with an opaque macOS device error.
