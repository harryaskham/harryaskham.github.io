# bd-013923: default graphics FPS bench to terminal sync

## What changed

- `scripts/tui-fps-bench.sh --graphics` / `--require-graphics` now enables `--terminal-sync` by default unless the caller explicitly chooses otherwise.
- Added `--no-terminal-sync` as the explicit escape hatch for app-side-only / pty-write-only runs.
- The startup summary prints when terminal sync was enabled by the graphics default.
- Harness JSON now records `harness.terminal_sync_defaulted` so benchmark artifacts disclose whether terminal-side synchronization was defaulted by the wrapper.

## Why

Single-run graphics benchmarks could still look near text parity because terminal synchronization was opt-in. That measured the app's render/write loop but could omit a large amount of Kitty/Ghostty command processing. Since the operator-facing graphics evidence path is `--graphics` / `--require-graphics`, those runs should include terminal-side command processing by default. Callers can still disable it deliberately with `--no-terminal-sync`, and the benchmark warnings continue to flag unsynchronized graphics work.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `scripts/tui-fps-bench.sh --help` includes `--no-terminal-sync` and the graphics-default note.
- Lightweight source assertions for the defaulting logic and `terminal_sync_defaulted` harness metadata.
- `git diff --check`
