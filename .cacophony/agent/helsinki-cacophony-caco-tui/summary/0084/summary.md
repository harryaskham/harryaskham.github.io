# bd-0ba745: warn when text-vs-graphics compare disables terminal sync

## What changed

- `scripts/tui-fps-compare.sh` now propagates per-run `benchmark_warnings` from the text and graphics benchmark JSON into the comparison summary JSON.
- The comparison summary adds a compare-level warning when graphics work is observed while `terminal_sync_enabled` is false.
- Human-readable compare output prints all benchmark warnings as `Benchmark warning: ...` lines.

## Why

The compare wrapper defaults to terminal synchronization, but `--no-terminal-sync` can still be used. Without a terminal-side barrier, a graphics run can report apparently good work-FPS parity while Kitty/Ghostty is still processing graphics commands outside the measured frame work. The compare output now makes that caveat explicit so operator parity evidence remains honest.

## Validation

- `bash -n scripts/tui-fps-compare.sh`
- `git diff --check`
- Lightweight source assertion verified the script contains the new compare warning, propagated `benchmark_warnings`, and printed `Benchmark warning:` output.
