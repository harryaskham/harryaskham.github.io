# bd-bfc2f0: show terminal-inclusive FPS in TUI benchmark output

## What changed

- `scripts/tui-fps-bench.sh` now prints a `Terminal+Work FPS` line when benchmark JSON includes terminal-inclusive metrics.
- The new line includes both terminal-inclusive FPS and average terminal+work frame time.
- Existing app-side `Work FPS` and separate terminal sync timing remain visible.

## Why

`bd-995c20` added terminal-inclusive benchmark metrics to JSON and compare summaries. Single-run benchmark output still emphasized app-side `Work FPS`, so an operator could miss terminal-side Kitty/Ghostty cost unless they inspected JSON or ran compare. Printing terminal-inclusive FPS directly makes the common benchmark output harder to misread.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check`
- Lightweight source assertion verified `Terminal+Work FPS`, `terminal_inclusive_work_fps`, and `avg_terminal_inclusive_work_frame_ms` are present in the wrapper.
