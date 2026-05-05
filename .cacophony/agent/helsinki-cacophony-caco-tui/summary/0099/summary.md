# bd-daa054: warn when terminal-inclusive TUI FPS lags app work FPS

## What changed

- `scripts/tui-fps-bench.sh` truth checks now parse `terminal_inclusive_work_fps` and `terminal_sync_enabled` from benchmark JSON.
- When terminal sync is enabled and terminal-inclusive FPS is materially below app-side `work_fps`, the wrapper prints an explicit truth-check warning that terminal-side Kitty/Ghostty processing is part of the graphics cost.
- Existing paced-FPS and app-side work-FPS warnings remain unchanged.

## Why

Single-run graphics benchmarks now print terminal-inclusive FPS, but operators could still focus on app-side `Work FPS` and miss terminal-side processing cost. This warning makes the gap explicit whenever terminal sync evidence shows the terminal-inclusive path is materially slower.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check`
- Lightweight source assertion verified parsing of `terminal_inclusive_work_fps`, `terminal_sync_enabled`, and the new warning text.
