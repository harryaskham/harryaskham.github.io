# bd-19852e: warn when terminal-inclusive compare ratio is worse than raw

## What changed

- `scripts/tui-fps-compare.sh` now computes and records `ratios.graphics_to_text_raw_work_fps` alongside the existing selected comparison ratio.
- When terminal sync is enabled, the printed `Graphics/Text` line includes both terminal-inclusive and raw app-work ratios.
- The compare summary adds a benchmark warning when the terminal-inclusive graphics/text ratio is materially below the raw app-work ratio.

## Why

Terminal-inclusive ratios are the right default when terminal sync is enabled, but operators still need to know whether the gap came from terminal-side Kitty/Ghostty command processing rather than app-side rendering. The raw ratio plus warning make that distinction explicit in both JSON and text output.

## Validation

- `bash -n scripts/tui-fps-compare.sh`
- `git diff --check`
- Lightweight source assertion verified `graphics_to_text_raw_work_fps`, the terminal-inclusive-vs-raw warning text, and printed `raw app-work` output.
