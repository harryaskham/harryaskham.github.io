# bd-a9341d: same-terminal text vs graphics FPS comparison wrapper

## What changed

- Added `scripts/tui-fps-compare.sh`.
- The wrapper runs the real-dashboard benchmark twice with the same selected terminal harness:
  - text baseline with `tui.graphics.enabled: false`
  - graphics run with `--require-graphics --force-gfx`
- Both legs use `--uncapped` and `--no-build` so the output compares render/upload work headroom for the same binary rather than target-FPS sleep budget.
- It writes `text.json`, `graphics.json`, and `summary.json` under a chosen output directory.
- `summary.json` includes text/graphics work FPS, avg work frame time, graphics upload/delete/wire counts, and graphics/text work-FPS ratio.
- Added configurable threshold support:
  - `--threshold <ratio>` defaults to `0.90`
  - `--fail-below-threshold` exits non-zero below threshold for CI/gating use
- Forwarded provenance guard support through `--require-current-binary`.
- Updated `SPEC.md`, `README.md`, and `docs/tui.html`.

## Why

The operator concern is specifically text-vs-graphics parity. Individual benchmark runs can be misleading if they use different terminals, stale binaries, target-FPS pacing, or no observed graphics work. This wrapper packages the comparison as a single first-party workflow and reports the ratio directly.

## Validation

High-load bounded validation only:

- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-compare.sh --help`
- `git diff --check`
- `docs/validate-pages.sh`

A full two-leg Xvfb/Kitty comparison is intentionally left to operator/performance runs when host load is lower.
