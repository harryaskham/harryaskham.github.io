# bd-7107fd: reject TUI FPS compare runs when text baseline uses graphics

## What changed

- `scripts/tui-fps-compare.sh` now inspects the text baseline JSON after both benchmark legs run.
- It exits with an explicit error if the text baseline reports `graphics_effective_enabled=true` or observed graphics work (including fallback counters for older JSON).
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, and `docs/tui.html` to document that the comparison wrapper rejects non-text baselines.

## Why

The graphics leg already requires observed graphics work. The text leg did not symmetrically prove it was text-only, so config override drift, force flags, or harness issues could make both legs graphics-enabled and produce a falsely healthy graphics/text ratio. This keeps parity evidence from lying in the opposite direction.

## Validation

- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-compare.sh --help`
- Synthetic `jq` guard checks:
  - `graphics_effective_enabled=true` is detected for the text leg.
  - legacy fallback counters such as `uploads_succeeded=1` are detected as observed text-leg graphics work.
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-7107fd.log`)
- `git diff --check`
