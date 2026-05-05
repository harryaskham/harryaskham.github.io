# bd-3a586e: fail TUI FPS compare invalid text baseline before graphics leg

## What changed

- Extracted `validate_text_baseline()` in `scripts/tui-fps-compare.sh`.
- The text baseline is now validated immediately after the text benchmark run and before launching the expensive graphics benchmark leg.
- The same effective-graphics / observed-graphics guard remains in place; this only moves it earlier and makes the failure cause clearer.

## Why

bd-7107fd made text-vs-graphics comparisons reject non-text text baselines, but it did so after the graphics leg had already run. If the text baseline is invalid, the comparison cannot be trusted, so running the Kitty leg only wastes time and can obscure the root cause. This fails fast.

## Validation

- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-compare.sh --help`
- Extracted and sourced `validate_text_baseline()`, then verified synthetic JSON with `graphics_effective_enabled=true` exits non-zero and prints `text baseline was not text-only`.
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-3a586e.log`)
- `git diff --check`
