# bd-f5f605: deduplicate identical TUI graphics border requests before flush

## What changed

- Added `retain_unique_graphics_requests()` for per-frame graphics request vectors.
- `flush_graphics_requests()` now drops exact duplicate `GraphicsBorderRequest`s after same-key/different-rect disambiguation and before border/background rendering work begins.
- Added regression coverage that preserves first-seen order while removing an exact duplicate request.

## Why

The existing disambiguation path protects correctness when one logical key appears at different rects. It did not avoid exact duplicate border requests. Processing exact duplicates cannot change output, but it can add cached background/border/decoration lookups, live-mark calls, and benchmark noise. This trims redundant steady-state graphics flush work while preserving draw/dedraw semantics.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f5f605"` — `tj-31aa13f3`, passed
