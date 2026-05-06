# bd-eea89b: reuse background layer scan for animation

## What changed

- Added `background_layers_renderable_and_animated()` to compute both background renderability and style-driven background animation from one layer pass.
- `App::flush_graphics_requests()` now uses that single pass and passes the precomputed animation flag into `effective_background_request_with_animation()`.
- Existing `effective_background_request_for_layers()` remains available for other call sites, but internally reuses the same helper.
- Added source-shape coverage ensuring the flush path does not re-scan layers for animation after checking renderability.

## Why

The graphics flush path was scanning resolved background layers once to determine whether any background could render and then scanning the same layers again when building the effective background request to decide whether background animation was style-driven. On graphics frames with many panels, this duplicate layer iteration adds avoidable hot-path overhead. The new helper keeps the same animation gating semantics with one scan.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_eea89b"` — `tj-2e1fdd58`, passed
