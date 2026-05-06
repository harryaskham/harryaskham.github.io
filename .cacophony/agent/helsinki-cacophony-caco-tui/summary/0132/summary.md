# bd-cbe9c7: skip empty background request cloning

## What changed

- Moved `effective_background_request_for_layers()` behind the background renderability / existing-cache guard in `App::flush_graphics_requests()`.
- Frames whose resolved background layers are all `None` and whose app background cache is empty now skip cloning `GraphicsBorderRequest` and scanning layers for visible background animation.
- Renderable-background frames and existing-cache cleanup frames still compute the effective background request and preserve cache/render/removal behavior.
- Added source-shape coverage to keep the clone/animation-scan work behind the guard.

## Why

Graphics border requests are frequent even for panels without background graphics. The prior path cloned the full border request and recomputed background animation state before proving there was no background work. This trims another per-panel empty-background hot-path cost while retaining correctness for background-capable and stale-cache cases.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_cbe9c7"` — `tj-5689a41d`, passed
