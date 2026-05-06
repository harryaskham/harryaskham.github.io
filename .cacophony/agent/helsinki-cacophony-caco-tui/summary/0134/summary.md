# bd-10c66b: short-circuit background layer scan

## What changed

- `background_layers_renderable_and_animated()` now stops scanning resolved background layers once it finds a renderable layer with `background_animate=true`.
- The helper still requires a renderable animated layer before enabling style-driven animation, so `background_style: None` layers with animation flags do not incorrectly animate.
- Added source-shape coverage for the short-circuit and renderable-animation condition.

## Why

The graphics flush path uses this helper for every graphics border request. Multi-layer backgrounds do not need to continue scanning after both output booleans are proven (`renderable=true`, `style_driven_animation=true`). Short-circuiting trims layer iteration for animated multi-layer roles while preserving the need to scan all layers in false-animation/static cases.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_10c66b"` — `tj-343e6543`, passed
