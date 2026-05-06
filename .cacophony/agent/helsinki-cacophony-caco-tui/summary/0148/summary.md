# bd-13f180: hoist graphics config presence in flush

## What changed

- `App::flush_graphics_requests()` now reads `self.graphics_config.is_some()` once into `graphics_config_present` before the per-border-request loop.
- The per-request background preflight branch uses that local boolean instead of repeatedly inspecting the `Option` field.
- Background cache/root pruning behavior remains unchanged; this only removes repeated field/Option checks in the hot loop.
- Added source-shape coverage ensuring the check stays hoisted.

## Why

Graphics frames can carry many border requests. Even small repeated branches add up while trying to bring graphics-mode overhead closer to ASCII/text mode. Hoisting the graphics-config presence check keeps the per-request path simpler without changing rendering behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_13f180"` — `tj-85d51b94`, passed
