# bd-f832f2: skip background style resolution when backgrounds disabled

## What changed

- Added a cheap `background_request_may_render_without_layer_resolution()` preflight for the graphics background path.
- The preflight inspects top-level background style, background image overrides, and raw role/instance layer overrides through `layers_for_role_raw()` before materializing resolved background layer `Vec`s.
- `flush_graphics_requests()` now calls this preflight before `resolved_background_layers()`, so all-`None` background roles with no prior app background cache skip layer resolution entirely.
- Existing cache cleanup still forces resolution/lookup when app background cache roots exist, preserving de-draw/removal correctness after config or view changes.
- Added source-shape coverage to keep the preflight before resolved layer allocation.

## Why

Border graphics requests are emitted for many panels even when the theme has no bitmap background for that role. The previous path still cloned/resolved role layers to later prove there was no background work. This skips that allocation/resolution for the common no-background/no-cache case while preserving false-positive safety for any override that may render.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f832f2"` — `tj-190760ea`, passed
