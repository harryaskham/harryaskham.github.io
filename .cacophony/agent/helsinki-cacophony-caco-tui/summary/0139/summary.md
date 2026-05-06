# bd-adbe75: use small cache for background preflight

## What changed

- Replaced the per-frame background preflight `HashMap` in `App::flush_graphics_requests()` with a small `Vec` cache keyed by `(PanelRole, instance)`.
- Repeated role/instance requests still reuse `background_request_may_render_without_layer_resolution()` results within the frame.
- Per-request background image overrides still bypass the cache and force background handling.
- Added source-shape coverage to ensure the cache stays a small Vec and does not reintroduce HashMap allocation on first use.

## Why

Most frames have a low number of distinct graphics role/instance pairs. A HashMap adds allocation and hashing overhead to avoid repeated raw-config scans. A small Vec keeps the common low-cardinality case cheaper while preserving the repeated-result reuse that prevents rewalking raw config for every matching panel.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_adbe75"` — `tj-deb9f118`, passed
