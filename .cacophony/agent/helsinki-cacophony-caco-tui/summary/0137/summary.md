# bd-b9fd66: cache background preflight per role instance

## What changed

- Added a lazy per-frame `background_preflight_cache` in `App::flush_graphics_requests()` keyed by `(PanelRole, instance)`.
- Requests without a per-request background image override reuse `background_request_may_render_without_layer_resolution()` results across repeated role/instance panels in the same frame.
- Requests with a profile/background image override bypass the cache and force background handling because that field is request-specific.
- Added source-shape coverage for the cache key, lazy allocation, and override bypass.

## Why

The raw-config background preflight is cheaper than resolving full background layers, but it can still walk raw role layers for every border request. Many frames render multiple panels with the same role/instance. Caching this boolean avoids repeated raw-config scans while preserving correctness for override-backed backgrounds.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_b9fd66"` — `tj-45e2a43f`, failed because `PanelRole` needed a fully-qualified path.
- Rerun — `tj-f6ec3e1b`, failed due rustfmt line wrapping in source assertion.
- Adjusted assertion to match formatted source.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_b9fd66"` — `tj-92123f31`, passed
