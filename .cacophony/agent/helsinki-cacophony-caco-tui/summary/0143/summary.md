# bd-279281: skip duplicate background renderability scan

## What changed

- Threaded the precomputed `background_layers_renderable` flag from `flush_graphics_requests()` into `render_graphics_background_with_layers_and_root_key()`.
- The root-key render helper now accepts `layers_known_renderable`; flush cache-miss callers pass `true`/the precomputed flag, while existing wrapper callers pass `false` and keep the defensive all-None check.
- Added source-shape coverage confirming the flush path passes the precomputed renderable flag and the render helper guards the all-None scan behind `!layers_known_renderable`.

## Why

The flush path already scans resolved background layers to decide whether background work is possible. On renderable cache misses, the render helper was scanning the same layer slice again to check for all-None. Passing the known renderability avoids duplicate per-panel iteration in the render-miss hot path while keeping existing direct-call safety.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_279281"` — `tj-182480d8`, passed
