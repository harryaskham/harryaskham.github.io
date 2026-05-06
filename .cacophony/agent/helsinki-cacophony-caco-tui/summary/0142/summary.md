# bd-129309: reuse background root key during render

## What changed

- Added `render_graphics_background_with_layers_and_root_key()` so callers that already formatted the app-level background root key can pass it into the render helper.
- `flush_graphics_requests()` now uses that helper on background cache misses, reusing the `enh:background:<panel>` string it already needed for cache lookup/insertion.
- The existing `render_graphics_background_with_layers()` wrapper remains for other callers and formats the key once before delegating.
- The single-layer render path converts the borrowed root key to an owned string only when returning the rendered surface key.
- Added source-shape coverage ensuring the flush path passes the preformatted key and the root-key helper does not format it again.

## Why

On renderable background cache misses, the previous path formatted `enh:background:<panel>` for app cache handling and then formatted the same string again inside the render helper. Reusing the preformatted key removes duplicate allocation/formatting in the background miss hot path while preserving existing behavior for other call sites.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_129309"` — `tj-e982ff15`, passed
