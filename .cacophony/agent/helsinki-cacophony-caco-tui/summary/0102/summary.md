# bd-960c62: reuse background root key for cache lookup

## What changed

- `background_cache_lookup_for_layers()` now accepts the caller's preformatted `background_root_key`.
- `flush_graphics_requests()` passes its existing `background_root_key` into the lookup helper instead of letting the helper format the same `enh:background:<panel_id>` string again.
- The test helper path formats the root key once and passes it through too.
- Added regression coverage for the helper signature and live call shape.

## Why

The background fast path runs for every graphics panel. `flush_graphics_requests()` already formats `background_root_key` for active-root tracking and later cache insertion, but the lookup helper formatted the same string again just to probe `graphics_background_cache`. Passing the key through avoids duplicate allocation/formatting on steady cached background frames.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_960c62"` — `tj-56a3d1b1`, passed
