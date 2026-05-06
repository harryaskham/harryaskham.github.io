# bd-e4218a: skip empty background cache lookup when no cache exists

## What changed

- `App::flush_graphics_requests()` now skips `background_cache_lookup_for_layers()` when the app-level `graphics_background_cache` is empty.
- First render/no-cache backgrounds render directly and create `BackgroundSurfaceSnapshot::new_with_layers()` only for cache insertion after a surface actually renders.
- Existing-cache frames keep the full lookup path, preserving cache-hit reuse, stale-cache invalidation, and cleanup behavior.
- Added source-shape coverage for the empty-cache lookup guard.

## Why

When no app-level background cache entries exist, a cache lookup cannot hit. The old path still built a background snapshot and probed the cache before rendering. This avoids that first-render/no-cache overhead, moving snapshot construction to the point where it is needed for insertion.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- Initial `caco test run --wait --command "cargo test -p caco-tui bd_e4218a"` — `tj-40a03c97`, failed due rustfmt line wrapping in source assertion.
- Adjusted assertion to match formatted source.
- Rerun `caco test run --wait --command "cargo test -p caco-tui bd_e4218a"` — `tj-c82e4c17`, passed
