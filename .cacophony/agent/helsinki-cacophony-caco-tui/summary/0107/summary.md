# bd-4a858e: defer TUI image data hashing until upload

## What changed

- `SurfaceManager::set_image_cache_data()` no longer computes an FNV hash for every changed bitmap payload during render/registration.
- Changed payloads now replace the image cache and clear any stale cached hash.
- Existing lazy hash behavior remains in `cached_image_data_hash()`, so upload/retained paths compute the hash only when a selected payload actually needs it.
- Regression coverage asserts the render-time cache replacement path does not call `fnv1a_hash` and clears stale hashes for upload-time recompute.

## Why

Graphics panels can generate many changed bitmap payloads during a frame, but upload budgeting may defer some candidates. Eagerly hashing every changed PNG during render means deferred or never-uploaded candidates still pay an O(bytes) scan. Deferring hash computation shifts that work to the selected upload/retained path while preserving unchanged-payload hash reuse and correctness after data replacement.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_4a858e"` — `tj-844a9d64`, passed
