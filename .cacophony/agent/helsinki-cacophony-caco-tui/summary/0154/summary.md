# bd-950991: defer unchanged graphics payload hashing until upload

## What changed

- Removed render-time eager payload hash warming from unchanged graphics registrations:
  - `refresh_registered_enhancement_inner()` now updates the cached payload only when data changed.
  - `cache_image_data_arc()` likewise leaves missing hashes unset on unchanged registrations.
- Kept the existing lazy `cached_image_data_hash()` upload/retained lookup path as the only place that computes a missing PNG-content hash.
- Added runtime coverage showing changed payloads clear stale hashes and subsequent unchanged registrations do not refill a missing hash before upload-time lookup.
- Added source-shape coverage ensuring the eager hash helper is gone and unchanged refresh/cache paths do not call hash helpers.

## Why

Previous slices deferred hashing for changed payloads, but an unchanged registration after a changed payload could still compute the FNV hash during render-time registration if no upload path had needed it yet. On graphics-heavy frames, that can make the app pay PNG-byte scans before it knows a payload will be selected for upload/retained display. This keeps hashing demand-driven and closer to text-mode overhead on cached frames.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_950991"` — `tj-bf90e358`, passed
- `caco test run --wait --command "cargo test -p caco-tui image_data_hash"` — `tj-e4a73bf4`, passed
