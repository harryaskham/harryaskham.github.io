# bd-ef507a: avoid hashing unchanged TUI Kitty image payloads while caching hashes

## What changed

- `refresh_registered_enhancement_inner()` and `cache_image_data_arc()` now preserve the existing `image_cache` entry and cached payload hash when the incoming data is unchanged.
- Pointer-identical steady frames avoid both Arc replacement and hash recomputation.
- Byte-identical steady frames with `surface_upload_dedupe` enabled preserve the original cached Arc/hash after the existing byte-equality check determines that no upload invalidation is needed.
- Changed payloads still replace `image_cache`, reset upload state, and compute/store a new FNV payload hash.
- Added regression coverage for pointer-identical, byte-identical, changed-payload, and post-retire fallback hash behavior.

## Why

bd-7cbaff cached payload hashes for retained-image lookups, but the initial implementation computed the hash every time cached image data was installed — including steady frames where the payload was unchanged. That could reintroduce the same PNG byte scan the cache was meant to avoid. This keeps retained redisplay checks cheap once graphics surfaces are warm.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_ef507a"`
  - `tj-b127393b` passed but matched zero tests before the regression test was renamed to include the bead ID.
  - `tj-1892e64e` passed with the intended regression test.
