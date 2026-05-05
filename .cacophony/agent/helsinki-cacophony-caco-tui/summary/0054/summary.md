# bd-7cbaff: cache TUI Kitty image payload hashes across retained redisplays

## What changed

- Added `SurfaceManager::image_data_hashes` keyed by surface key alongside `image_cache`.
- Hashes are computed when image-cache data is installed or changed, and cleared when cached image data is removed/cleared.
- Live upload paths now use `SurfaceManager::cached_image_data_hash()` for retained-image lookup instead of rescanning every pending PNG payload with FNV-1a.
- Real-dashboard benchmark upload paths use the same cached hash path.
- Moved the shared FNV-1a helper into `kitty.rs` so `SurfaceManager` owns the payload-hash logic.
- Added regression coverage proving cached hashes update when payloads change, remain stable for repeated lookup, and fall back after retirement.

## Why

Retained Kitty images avoid retransmitting PNG bytes, but the upload path still hashed every pending PNG payload before it could discover the retained image. Animated/cached graphics phases often revisit the same `Arc<[u8]>` payload repeatedly, so rescanning those bytes is avoidable steady-state work. Caching the hash at data-install time keeps retained redisplay checks cheap and makes the graphics path closer to ASCII/text overhead once surfaces are warm.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_7cbaff"`
  - `tj-395c894b` failed and caught that the FNV helper was private to `app.rs`.
  - first retry hit transient daemon reachability.
  - `tj-fbc94cce` passed after moving the helper into `kitty.rs`.
