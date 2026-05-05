# bd-008445: avoid duplicate image-cache lookup in TUI pending uploads

## What changed

- `SurfaceManager::pending_uploads()` now uses `filter_map()` and `image_cache.get(key)` to collect upload candidates.
- Eligible pending uploads clone their cached `Arc<[u8]>` from the same lookup used to prove image data exists.
- Preserved upload eligibility rules, sorting, budget truncation, and skipped-animation diagnostics.
- Added regression coverage to guard against the old `contains_key()` plus indexing pattern.

## Why

The previous pending upload collection checked `image_cache.contains_key(key)` during filtering, then indexed `image_cache[key]` in the map step. That was two `HashMap` lookups for each eligible upload candidate on graphics frames that already need to be as cheap as possible. Collapsing to one lookup trims steady upload-pass overhead without changing visible behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_008445"`
  - `tj-b51ede6f` failed because the assertion matched its own forbidden string.
  - `tj-1b8390f6` passed after splitting the assertion string.
