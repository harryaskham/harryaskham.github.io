# Session summary — bd-5012db upload-candidate empty-cache probe skip

## Bead

- `bd-5012db` — Skip upload-candidate cache probes when image cache is empty

## Before state

`SurfaceManager::has_pending_upload_candidates()` walked every surface and called `image_cache.contains_key(key)` for each not-uploaded non-native surface. When the image cache was empty, those probes were guaranteed misses. This path is used by graphics testbed/editor upload gates and can run on cold-cache or graphics-disabled redraws where avoiding extra HashMap work helps keep graphics overhead closer to text mode.

## Changes

- Hoisted `let image_cache_empty = self.image_cache.is_empty()` in `has_pending_upload_candidates()`.
- Short-circuited permanently-failed and backoff cases explicitly.
- Skipped per-surface `contains_key` probes when the image cache is empty.
- Preserved non-empty cache upload-candidate detection and preserved backoff detection even with an empty image cache.
- Added focused source/runtime coverage for the empty-cache fast path.

## Validation evidence

- `rustfmt --edition 2021 crates/caco-tui/src/kitty.rs` — passed.
- `git diff --check` — passed.
- `caco test run --wait --command "cargo test -p caco-tui pending_upload_candidate_preflight"` — `tj-980f903d`, passed.
- `caco test run --wait --command "cargo clippy -p caco-tui --lib -- -D warnings"` — `tj-7f912e32`, passed.
- `caco test run --wait --command "cargo test -p caco-tui"` — `tj-e1e897e0`, passed.

## Result

The upload-candidate preflight now avoids guaranteed-miss image-cache probes on empty caches while preserving correctness for cached uploads and backoff ticking.
