# bd-06b0a8: avoid retained shared-map scans for known-untracked deletes

## What changed

- Added `cleanup_orphaned_image_after_shared_untracked()` / `cleanup_orphaned_image_inner()` so callers can queue orphan image deletes without re-scanning `shared_retained_images` when they already removed the shared lookup.
- Per-surface retained eviction, global retained-byte eviction, and `remove_retained()` now use the no-rescan cleanup path immediately after `remove_shared_retained_image_if_unbacked()`.
- Kept the original cleanup path for callers that have not already untracked shared retained aliases.
- Added source-shape coverage and ran the retained-image regression suite.

## Why

Retained-image eviction/remove paths already know the `(hash, image_id)` they are removing and call `remove_shared_retained_image_if_unbacked()`. The old cleanup then retained-scanned the whole `shared_retained_images` map by image ID before queuing a delete. Skipping that second scan trims cleanup work on graphics churn/eviction frames while preserving correctness for generic orphan cleanup callers.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_06b0a8"` — `tj-f10cf9b8`, passed
- `caco test run --wait --command "cargo test -p caco-tui retained"` — `tj-191c26e9`, passed
