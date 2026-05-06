# bd-4a1ec6: skip retained-image scans when retained cache is empty

## What changed

- Added empty-cache guards to `SurfaceManager::retained_image_still_tracked()` and `retained_hash_image_still_tracked()`.
- The helpers now return immediately when `retained_images` is empty instead of constructing an iterator over an empty map.
- Added focused source/runtime coverage for both empty-cache fast paths and non-empty retained tracking behavior.

## Why

Retire/orphan cleanup paths consult retained-image tracking to decide whether a terminal image can be deleted. In ordinary cold/non-retained graphics sessions, the retained cache is empty, so scanning `retained_images.values()` is guaranteed empty work on a correctness hot path. The guard keeps behavior identical while shaving another small piece of graphics-only overhead.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_4a1ec6"` — `tj-3b5041f7`, passed
- `caco test run --wait --command "cargo test -p caco-tui retained_image"` — `tj-430a0116`, passed
