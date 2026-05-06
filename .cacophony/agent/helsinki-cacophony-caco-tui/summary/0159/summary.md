# bd-349f0f: skip retained forget scans when retained cache is empty

## What changed

- `SurfaceManager::forget_retained_image_id()` now returns immediately when `retained_images` is empty.
- This avoids building the removal `Vec` and iterating retained variant maps for image-delete acknowledgements in cold/non-retained graphics sessions.
- Added focused coverage that:
  - the empty-cache guard appears before removal collection,
  - delete acknowledgement still recycles IDs on the empty-retained fast path,
  - non-empty retained caches still forget matching variants correctly.

## Why

`mark_image_delete_sent()` runs for every terminal image delete acknowledgement and always calls `forget_retained_image_id()`. Most non-retained/cold graphics paths have no retained variants, so collecting removals from an empty retained map is guaranteed work. This trims another small but frequent graphics-only cleanup cost without changing delete/recycle or retained/shared correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_349f0f"` — `tj-b7c0458c`, passed
- `caco test run --wait --command "cargo test -p caco-tui retained_image"` — `tj-a35b8e97`, passed
