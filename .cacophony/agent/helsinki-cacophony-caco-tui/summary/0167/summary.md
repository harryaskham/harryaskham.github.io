# bd-491be2: collapse image-delete duplicate and placement scans

## What changed

- `SurfaceManager::queue_image_delete()` now uses one `pending_deletes.retain()` pass for the non-empty queue path.
- That single pass both:
  - detects whether the same full-image delete is already queued, and
  - removes placement-only deletes superseded by the full-image delete.
- Empty queues still use the existing direct-push fast path.
- Added source/runtime coverage for the single-pass non-empty path, duplicate coalescing, and placement-delete supersede behavior.

## Why

After the empty-queue fast path, non-empty image-delete queues still paid two passes: a duplicate full-image scan and then a retain pass. Retire/orphan cleanup can queue deletes during graphics transitions, so collapsing this to one pass trims cleanup overhead while preserving terminal delete correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_491be2"` — `tj-35014722`, passed
- `caco test run --wait --command "cargo test -p caco-tui queued_image"` — `tj-381afb8a`, passed
