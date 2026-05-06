# bd-df535f: skip image-delete queue scans when delete queue is empty

## What changed

- `SurfaceManager::queue_image_delete()` now checks `pending_deletes.is_empty()` first.
- On an empty queue, it pushes the full-image delete directly and returns, avoiding:
  - the existing full-image duplicate scan,
  - the placement-delete `retain()` supersede scan.
- Non-empty queues still preserve existing behavior: duplicate full-image deletes are ignored and full-image deletes supersede placement-only deletes for the same image.
- Added source/runtime coverage for the empty-queue fast path.

## Why

Retire/orphan cleanup can queue image deletes frequently, but steady cached graphics frames usually have an empty delete queue. Scanning/retaining an empty vector before pushing the first delete is pure graphics-only overhead. This keeps deletion correctness intact while shaving the common empty-queue path.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_df535f"` — `tj-57faa2f1`, passed
- `caco test run --wait --command "cargo test -p caco-tui queued_image"` — `tj-b7768334`, passed
- `caco test run --wait --command "cargo test -p caco-tui queue_image"` — `tj-16be6df1`, passed
