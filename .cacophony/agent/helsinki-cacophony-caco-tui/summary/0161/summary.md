# bd-3dfb5d: skip placement-delete queue scans when delete queue is empty

## What changed

- `SurfaceManager::queue_placement_delete()` now checks `pending_deletes.is_empty()` first.
- On an empty queue, it pushes the placement delete directly and returns, avoiding the full-image-delete scan and duplicate-placement scan.
- Non-empty queues still preserve the existing behavior:
  - full-image deletes supersede placement deletes for the same image,
  - duplicate placement deletes are coalesced.
- Added source/runtime coverage for the empty-queue fast path.

## Why

Retire and retained-replacement paths can queue placement deletes frequently. On steady cached graphics frames the delete queue is usually empty, so scanning it before pushing the first placement delete is pure overhead. This trims another small graphics-only hot-path cost while keeping deletion correctness intact.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_3dfb5d"` — `tj-684f74a2`, passed
- `caco test run --wait --command "cargo test -p caco-tui queued_placement"` — `tj-6c1eb1fb`, passed
- `caco test run --wait --command "cargo test -p caco-tui queue_placement"` — `tj-1960dc78`, passed
