# bd-399627: skip delete split allocation when delete queue is empty

## What changed

- `SurfaceManager::take_pending_deletes_split()` now checks `pending_deletes.is_empty()` first.
- On an empty queue, it returns `(Vec::new(), Vec::new())` immediately, avoiding the placement/image Vec allocations and the drain loop setup.
- Non-empty queues still split placement-only and full-image deletes exactly as before.
- Added source/runtime coverage for the empty-queue fast path.

## Why

Live and benchmark upload passes use the split delete drain whenever cleanup work may run. The common no-cleanup/empty-delete-queue path should avoid allocation and iteration setup so graphics-enabled steady frames stay closer to ASCII/text overhead.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_399627"` — `tj-bf5e7813`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_delete_split"` — `tj-19d7fc91`, passed
