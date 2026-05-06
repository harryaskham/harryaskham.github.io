# bd-041edd: skip empty delete drains before split helper

## What changed

- `SurfaceManager::take_pending_deletes()` now returns `Vec::new()` immediately when `pending_deletes` is empty.
- `SurfaceManager::take_pending_placement_deletes()` now does the same for placement-only drains.
- Non-empty queues still delegate to `take_pending_deletes_split()` to preserve image-vs-placement split behavior.
- Added source/runtime coverage proving both drain-specific helpers return before invoking the split helper on empty queues.

## Why

Some live/test helper paths call the drain-specific delete helpers directly. Even after the split helper gained an empty-queue fast path, those callers still constructed the split tuple before returning one side. This trims that extra empty-frame allocation path and keeps graphics cleanup overhead closer to the ASCII/text path.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_041edd"` — `tj-ef4285a1`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_delete"` — `tj-b8675d9a`, passed
