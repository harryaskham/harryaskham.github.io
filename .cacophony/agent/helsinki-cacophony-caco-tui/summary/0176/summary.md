# bd-8989ce: skip retained order scan on new variant insert

## What changed

- `SurfaceManager::mark_retained()` no longer calls `retained.order.retain(...)` in the new-variant insertion path.
- The variants map already proved `data_hash` absent in that branch, so the per-surface retained-order scan was redundant.
- Replacement updates still retain/remove the old hash position before pushing to the back, preserving move-to-back and duplicate-free LRU semantics.
- Added focused source/runtime coverage for new insert order, replacement move-to-back behavior, and no duplicate retained-order entries.

## Why

Retained Kitty redisplay is the primary path for getting graphics closer to text-mode cost. New retained variants are common when a panel/background changes content or geometry. Skipping an avoidable per-surface LRU scan trims retained-cache maintenance while preserving eviction correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_8989ce"` — `tj-95206c4c`, passed
- `caco test run --wait --command "cargo test -p caco-tui mark_retained"` — `tj-06c74635`, passed

Initial validation enqueue hit transient local daemon reachability; the same checks passed on retry.
