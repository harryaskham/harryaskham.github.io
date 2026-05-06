# bd-2ebe42: collapse placement-delete duplicate scans

## What changed

- `SurfaceManager::queue_placement_delete()` now uses one combined scan over `pending_deletes` for the non-empty queue path.
- That single scan detects both:
  - a full-image delete already queued for the image (supersede), and
  - an exact duplicate placement-only delete.
- Empty queues still use the direct-push fast path.
- Added source/runtime coverage for the combined scan, duplicate coalescing, and full-image supersede behavior.

## Why

Placement delete queueing previously scanned the same vector twice on non-empty queues. Retained replacement and cleanup paths can queue placement deletes during graphics transitions, so this trims graphics cleanup overhead without changing terminal delete correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2ebe42"` — `tj-787ddb41`, passed
- `caco test run --wait --command "cargo test -p caco-tui queued_placement"` — `tj-5f13519f`, passed
