# bd-6cb85c: drain Kitty delete queues in one pass

## What changed

- Added `SurfaceManager::take_pending_deletes_split()`, which drains placement deletes and full-image deletes from `pending_deletes` in a single pass.
- Kept `take_pending_deletes()` and `take_pending_placement_deletes()` compatibility behavior by re-queuing the other delete class after the split helper.
- Switched the live Kitty upload pass to use the split drain when processing delete/de-draw work.
- Switched both batched and non-batched real-dashboard benchmark upload paths to use the split drain.
- Added regression coverage for mixed placement/image delete draining.

## Why

Cleanup/de-draw frames are common when views, tabs, panes, modal overlays, or retained placements change. The live and benchmark upload paths previously walked the same `pending_deletes` queue once for placement deletes and again for image deletes. Draining both classes together avoids redundant queue scans while preserving the distinction between placement-only deletes (retain terminal-side image data) and full-image deletes.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_6cb85c"` — `tj-72afec5e`, passed
