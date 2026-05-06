# bd-31b35f: defer Arc clones in TUI pending upload budget

## What changed

- `SurfaceManager::pending_uploads()` now collects and sorts lightweight upload metadata first.
- The per-frame `max_uploads_per_frame` budget is applied before cloning `Arc<[u8]>` payload handles.
- Only selected upload records clone their cached payload from `image_cache` after truncation.
- Added regression coverage that no `Arc::clone(data)` occurs before the budget point and that selected payloads are cloned from the cache after truncation.

## Why

When many graphics surfaces are eligible but the frame budget sends only a subset, the old path cloned an `Arc` payload handle for every candidate before sorting/truncating. That made deferred surfaces pay avoidable atomic refcount churn on hot graphics frames. Sorting lightweight metadata first keeps upload order and budget behavior intact while reducing overhead on saturated Kitty/Ghostty frames.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_31b35f"` — `tj-777a0562`, passed
