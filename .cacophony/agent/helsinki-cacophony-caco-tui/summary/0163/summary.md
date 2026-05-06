# bd-06c541: skip animation-stop duplicate scans when queue is empty

## What changed

- `SurfaceManager::queue_animation_stop()` now checks `pending_animation_stops.is_empty()` first.
- On an empty queue, it pushes the animation-stop image ID directly and returns, avoiding the duplicate `Vec::contains()` scan.
- Non-empty queues still preserve duplicate coalescing.
- Added source/runtime coverage for the empty-queue fast path and non-empty duplicate behavior.

## Why

Native-animation replacement/retry paths can queue animation stops. In the common steady path the stop queue is empty, so checking for duplicates before the first push is avoidable graphics cleanup overhead. This keeps correctness identical while shaving another small graphics-only hot-path cost.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_06c541"` — `tj-5eee392e`, passed
- `caco test run --wait --command "cargo test -p caco-tui animation_stop"` — `tj-b1d28537`, passed
