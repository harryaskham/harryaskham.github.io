# bd-d61a31: skip animation-stop drain take when queue is empty

## What changed

- `SurfaceManager::take_pending_animation_stops()` now returns `Vec::new()` immediately when `pending_animation_stops` is empty.
- Non-empty queues still use `std::mem::take()` and drain exactly as before.
- Added source/runtime coverage for the empty-drain fast path and non-empty behavior.

## Why

Live and benchmark upload passes can drain animation-stop commands during cleanup handling. On most steady graphics frames the queue is empty, so mutating the stored Vec with `std::mem::take()` is unnecessary. This trims a small cleanup hot-path cost while preserving native-animation stop correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d61a31"` — `tj-8156f903`, passed
- `caco test run --wait --command "cargo test -p caco-tui animation_stop"` — `tj-761c3e10`, passed
