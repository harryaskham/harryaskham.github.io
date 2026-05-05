# bd-f4be21: align live TUI graphics-frame telemetry with real placement work

## What changed

- Live upload telemetry now counts retained Kitty display-only fast paths via `GraphicsPerfTracker::record_upload_dedupe_hits()`.
- Live `graphics_frames` is now recorded only after actual placement work is known:
  - full upload success,
  - upload failure,
  - retained redisplay.
- Removed the prior early animation-redraw-only `record_graphics_frame()` path, which could count graphics frames before knowing whether any placement work happened.
- Added source-level regression coverage so the live path remains aligned with the benchmark evidence contract.

## Why

The benchmark contract was tightened so graphics-frame evidence cannot be satisfied by animation/delete bookkeeping alone. Live perf telemetry still counted animation redraws before upload work was known and failed to record retained redisplays in the main graphics counters. That made live telemetry disagree with benchmark semantics and could obscure whether optimized cached graphics frames were doing retained-display work or simply waking.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f4be21"` — `tj-dc219c2e`, passed
