# bd-f354b8: tick TUI Kitty upload backoff during mixed fetch/native/delete passes

## What changed

- Live Kitty upload loop now ticks `SurfaceManager::tick_backoff_counters()` when a non-animation pass has upload backoff but no regular upload candidates, even if other graphics work brought it through the full upload path.
- Real-dashboard benchmark upload loop mirrors the live behavior.
- `pending_uploads()` is still skipped in that mixed path, so the optimization from prior pending-upload scan reductions remains intact.
- Added regression coverage for live and benchmark branches.

## Why

After moving backoff ticking out of the full `pending_uploads()` scan, pure-backoff frames had a fast path, but mixed frames could still skip `pending_uploads()` when there were no regular uploads. If fetch/native/delete work coexisted with backed-off regular uploads, the pure-backoff fast path did not run and the counters could stall. Explicitly ticking in the mixed branch preserves retry timing without reintroducing a full pending-upload scan.

## Validation

- Initial targeted test caught an overly broad assertion that matched the intended pure-backoff fast path.
- Tightened the assertions to inspect only the mixed-work branches.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f354b8"` — `tj-2c2a7ee4`, passed
