# bd-df49a9: avoid upload-pass scans while TUI graphics surfaces are only backing off

## What changed

- Split `PendingGraphicsWork::regular_uploads_or_backoff` into:
  - `regular_uploads`,
  - `backoff_ticks`.
- Added `PendingGraphicsWork::has_upload_or_fetch()` for distinguishing real fetch/upload/native-animation work from pure backoff ticking.
- Live upload pass now detects pure backoff-only frames and calls `SurfaceManager::tick_backoff_counters()` without entering the full upload/delete/fetch scan path.
- Real-dashboard benchmark upload path uses the same distinction for non-animation backoff-only frames.
- Added regression coverage for summary classification and the live fast-path shape.

## Why

Upload failure backoff only needs counters decremented between retry attempts. The previous combined flag forced the full graphics upload pass whenever any surface had `backoff_remaining > 0`, even when no fetch/upload/delete work was possible. Separating pure backoff ticking trims unnecessary scan/write-path overhead from degraded graphics sessions while preserving retry timing and full behavior whenever real work is pending.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_df49a9"` — `tj-a3107af1`, passed
