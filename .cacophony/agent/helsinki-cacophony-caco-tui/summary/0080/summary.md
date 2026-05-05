# bd-eed9b1: skip TUI Kitty pending-work summary when uploads are disabled

## What changed

- Live upload loop now checks whether graphics uploads are capable before running `pending_graphics_work_summary()`.
- If graphics uploads are disabled/unavailable and there is no cleanup transport need, the loop skips the upload pass without scanning surface state.
- `should_run_kitty_upload_pass()` mirrors the same early decision for direct callers.
- Real-dashboard benchmark upload path uses an empty pending-work summary when graphics uploads are disabled, avoiding scan work while preserving cleanup handling.
- Added regression coverage for live and benchmark paths.

## Why

`pending_graphics_work_summary()` scans registered surface state. Text/ASCII, graphics-disabled, and no-upload terminal paths should not pay that graphics bookkeeping cost unless cleanup commands may still need to be sent. This keeps graphics-disabled benchmark/text-mode behavior closer to ASCII performance while preserving stale-placement cleanup semantics.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_eed9b1"` — `tj-d24b48ff`, passed
