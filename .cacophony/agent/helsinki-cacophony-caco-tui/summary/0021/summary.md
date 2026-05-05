# bd-2fb0f7: reuse TUI graphics pending-work summary inside upload pass

## What changed

- Added `PendingGraphicsWork` summary flags to `SurfaceManager`.
- `pending_graphics_work_summary()` now captures fetch, regular upload/backoff, and native-animation upload candidate state in one scan.
- `should_run_kitty_upload_pass_with_summary()` accepts that precomputed summary.
- The live upload pass and real-dashboard benchmark upload path reuse the summary to avoid immediately re-running native/regular upload preflight scans.
- Added regression coverage that the live upload pass uses the summary path and does not re-scan native/regular candidates after the summary scan.

## Why

bd-b9004e collapsed the app upload-pass gate into one preflight scan, but the upload body still rechecked native and regular upload candidate helpers before doing work. Reusing the same summary trims low-work and no-op graphics frames further while preserving fetch/upload/native/delete/backoff semantics.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2fb0f7"` — `tj-59ff8b51`, passed

Earlier validation attempts:
- `tj-6d6f2442`: caught stale benchmark-support reference to the removed native preflight helper.
- `tj-177b4e58`: caught the regression assertion matching its own literal; fixed with split-string construction.
