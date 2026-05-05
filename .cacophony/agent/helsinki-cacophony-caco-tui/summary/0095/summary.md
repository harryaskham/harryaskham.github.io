# bd-163d63: reuse cleanup summary in live upload-pass gate

## What changed

- Added `should_run_kitty_upload_pass_with_summary_and_cleanup(...)`, which accepts the caller's precomputed `pending_cleanup` boolean.
- Kept the existing `should_run_kitty_upload_pass_with_summary(...)` API for direct callers and benchmark code; it still computes cleanup state internally.
- Switched the live Kitty upload loop to call the cleanup-aware gate using the `pending_cleanup` value it already computed before the graphics summary.
- Added regression coverage for the live loop calling the cleanup-aware gate with the precomputed boolean.

## Why

The live upload loop already checks delete and animation-stop queues to avoid pending-work summary scans when graphics uploads cannot run. It then called a helper that checked those same queues again. Reusing the precomputed cleanup state removes duplicate probes from steady graphics frames while preserving direct-predicate and benchmark behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_163d63"` — `tj-fc10ea1c`, passed
