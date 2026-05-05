# bd-2a8d13: reuse cleanup summary in benchmark upload gate

## What changed

- `benchmark_upload_pending()` now computes `pending_cleanup` once before the upload-pass gate.
- The benchmark path now calls `should_run_kitty_upload_pass_with_summary_and_cleanup(...)`, matching the live path and passing that precomputed cleanup boolean.
- Added regression coverage for the benchmark gate shape.

## Why

Real-dashboard benchmarks should mirror live graphics behavior while keeping measurement overhead truthful. The benchmark upload path previously called the older helper, which re-probed pending delete and animation-stop queues, then drained those queues later. Reusing the cleanup-aware gate removes duplicate cleanup queue probes from benchmark frames without changing delete/upload behavior.

## Validation

- First targeted run failed because the source assertion did not account for rustfmt line wrapping.
- Fixed the assertion.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2a8d13"` — `tj-f618c091`, passed
