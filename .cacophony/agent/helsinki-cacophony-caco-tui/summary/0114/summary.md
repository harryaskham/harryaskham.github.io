# bd-071b8f: classify TUI pending upload ranks during summary

## What changed

- `PendingGraphicsWork` now carries `regular_upload_rank` when every regular upload candidate seen during the summary scan has the same upload priority rank.
- If mixed upload ranks are observed, the summary marks `regular_upload_rank_mixed` and upload collection falls back to per-key rank computation.
- `pending_uploads_with_summary()` accepts the already-computed summary and reuses the uniform rank when available.
- Live and real-benchmark upload paths now pass the summary they already computed into upload collection.
- Added coverage for uniform/mixed rank summary behavior and the source shape that reuses the summary rank.

## Why

The upload pass already scans surfaces once to decide whether work exists. For common frames where all pending regular uploads are the same tier (for example background-only fills), the old collection pass recomputed `surface_upload_rank(key)` for every eligible surface before sorting. Carrying a uniform rank through the summary avoids that redundant classification while preserving mixed-tier ordering correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_071b8f"` — `tj-0138d15e`, passed
