# bd-d44a6d: eliminate unreachable retained-success fallback lookups in TUI Kitty uploads

## What changed

- Live upload path:
  - `RetentionPlan` is now a non-optional `(image_id, data_hash, rect, z_index)` tuple.
  - Retained redisplay and full upload branches both carry the exact image id through success handling.
  - Removed success-time `get_retained(key).or_else(surface_id)` fallback lookups from batched/chunked and non-batched paths.
- Real-dashboard benchmark upload path mirrors the live non-optional retention plan behavior.
- Added regression coverage for live and benchmark upload-path bodies.

## Why

Both retained redisplays and full uploads already know the exact terminal image id that should be marked displayed on success. Keeping the plan optional forced unreachable fallback lookups and risked using a surface placement id when a retained image came from a shared global payload. Carrying the exact id trims unnecessary map probes and keeps retained/shared placement tracking truthful.

## Validation

- First test enqueue hit transient daemon reachability.
- First targeted run then caught overly broad source assertions that matched their own assertion strings; tests were narrowed to the upload path bodies.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d44a6d"` — `tj-aeed09d1`, passed
