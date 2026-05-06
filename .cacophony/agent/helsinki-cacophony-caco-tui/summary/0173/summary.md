# bd-591449: skip benchmark delete batch allocation when cleanup is empty

## What changed

- `App::benchmark_upload_pending()` now computes `pending_cleanup` after draining animation-stop/delete queues in the batched upload path.
- The benchmark path only allocates and fills `delete_batch` when cleanup exists.
- Combined batch capacity and append logic now treat the delete batch as optional, so upload-only benchmark frames do not pay for an empty delete command buffer.
- Fallback handling for non-empty animation stops, placement deletes, and full-image deletes remains intact.
- Added focused source coverage proving the cleanup guard precedes delete-batch allocation and that combined batch assembly handles an absent delete batch.

## Why

The live upload path already skipped empty batched delete assembly. The benchmark path still mirrored the older behavior, adding overhead to upload-only graphics frames that ASCII/text mode never pays. Matching the live path keeps benchmark graphics work more representative and avoids another small source of fake parity/noise when comparing cached Kitty frames against text mode.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_591449"` — `tj-8c68eb2f`, passed
- `caco test run --wait --command "cargo test -p caco-tui benchmark_upload_path"` — `tj-c4ad3d23`, passed
