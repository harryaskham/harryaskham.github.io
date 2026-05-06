# bd-99d8ac: skip pending-delete scans when delete queue is empty

## What changed

- `SurfaceManager::cancel_pending_delete_for_display()` now returns immediately when `pending_deletes` is empty.
- Successful upload/retained redisplay still cancels stale full-image and matching placement deletes when the queue is non-empty.
- Added focused source/runtime coverage proving the empty-queue guard precedes `Vec::retain` and steady successful display leaves the empty queue untouched.

## Why

Every successful upload path calls `mark_uploaded_with_image_id()`, which calls the delete-cancel helper. Steady cached graphics frames usually have no pending deletes, so running `Vec::retain` is empty work. This trims another always-on graphics hot-path cost while preserving stale delete cancellation correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_99d8ac"` — `tj-d902f508`, passed
- `caco test run --wait --command "cargo test -p caco-tui successful_display"` — `tj-5e36fece`, passed
