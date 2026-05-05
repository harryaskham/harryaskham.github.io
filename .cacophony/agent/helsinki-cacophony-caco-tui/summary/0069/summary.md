# bd-69cd9b: clone TUI Kitty native animations only after upload-budget truncation

## What changed

- `SurfaceManager::pending_native_animation_uploads()` now collects lightweight native-upload metadata first:
  - key,
  - surface id,
  - rect,
  - upload-priority fields.
- Sorting and `max_uploads_per_frame` truncation happen before cloning `NativeAnimation` payloads.
- Only selected post-budget entries clone and return the `NativeAnimation` value to the upload pass.
- Added a regression test proving clone happens after sort/truncation and that return conversion is post-budget.

## Why

Native animation values include frame payload `Arc`s. The previous path cloned every eligible `NativeAnimation` before sorting and budget truncation, so candidates deferred by `max_uploads_per_frame` still paid clone/refcount overhead. Cloning only selected entries reduces upload-pass overhead during native-animation bursts while preserving priority ordering and budget behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_69cd9b"` — `tj-03a45264`, passed
