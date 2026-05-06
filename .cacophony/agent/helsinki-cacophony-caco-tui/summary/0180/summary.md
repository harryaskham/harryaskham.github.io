# bd-788538: skip empty retained cache probes in mark_retained

## What changed

- `SurfaceManager::mark_retained()` now skips local retained-variant lookup when `retained_images` is empty.
- It also skips shared retained lookup while computing byte-accounting alias state when `shared_retained_images` is empty.
- Non-empty local replacement/accounting behavior and shared retained alias byte de-duplication remain intact.
- Added focused source/runtime coverage for guarded local/shared probes and shared-alias byte accounting.

## Why

The first retained upload on a cold graphics session cannot have any local or shared retained entries yet. Probing both maps in that state is guaranteed miss work on the cached Kitty hot path. These empty-cache guards trim retained bookkeeping while preserving duplicate-payload accounting once retained caches are populated.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_788538"` — `tj-b3d6bf7e`, passed
- `caco test run --wait --command "cargo test -p caco-tui mark_retained"` — `tj-638f2222`, passed

Earlier focused run `tj-e336a628` failed due a rustfmt-sensitive source assertion; the assertion was updated and validation passed.
