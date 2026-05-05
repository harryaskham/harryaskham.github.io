# bd-313ae0: fold TUI Kitty upload backoff ticking into pending-upload scan

## What changed

- `SurfaceManager::pending_uploads()` now uses a single mutable surface walk to:
  - tick `backoff_remaining` counters,
  - skip backed-off surfaces for the current cycle,
  - collect eligible upload candidates,
  - count skipped cached animated surfaces for diagnostics.
- Removed the second full mutable `values_mut()` scan that previously existed only to decrement backoff counters after candidate collection.
- Preserved retry timing: surfaces whose backoff reaches zero during the scan still wait until the next upload cycle, matching the old collect-before-tick behavior.
- Added a regression test scoped to `pending_uploads()` guarding against reintroducing the second tick-only scan.

## Why

The graphics upload path may already be on a costly frame when `pending_uploads()` runs. Walking every surface again solely to decrement retry backoff counters added avoidable per-frame overhead. Folding the tick into the candidate collection pass keeps failure retry behavior intact while removing one complete surface-map traversal.

## Validation

- First `caco test run --wait --command "cargo test -p caco-tui bd_313ae0"` failed because the initial regression test matched the standalone `tick_backoff_counters()` helper instead of only the `pending_uploads()` body.
- Fixed the test to inspect only `pending_uploads()`.
- A retry enqueue briefly hit daemon reachability and was retried.
- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_313ae0"` — `tj-364ec7cc`, passed
