# bd-f77031: reuse pending cleanup check in pure backoff path

## What changed

- The live Kitty upload loop already computes `pending_cleanup` before deciding whether the upload pass can run.
- The pure-backoff fast path now reuses that boolean instead of probing `has_pending_deletes()` and `has_pending_animation_stops()` again.
- Added regression coverage ensuring the fast path uses `pending_cleanup` and does not duplicate cleanup queue probes in that block.

## Why

Pure-backoff frames are intended to be the cheapest possible graphics pass: tick upload backoff counters and skip upload/fetch/delete work. Reusing the existing cleanup classification avoids duplicate queue checks on that hot path while preserving cleanup semantics when deletes or animation stops are pending.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f77031"` — `tj-63508848`, passed
