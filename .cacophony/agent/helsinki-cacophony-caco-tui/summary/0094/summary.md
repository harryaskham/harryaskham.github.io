# bd-ef21ff: reduce app background cache-hit cloning

## What changed

- App-level background cache lookup now reports hit/miss state plus the already-built snapshot instead of returning cloned cached key/flag vectors.
- The cached fast path inspects cached entries in place and clones only the current surface key when inserting it into the owned active-background set.
- The test helper that exposes cached background keys still returns owned vectors for existing tests.
- Added regression coverage for the cache-hit path shape.

## Why

Steady graphics frames frequently hit the app-level background cache. The old lookup cloned the complete cached `surface_keys` and `draw_above_flags` vectors before the caller iterated them. Borrowing the entry for the lookup initially ran into Rust's disjoint-borrow constraints because the caller also mutates `surfaces` and perf counters. The final approach keeps the lookup cheap and avoids up-front vector clones while cloning only each key needed for active-set ownership.

## Validation

- First targeted run failed to compile due borrow conflicts from returning borrowed slices across later mutable field access.
- Second targeted run failed because the source assertion self-matched its forbidden string.
- Fixed by returning hit state and scoping source assertions.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_ef21ff"` — `tj-ff60d5f9`, passed
