# Session summary — bd-fd300a BeadTitle write-path validation

## Goal
Hoist bead title length validation into the typed `BeadTitle` wrapper for create and update paths so invalid titles fail before SQLite CHECK constraints.

## Bead(s)
- `bd-fd300a` — Beads validation: hoist title-length validation from CHECK constraint into a typed BeadTitle wrapper

## Before state
- `BeadTitle` existed and was used by the reimport repair path, but create/update write paths still relied on validator/string logic and ultimately the SQLite title CHECK as the hard floor.
- Validation had its own max-title constant instead of reusing the typed wrapper invariant.

## After state
- Added `BeadsStore::validate_title_for_write`, which constructs `crate::title::BeadTitle` before SQL writes.
- `create_bead_inner` rejects invalid titles before duplicate detection / mutation write.
- `update_bead` applies the same typed guard before setting a new title.
- `validation.rs` now shares `crate::title::TITLE_MAX_CHARS` and uses `BeadTitle::try_new` for overlong detection.
- Validation: `cargo test -p caco-beads --lib` passed 277/277; `cargo clippy -p caco-beads --all-targets` clean.

## Diff summary
- Commits: `8b6bfed38`, `066b8a945`
- Files touched: `crates/caco-beads/src/store.rs`, `crates/caco-beads/src/validation.rs`
- Tests: +1 regression test covering create/update overlong title failures before raw SQLite CHECK errors.
- Behavioural delta: create/update title length failures now surface through the typed wrapper path instead of SQLite.

## Operator-takeaway
The `BeadTitle` type is now load-bearing for normal bead create/update writes, not just a helper for reimport repair. Operators should see typed/actionable validation rather than raw SQLite title-length failures.
