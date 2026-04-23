# Session summary 0000 — bd-4c0e22: reconciler commit msg surfaces line delta

## Goal

Make destructive reconciler commits glanceable in `git log` /
`caco bd log` instead of needing forensic `git show --stat` to
detect catastrophic rewrites.

## Bead(s)

- `bd-4c0e22` — slice 1 of 5-point acceptance. Slices for
  `caco bd log` viewer + doctor sensor deferred to follow-on.

## Before state

- Commit message format:
  `bd: sync reconcile (X imported, Y exported, Z skipped)`
- Tonight's 2775→113 truncation (commit 1d6c4297) was
  `bd: sync reconcile (0 imported, 113 exported, 114 skipped)` —
  identical in shape to a benign no-op.

## After state

- New format:
  `bd: sync reconcile prev=P new=Q delta=±D (imported X, exported Y, skipped Z)`
- When `delta < 0`, prefixed with `⚠ DESTRUCTIVE `:
  `⚠ DESTRUCTIVE bd: sync reconcile prev=2775 new=113 delta=-2662 (...)`
- Counts use `.lines().filter(|l| !l.is_empty()).count()` so
  trailing-newline noise doesn't shift the delta.
- 2 new unit tests; existing `reconcile_with_git_creates_commit`
  unaffected.

## Diff summary

- Files (1): `crates/caco-beads/src/store.rs` (+62 lines).
- `cargo test -p caco-beads reconcile_commit_msg`: 2/2 pass.
- `cargo clippy -p caco-beads --all-targets -- -D warnings`
  clean.

## Operator-takeaway

Any future reconciler bug that net-deletes records will land
in git history with a `⚠ DESTRUCTIVE` prefix and an explicit
`delta=-N` count. Detection drops from forensic-dig to
glanceable in `git log --oneline`. Slice 2 (`caco bd log`
viewer + doctor sensor for last-24h destructive reconciles)
follow-on.
