# Session summary — bd-a04b65 auto-claim queue hygiene

## Goal

Fix the board bug discovered during burn-down where bead-id-less `caco bd claim` could assign permanent reference/umbrella beads to workers, leaving them with non-implementation work in the dispatch path.

## Bead(s)

- `bd-a04b65` — caco bd claim must not assign permanent beads

## Before state

- Running `caco bd claim` with no bead id assigned `bd-5bfb2c`, even though it was `status: permanent` and meant to be a tracking umbrella.
- `claim_next_ready` skipped epics but allowed permanent beads returned by `list_ready` to flow into the auto-claim path.
- Explicit permanent bead claim/unclaim behavior existed and needed to remain intact.

## After state

- `BeadsStore::claim_next_ready` now skips `BeadStatus::Permanent` candidates before attempting claim.
- Explicit permanent claim/unclaim semantics are unchanged.
- Regression coverage verifies a P0 permanent bead is skipped in favor of a lower-priority open implementation bead and remains unassigned.

## Diff summary

- Commits: `6257de276` after replay onto the remote agent branch.
- Files touched: `crates/caco-beads/src/store.rs`.
- Tests: `cargo test -p caco-beads claim_next_ready_skips_permanent_beads --lib`; `cargo test -p caco-beads claim_permanent_bead_stays_permanent --lib`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: auto-claim drains only implementable open work and no longer strands permanent tracking beads on workers.

## Operator-takeaway

Permanent beads can still exist as visible reference/umbrella records, but they will not be handed to workers by the no-id burn-down claim path.
