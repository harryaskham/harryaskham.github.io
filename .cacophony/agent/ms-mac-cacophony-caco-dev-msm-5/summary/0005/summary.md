# Session summary — bd-d4d8dd: priority-ordered bead selection for spawn_and_claim

## Goal

Confirm that `ThenAction::SpawnAndClaim`'s `bead: next_ready_bead`
selector walks priorities **P0 → P1 → P2 → P3** so a busy queue cannot
starve high-priority work behind low-priority dreamware. Pin the
contract with tests, rustdoc, and a SPEC line.

## Bead(s)

- `bd-d4d8dd` — Priority-ordered bead selection for spawn_and_claim
  (next_ready_bead should walk P0 → P3)

## Before state

Audit:

- `modes::claim_next_bead_goal` → `BeadsStore::claim_next_ready` →
  `BeadsStore::list_ready` → SQL `ORDER BY priority ASC, created_at ASC`.
- Priority is stored as `u8` with P0=0..P3=3, so `priority ASC`
  already drains highest-priority first; `created_at ASC` is FIFO
  within priority.

Conclusion: ordering is **already correct**. No selector behaviour
change required. What was missing was test coverage and documentation
to lock the contract in.

Existing test `claim_next_ready` only verified P0-vs-P4 (two beads);
no test pinned the full ladder or the within-priority FIFO.

## After state

- Failing tests: none.
- Two new tests in `crates/caco-beads/src/store.rs`:
  - `claim_next_ready_walks_priority_ladder_p0_to_p3`: inserts beads
    in mixed-priority order (P3, P0, P2, P1) and asserts drain order
    is P0 → P1 → P2 → P3.
  - `claim_next_ready_ties_break_by_created_at_within_priority`:
    three same-priority beads created with sleep-spaced timestamps
    drain oldest-first.
- Rustdoc on `list_ready` and `claim_next_ready` documents the
  ordering contract and links it to `SpawnAndClaim`.
- SPEC §6.5.2 gains a bullet making the `next_ready_bead`
  priority-ladder + FIFO contract operator-visible.
- `cargo clippy -p caco-beads --tests` clean.

## Diff summary

- Commits: `6a9f40c2` + SPEC update
- Files touched:
  - `crates/caco-beads/src/store.rs` (+110 lines: 2 tests + rustdoc)
  - `SPEC.md` (+1 line in §6.5.2)
- Tests: +2, 0 removed, 0 flipped.

## Out of scope (deferred)

The bead also proposed additional selector variants
(`next_ready_bead_p0_p1`, `next_ready_bead_p0_only`) so operators
could define burndown variants that drain only high-priority work.
That's a separate feature (new `SpawnAndClaimParams` variants +
config validator + tests) and not required to fix the
"starvation by dreamware" risk this bead documented. Filing as a
follow-up if anyone asks.

## Operator-takeaway

`bead: next_ready_bead` already walks P0 → P3 with FIFO tie-break —
the proposed change was already in the code, just unproven. Now it
has rustdoc, two regression tests, and a SPEC bullet, so any future
refactor that flips the SQL `ORDER BY` will fail loudly in CI.
