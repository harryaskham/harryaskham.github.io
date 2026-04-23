# Session summary — bd-96dcf5 reconciler dedup discriminator

## Goal

Stop the reconciler from tagging benign duplicate-row dedup
operations as DESTRUCTIVE, fixing alarm fatigue that eroded
bd-cf99b7 detection.

## Bead(s)

- `bd-96dcf5` (P2 bug) — false-alarm DESTRUCTIVE markers on
  benign dedup.

## Before state

- Any reconcile with negative delta → `⚠ DESTRUCTIVE` prefix.
- ~10-15 false alarm commits/hour from duplicate-row cleanup.
- caco-ctrl escalated the pattern as bd-cf99b7 regression.
- 2 existing tests (grow=no-destructive, shrink=destructive).

## After state

- DESTRUCTIVE only when bd-ID set genuinely shrinks (at least one
  ID lost). Dedup operations that preserve all IDs get `dedup (all
  IDs preserved)` suffix instead.
- ID extraction uses cheap string-match (`"id":"` prefix) to avoid
  full serde parse on the reconcile hot path.
- 3 commit-msg tests: grow, genuine-shrink, dedup-only. All pass.
- bd-53f5a7 shrink-cap abort path unchanged (operates on line
  count, not ID sets).

## Diff summary

- `crates/caco-beads/src/store.rs`: +83 / -2 — ID-set comparison
  logic + dedup label + new test.
- Behavioural delta: reconcile commit messages now distinguish
  dedup from loss; DESTRUCTIVE only on genuine ID loss.
- cargo test-small green (2842); clippy clean for caco-beads.

## Operator-takeaway

The alarm noise from tonight's ~5/h false positive rate is fixed.
`git log` on the beads branch will now show `dedup (all IDs
preserved)` for benign cleanups and `⚠ DESTRUCTIVE` only when
real data loss occurs, restoring the signal-to-noise ratio the
bd-cf99b7 pipeline depends on.
