# Session summary — bd-53f5a7 slice 1: reconciler destructive-shrink abort guard

## Goal

There is no bead-delete API. Therefore any sync-reconcile that REDUCES
jsonl record count is by definition a bug. On 2026-04-22 18:39:36 BST
the v1.2.491→512 reconciler deleted 2664 of 2775 records in a single
commit (bd-cf99b7 RCA). The reconciler should be incapable of
net-deletion in steady-state operation.

## Bead(s)

- `bd-53f5a7` — [BLOCKER] Reconciler must never net-delete jsonl
  records — cap shrink at 5% w/ operator confirm + abort, reroot from
  bd-cf99b7

## Before state

- `BeadsStore::reconcile` would happily write any computed export
  content over `.beads/issues.jsonl`, regardless of magnitude.
- The bd-cf99b7 incident landed because nothing checked
  prev_count vs new_count before the destructive `fs::write`.
- bd-6ac6b5 (just shipped) added a pre-snapshot, but the snapshot is
  recovery-after-the-fact; this BLOCKER prevents the destructive
  write from happening at all.

## After state

### caco-beads (criteria 1, 2, 4)

- New `BeadsError::DestructiveShrinkRefused { prev, new, delta, pct }`
  with a self-explanatory message that references bd-53f5a7 and
  bd-cf99b7 so the operator who hits it knows immediately what to do.
- New `ReconcileOptions { allow_shrink: bool }` in `caco-beads::model`
  for the override path. `Default` impl is `allow_shrink: false`.
- Refactored `reconcile()` to delegate to
  `reconcile_with_options(ReconcileOptions::default())`. All existing
  callers continue to use the safe path; new callers that need
  legitimate shrink (operator-driven cleanup) opt in explicitly.
- Guard fires inside the destructive-write branch BEFORE the bd-6ac6b5
  snapshot, so a refused shrink doesn't litter `.beads/snapshots/`
  with a forced entry.

### Threshold

- `max(5%, 50 records)` — both must exceed for the guard to trip.
- 5% catches the bd-cf99b7 truncation (2775 → 113 = 96% shrink).
- 50-record floor avoids tripping on small projects where a
  legitimate 1-record delete looks like 50%+.

### Tests

- `reconcile_aborts_on_destructive_shrink_bd_cf99b7_signature`: seed
  the exact 2775-record bd-cf99b7 count, reconcile against an empty
  store-of-record, assert (a) `DestructiveShrinkRefused` with correct
  prev/new/delta/pct fields, (b) jsonl is byte-identical to the seed
  (no partial mutation), (c) line count unchanged.
- `reconcile_allows_small_shrink_under_record_floor`: 5-record seed
  reconciled to empty (100% shrink but only 5 records); must succeed
  because 5 ≤ 50-record floor. This also keeps the existing
  `reconcile_commit_msg_marks_destructive_shrink` test green.
- `reconcile_with_allow_shrink_bypasses_guard`: same 2775-record
  seed, `reconcile_with_options(ReconcileOptions { allow_shrink:
  true })` must succeed and the destructive write proceeds.
- All 11 reconcile tests pass; `cargo test-small` 57/57 pass; clippy
  clean.

## Diff summary

- 1 commit, 3 files (`crates/caco-beads/src/error.rs`,
  `crates/caco-beads/src/model.rs`, `crates/caco-beads/src/store.rs`).
- Net: +191 / -2 lines.

## Deferred to follow-up beads (under bd-53f5a7)

- Criterion 3: `caco bd sync --allow-shrink` CLI flag plumbing
  through to `ReconcileOptions { allow_shrink: true }`. Trivial once
  the daemon side accepts an option in the sync request.
- Criterion 5: live-mesh integration test (full reconcile cycle on a
  2000-record fixture; assert `prev_count <= new_count` always).
- Criterion 6: `caco doctor` sensor 'beads jsonl shrink in last 24h'
  that scans the beads-branch git log for any commit with >5%
  net-deletion.

## Operator-takeaway

After-rollout, any future bd-cf99b7-class reconciler bug surfaces as:
```
reconcile refused: would shrink jsonl from 2775 to 113 records
(delta=-2662, 95.9%); this exceeds the safety cap (max(5%, 50
records)). Re-run with allow_shrink=true if intentional.
See bd-53f5a7 / bd-cf99b7.
```
The destructive write never lands on disk. Combined with bd-6ac6b5
(pre-snapshot defence) and bd-fa0603 (peer-divergence sensor), this
class of incident now has three independent layers of protection.
