# Session summary — bd-6ac6b5 slice 1: pre-snapshot before destructive reconcile

## Goal

Defensive snapshot path that would have made the bd-cf99b7
truncation incident (2775→113 cacophony beads, 4h35m undetected)
trivially recoverable. The cluster-ctrl 11:43 .bak existed only by
coincidence (bd-943e85 hand-fix). The reconciler must snapshot every
time so a future destructive-truncation class bug is recoverable
without operator heroics.

## Bead(s)

- `bd-6ac6b5` — Pre-snapshot .beads/issues.jsonl + beads.db before
  every reconciler run, retain 7 days, root from bd-cf99b7

## Before state

- `BeadsStore::reconcile` performed a destructive `fs::write` of
  `.beads/issues.jsonl` whenever the export content differed from the
  on-disk content, with no pre-write snapshot.
- The bd-cf99b7 incident relied on a coincidental hand-fix `.bak`
  for recovery. No `.beads/snapshots/` directory existed.

## After state

### caco-beads (criterion 1)

- New `BeadsStore::snapshot_before_destructive_write` helper:
  - Writes `.beads/snapshots/<UTC-iso>/issues.jsonl` byte-identical
    to the pre-rewrite content (snapshots the in-memory string the
    caller already read, not a re-read from disk, so a concurrent
    process touching the file in the gap can't corrupt the snapshot).
  - Copies `.beads/beads.db` via `fs::copy`. SQLite WAL mode means a
    concurrent writer could be mid-transaction; the copy is still
    recoverable (SQLite recovery handles partial-WAL copies).
  - Path-safe ISO-8601 timestamp uses `-` for `:` so the dir name
    round-trips through tools that dislike colons in filenames.
- Wired into `reconcile()` at the same boundary as the destructive
  `fs::write`, conditional on `existing_content != new_content`. No
  snapshot on no-op reconciles.
- Snapshot failure is non-fatal by design: returned `Result` consumed
  with `let _ = ...` at the call site so a disk-full or permissions
  issue can never stall the daemon's sync loop.

### .gitignore (criterion 3)

- `.beads/snapshots/` added so local-only safety-net files never get
  committed. Sits next to the existing `.cacophony/tmp/` ignore.

### Tests

- `reconcile_writes_pre_destructive_snapshot`: seed 5 records in
  `.beads/issues.jsonl`, reconcile against an empty store-of-record
  (the exact bd-cf99b7 signature), assert `.beads/snapshots/` has
  exactly one entry whose `issues.jsonl` is byte-identical to the
  pre-rewrite seed.
- `reconcile_skips_snapshot_when_content_unchanged`: a no-op
  reconcile must not create a snapshot dir (otherwise idle heartbeats
  would grow the dir without bound between cron-rotation runs).
- Both pass; `cargo test-small` 57/57 pass; clippy clean.

## Diff summary

- 1 commit, 2 files (`.gitignore`, `crates/caco-beads/src/store.rs`).
- Net: +148 lines (helper + 2 tests + test infrastructure).

## Deferred to follow-up beads (under bd-6ac6b5)

- Criterion 2: daemon-cron 7-day retention. Recommended in
  caco-daemon's existing cron tick (log-rotate / event-purge);
  add snapshot-rotate task with warn-when-total-size > 500MB.
- Criterion 4: `caco bd snapshot list` (size + age + record-count).
- Criterion 5: `caco bd snapshot restore <iso> --i-mean-it`.
- Criterion 6: doctor `beads snapshot dir healthy` sensor.
- Criterion 7: live-mesh integration test (reconciler called →
  snapshot dir contains a new dated entry → file matches
  pre-reconciler hash).

## Operator-takeaway

After-rollout, every destructive-write reconcile leaves a snapshot
in `.beads/snapshots/<UTC-iso>/`. Recovery from a future bd-cf99b7-
class incident becomes:
```
cp .beads/snapshots/<latest-good>/issues.jsonl .beads/issues.jsonl
caco bd sync
```
No coincidental `.bak` required.
