# Session summary — bd-10e37c P1 broken-on-main: reconcile snapshot round-trip

## Goal

Fix `caco_beads::store::tests::reconcile_skips_export_when_content_unchanged` failing reproducibly on origin/main after bd-fb9318. Root-cause analysis found two distinct bugs (one in SQL, one in serialization round-trip).

## Bead(s)

- `bd-10e37c` — [broken-on-main] reconcile_skips_export_when_content_unchanged fails after bd-fb9318 estimated_effort schema add (P1, claimed and fixing)
- Filed: `bd-b5c287` (downgraded by this fix; closing).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean (post msm-5's bd-fb9318).
- `cargo test -p caco-beads --lib reconcile_skips_export_when_content_unchanged`: FAILED — second reconcile produced different content from first, so `skipped_export` stayed `false`.
- Hidden 2nd bug: `get_bead_from_db` SQL `SELECT` accidentally aliased `closed_by_session AS close_reason` (missing comma, then re-listed `close_reason, closed_by_session`), so column 20 (`estimated_effort`) ended up reading a timestamp from `last_seen_at`. Test output showed `"estimated_effort":"2026-04-22T08:05:08.022166+00:00"` after first reconcile.

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.
- `cargo test -p caco-beads --lib reconcile_skips_export_when_content_unchanged`: PASS.

## Implementation

Two bug fixes, both in `crates/caco-beads/src/`:

1. **SQL column-shift bug** (`store.rs` 4 sites: `get_bead_from_db`, `list_beads`, two more SELECT statements at lines 1873 and 2522): the SELECT list had `... close_reason, closed_by_session\n   close_reason, closed_by_session, parent_bead_id, ...` with no comma between the duplicated lines. SQLite read this as `closed_by_session AS close_reason` then continued, so columns 17-20 read parent_bead_id, occurrence_count, last_seen_at, estimated_effort with **shifted** indices. Fix: removed the four duplicate `close_reason, closed_by_session` lines.

2. **SnapshotBead round-trip** (`model.rs`): the `SnapshotBead` struct (used to parse plain-Bead JSONL lines on import) didn't carry the four newest fields (`parent_bead_id`, `occurrence_count`, `last_seen_at`, `estimated_effort`). After reconcile imported, exported, then re-imported a bead, `last_seen_at` was lost (defaulted to `None`), so the next export differed from the previous file content and `skipped_export` stayed `false`. Fix: added the four fields to `SnapshotBead` (with serde defaults matching `Bead::new`) and populated them in `into_bead()`.
- 11 construction sites of `SnapshotBead` swept via Python brace-walker.

## Diff summary

- `crates/caco-beads/src/store.rs` — 4 SELECT-list cleanups; removed debug repro module.
- `crates/caco-beads/src/model.rs` — `SnapshotBead` gains 4 fields + `default_occurrence_count_snap` helper; `into_bead()` plumbs them; 3 fixture sites updated.
- 8 SnapshotBead test fixture inserts in `store.rs`.
- Commit: `<TBD>`.

## Operator-takeaway

Two-bug interaction: msm-5 added `estimated_effort` as a new column at position 20, which exposed an old SQL bug (the duplicate-`close_reason, closed_by_session` malformed list, likely from a prior merge of two parallel column-add commits) AND simultaneously needed a SnapshotBead round-trip update for journal-content-equality. Either bug alone would have been masked; both together caused the reproducible failure. The SQL bug is the more dangerous one — it was silently misreading column 17-20 values in production reads of the `issues` table.

bd-b5c287 (the duplicate I filed earlier in 0020) is now superseded — closing as duplicate-of bd-10e37c.

## Coordination notes

- bd-274c2d cycle (msd-4 owns) doesn't intersect; this is a P1-bug claim, not a sweep.
- Resulting JSONL round-trip is now stable; should also fix any downstream reconcile→export→re-import flows in production daemon snapshot pulls.
