# Session summary 0028 — bd-a23a7e: occurrence_count + last_seen_at (slice 1)

## Goal

Add first-class fields for aggregating bead recurrence so the
auto-filer (bd-05b1f8) can bump a counter instead of filing N
duplicate beads.

## Bead(s)

- `bd-a23a7e` slice 1 — schema + info rendering.

## Before state

- No way to express "this bead has happened 47 times" — duplicate
  occurrences either piled into the description or filed as
  separate beads (988 drafts as of 05:00 BST).

## After state

- `Bead.occurrence_count: u32` (default 1) and
  `Bead.last_seen_at: Option<DateTime<Utc>>`.
- `issues.occurrence_count INTEGER NOT NULL DEFAULT 1` +
  `issues.last_seen_at TEXT`. ALTER TABLE migrations added.
- `Bead::from_params` initialises `occurrence_count = 1` and
  `last_seen_at = Some(now)`.
- 4 SELECTs include both columns; INSERT/UPSERT writes them and
  ON CONFLICT branch updates them.
- `caco bd info` renders `occurrences: N (last seen <relative>)`
  when `occurrence_count > 1`.

## Diff summary

- Commit: `130f1dd9`.
- Files (3): caco-beads model.rs + store.rs, caco-cli lib.rs.
- `cargo build` and `cargo clippy` for caco-beads + caco-daemon
  + caco-cli: clean.

## Operator-takeaway

Persistence + display layer for recurrence aggregation is now in
place. Until the auto-filer (bd-05b1f8) bumps these fields on
duplicate detection, they remain at 1 / unset. `caco bd info`
shows the count only when > 1, so existing beads display unchanged.

A `caco bd list` sort/column for `occurrence_count` is a
follow-up and would let operators triage the noisiest recurrences
first.
