# Session summary 0029 — bd-a23a7e extension: --sort occurrence_count

## Goal

Extend bd-a23a7e (occurrence_count field) with a list-sort option
so operators can triage noisy recurrences first.

## Bead(s)

- bd-a23a7e extension (no separate bead — strict additive
  extension to bead landed earlier this session).

## Before state

- bd-a23a7e shipped occurrence_count + last_seen_at fields and
  caco bd info rendering.
- caco bd list had no way to sort by recurrence — the noisiest
  beads were buried in the priority/created_at default ordering.

## After state

- `BeadSortField::OccurrenceCount` variant.
- `parse_bead_sort` accepts `occurrence_count` and `occurrences`.
- `sort_beads_in_place` + `sort_global_beads_in_place` order
  by descending occurrence_count by default; `--reverse` swaps.
- ORDER BY clause emits
  `occurrence_count DESC, datetime(created_at) ASC, id ASC`.

## Diff summary

- Commit: `ee84ac10`.
- Files (2): caco-beads store.rs, caco-daemon beads.rs.
- `cargo build` and `cargo clippy` for caco-beads + caco-daemon
  + caco-cli: clean.

## Operator-takeaway

Run `caco bd list --status open --sort occurrence_count` to see
the most-recurring beads first. Pairs with the upcoming
auto-filer integration (bd-05b1f8) which will start populating
occurrence_count when it detects duplicates.
