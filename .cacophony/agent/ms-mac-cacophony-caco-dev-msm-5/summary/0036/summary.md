# Session summary 0036 — bd-16de2f: bd list EFFORT column + --sort effort (slice 1, redo)

## Goal

Surface bd-fb9318's `estimated_effort` field in `caco bd list` and
add a sortable axis. (Original attempt landed in summary 0035 but
was lost in a cherry-pick conflict during reintegrate; this is the
clean re-application on top of the latest agent branch.)

## Bead(s)

- `bd-16de2f` slice 1 — list column + sort axis only.

## Before state

- bd-fb9318 slice 1 had landed estimated_effort field, --effort
  flag, and bd info rendering.
- caco bd list did not show effort and had no sort axis using it.

## After state

- `caco bd list` auto-shows EFFORT column when any bead has an
  estimate (matches WORKER_AGE auto-detect convention).
- `BeadSortField::EstimatedEffort` variant; parse_bead_sort
  accepts `estimated_effort` / `effort`.
- ORDER BY pushes NULL estimates last; in-memory
  sort_beads_in_place / sort_global_beads_in_place arms use
  effort_sort_key for canonical T-shirt → numeric mapping
  (XS=1, S=2, M=4, L=8, XL=16; free-form numerics parsed via
  f64; None/unparseable → u32::MAX).

## Diff summary

- Commit: `748e919e`.
- Files (3): caco-beads store.rs, caco-daemon beads.rs,
  caco-cli lib.rs.
- `cargo build` for caco-beads + caco-daemon + caco-cli: clean.

## Operator-takeaway

`caco bd list --status open --sort effort` now orders smallest
beads first; `--reverse` for biggest first. EFFORT column appears
automatically once any bead in the slice has an estimate.
`caco fleet eta` aggregation deferred to bd-16de2f slice 2.
