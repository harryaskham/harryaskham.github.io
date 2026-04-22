# Session summary 0034 — bd-fb9318: estimated_effort field (slice 1)

## Goal

Add a first-class effort-estimate field to beads so the burndown
ETA work (caco fleet eta) has a concrete number to sum across
the open + in_progress queue.

## Bead(s)

- `bd-fb9318` slice 1 — schema + create-flag + info render.

## Before state

- Beads had `priority` but no effort axis. ETA could only be
  approximated as bead-count × average-cycle-time, which collapsed
  XS bug-fixes and L feature-implementations into one bucket.

## After state

- `Bead.estimated_effort: Option<String>`. Soft schema accepts
  T-shirt sizes (`XS`|`S`|`M`|`L`|`XL`) or positive numeric hours.
- `issues.estimated_effort TEXT` column + ALTER migration.
- INSERT/UPSERT writes the field; ON CONFLICT updates it; 4
  SELECT lists carry it; `row_to_bead_base` reads col 20.
- `CreateBeadRequest.estimated_effort` plumbs through
  `handle_create_bead` → `CreateBeadParams`.
- `caco bd create --effort <value>` with soft validation
  (T-shirt uppercased; numerics must be > 0).
- `caco bd info` renders `effort: <value>` when set.

## Diff summary

- Commit: `85617d9a`.
- Files (9): caco-beads model.rs + store.rs + sync.rs;
  caco-daemon audit.rs + beads.rs + lib.rs + release_queue.rs +
  ui_stream.rs; caco-cli lib.rs.
- `cargo build` and `cargo clippy` for the touched crates: clean.

## Operator-takeaway

Run `caco bd create --title "..." --effort M` (or `--effort 4`
for hours) to attach an estimate. `caco bd info` shows it.
Burndown ETA aggregation (`caco fleet eta`), `caco bd list`
EFFORT column, and `--sort effort` are bd-fb9318 slice 2.
