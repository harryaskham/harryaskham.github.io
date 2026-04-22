# Session summary 0025 — bd-d8fc57: bead parent_bead_id (slice 1)

## Goal

Add a first-class `parent_bead_id` field to the bead model so the
parent-child relationship currently encoded in title prefixes
("[bd-XXX follow-up]") becomes structured data, queryable later
by tree-rendering and child-aware close UX.

## Bead(s)

- `bd-d8fc57` slice 1 — schema + create flag.

## Before state

- Parent/child decomposition was prose-only (title prefix). No
  field; no index; no way to walk descendants programmatically.

## After state

- `Bead.parent_bead_id: Option<String>` on the model.
- `issues.parent_bead_id TEXT` column + `idx_issues_parent_bead_id`
  index. ALTER TABLE migration handles existing DBs.
- `CreateBeadParams.parent_bead_id` propagated through all
  CreateBeadParams call sites (4 sites updated).
- `CreateBeadRequest.parent_bead_id` JSON field.
- `caco bd create --parent bd-XXX` CLI flag.
- `caco bd info` renders `parent: bd-XXX` when present.

## Diff summary

- Commit: `b97d71e4`.
- Files (6): caco-beads model.rs + store.rs, caco-daemon
  beads.rs + audit.rs + release_queue.rs, caco-cli lib.rs.
- `cargo build` and `cargo clippy` for caco-beads + caco-daemon
  + caco-cli: clean.

## Operator-takeaway

When filing follow-ups, use:
`caco bd create --title '...' --parent bd-XXX ...`
to attach the new bead as a child. `caco bd info bd-CHILD` will
show the parent reference. Tree walks (`caco bd list --tree
bd-XXX`), close-children prompting, and parent auto-close are
deferred follow-ups but the persistence layer is now in place.
