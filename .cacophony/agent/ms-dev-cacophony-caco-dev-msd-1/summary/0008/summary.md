# Session summary — bd-c5c3a0 incremental label mutation

## Goal

The bead's primary ergonomic ask is incremental label management.
Existing infra (normalized `labels` table, `--labels` replace flag,
`--label` filter on `bd list`) was already wired; what was missing
was a way to add or remove labels without re-supplying the full set,
plus visibility of labels in the `bd show` output.

## Bead(s)

- `bd-c5c3a0` — Beads should support labels/tags (e.g. supervision,
  ux, perf, broken-on-main) for cross-bead grouping.

## Diff summary

`crates/caco-cli/src/lib.rs`:
- `BD_UPDATE_ARGS`: registered `--add-label` and `--remove-label`
  ArgSpecs (both repeatable, both optional).
- `dispatch_bd_update`: new signature accepts `multi_flags`. When
  `--add-label` and/or `--remove-label` is supplied:
  - rejects combination with `--labels` (full replace) — pick one
    mode, refuse to silently mix replace + mutate semantics;
  - calls new helper `fetch_bead_labels` (GET
    `/api/v1/projects/{project}/beads/{bead_id}` → `data.labels`),
    union-adds, set-removes, and submits the resulting array via
    the existing PATCH path so server semantics remain unchanged
    (no daemon work needed for this slice).
- `format_bead_detail` now renders a `labels: a, b, c` row in
  `bd show`, suppressed when empty. Previously labels were stored
  but invisible in CLI output unless you passed `--json`.
- `dispatch_bd_update` no-op error message updated to mention the
  new flags.
- `bd update` dispatch site updated to pass `parsed.multi_flags`.
- 2 new unit tests:
  - `bd_update_args_include_add_and_remove_label` — surface
    contract ensures the spec exposes both flags (so MCP tooling
    picks them up automatically).
  - `parse_bd_update_with_repeatable_label_mutations` — verifies
    `parse_command_path` collects every occurrence of `--add-label`
    / `--remove-label` into the multi-flag map.

## Before state

- `caco bd update --labels` only supported full-replace; no way to
  add or remove a single label without listing the full current set
  (and racing with concurrent updates).
- `caco bd show` did not display labels in human output, so
  operators couldn't see what tags a bead carried without `--json`.

## After state

- `caco bd update --bead-id bd-XXXX --add-label supervision
  --add-label P0-cluster --remove-label stale` adds two labels,
  removes one, and preserves every other label that was already
  set. Repeatable flags compose naturally.
- `caco bd show bd-XXXX` now renders `labels: foo, bar` when
  present.
- Existing `--labels` flag and `bd list --label <name>` filter are
  unchanged.

## Notes / verification

- `cargo test -p caco-cli --lib bd_update` → 5/5 green (including
  pre-existing `bd_update_rejects_no_field_flags`).
- `cargo test-small` 51 green.
- 87 pre-existing failures in `cargo test -p caco-cli --lib` were
  confirmed to occur on clean origin/main as well (verified via
  `git stash` round-trip); not introduced by this change. Some are
  the broken-on-main env-isolation cluster (msd-3's bd-51859d).

## Out of scope

- Label hierarchy (`parent:child` implications, e.g.
  `broken-on-main` ⇒ `tests`) — left for a follow-up if anyone
  starts using it.
- Label-aware dispatch heuristics ("don't dispatch two
  `supervision` beads to the same node") — separate scheduler bead.
- TUI / web / android filtering UIs — separate viewer beads.

## Operator-takeaway

Incremental label mutation now ergonomic from the CLI:
`caco bd update --bead-id bd-XXXX --add-label supervision
--remove-label stale`. Labels are visible in `bd show`. Replace-mode
(`--labels`) and mutate-mode (`--add-label`/`--remove-label`) are
mutually exclusive to avoid silent partial-applies. Server semantics
unchanged — only the CLI grew the new mode.
