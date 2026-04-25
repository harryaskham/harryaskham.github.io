# Session summary — notify get not-found wording

## Goal

This session aligned `caco notify get` not-found wording with the rest of the notify CLI polish sweep so operators who provide a bogus notification ID are pointed back to the list surface.

## Bead(s)

- `bd-67ebfd` — [CLI polish] notify get not-found discoverability wording

## Before state

- Failing tests: no regression covered non-JSON `notify get` daemon-side not-found wording.
- Relevant metrics: missing and empty notification IDs already pointed to `caco notify list`, but bogus IDs returned a dead-end lookup failure.
- Context: this was filed in collab-mode after the normal implementation queue drained and after completing the notify-ack addendum.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli notify_get_not_found_errors_point_to_notify_list --lib` and `cargo check -p caco-cli --lib` passed before replay; the focused regression is rerun after replay.
- Context: `notify get` daemon-side lookup failures now append the same `caco notify list` discovery pointer.

## Diff summary

- Commits: `4670ef89c`
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/outbox_cmd.rs`
- Tests: added exact regression `notify_get_not_found_errors_point_to_notify_list`.
- Behavioural delta: bogus notify-get IDs now produce actionable next-step guidance.

## Operator-takeaway

The notify command family now has consistent list-pointer guidance for missing, empty, and not-found notification IDs across get and ack surfaces.
