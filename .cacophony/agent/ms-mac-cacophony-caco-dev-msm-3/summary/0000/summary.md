# Session summary — notify ack missing-id wording

## Goal

This session aligned `caco notify ack` with the notification-family discoverability pattern by pointing missing `--id` errors at `caco notify list`.

## Bead(s)

- `bd-d8a781` — [CLI polish] notify ack missing-id discoverability wording

## Before state

- Failing tests: no exact regression covered missing `--id` for `caco notify ack`.
- Relevant metrics: `notify get` already pointed to `caco notify list`, while `notify ack` emitted a bare `--id is required for notify ack` error.
- Context: this was a narrow sibling CLI polish miss found while the normal implementation queue was empty.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli notify_ack_missing_id_points_to_notify_list --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: notify ack missing-id now guides operators to recent notifications via `caco notify list`.

## Diff summary

- Commits: `565d0de81`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `notify_ack_missing_id_points_to_notify_list`.
- Behavioural delta: `caco notify ack` missing-id errors now include a discoverability pointer.

## Operator-takeaway

Notification acknowledgement no longer dead-ends when the notification ID is omitted; users are pointed to the list surface that shows valid IDs.
