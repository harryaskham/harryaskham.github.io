# Session summary — scratch note-id missing wording

## Goal

This session aligned scratch note-targeted commands with `caco scratch show` by pointing missing `--note-id` errors at `caco scratch list`.

## Bead(s)

- `bd-d8b77b` — [CLI polish] scratch note commands missing-note-id discoverability wording

## Before state

- Failing tests: no exact regression covered missing `--note-id` for scratch write, append, connect, or disconnect.
- Relevant metrics: `scratch show` already pointed to `caco scratch list`, while the four sibling commands emitted bare missing-note-id errors.
- Context: this was a narrow CLI polish slice taken while the normal implementation queue contained only permanent trackers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli scratch_note_commands_missing_note_id_point_to_scratch_list --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: scratch write/append/connect/disconnect missing-note-id errors now guide operators to `caco scratch list`.

## Diff summary

- Commits: `28fc40604`
- Files touched: `crates/caco-cli/src/scratch_cmd.rs`, `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `scratch_note_commands_missing_note_id_point_to_scratch_list`.
- Behavioural delta: four scratch note-targeted commands now include a discoverability pointer for missing note IDs.

## Operator-takeaway

Scratch note workflows now consistently tell users how to discover note IDs instead of stopping at a bare required-argument error.
