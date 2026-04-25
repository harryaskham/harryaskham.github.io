# Session summary — scratch connect/disconnect argument guidance

## Goal

Improve `caco scratch connect` and `caco scratch disconnect` error messages when `--note-id` is present but `--scope` or `--target` is missing.

## Bead(s)

- `bd-c9d121` — [CLI polish] scratch connect/disconnect missing-link-argument wording

## Before state

- Missing `--note-id` already pointed to `caco scratch list` correctly.
- Missing `--scope` after `--note-id` emitted terse wording: `--scope is required for scratch connect (agent, node, project)`.
- Missing `--target` emitted only `--target is required for scratch connect` / disconnect.

## After state

- Missing `--scope` now says the expected vocabulary is `agent`, `node`, or `project` and gives a concrete example command.
- Missing `--target` now explains the target is the agent ID, node name, or project name selected by `--scope`, again with a concrete example.
- Existing missing-`--note-id` list guidance is unchanged.

## Diff summary

- Commit: `5399a2d3b`
- Files touched: `crates/caco-cli/src/scratch_cmd.rs`, `crates/caco-cli/src/lib.rs`
- Tests: +1 regression covering connect/disconnect missing `--scope` and `--target`.
- Validation: `timeout 120 cargo test -p caco-cli --lib scratch_connect_disconnect_missing_scope_or_target_are_actionable_bd_c9d121 -- --nocapture`; `timeout 120 cargo test-small`.

## Operator-takeaway

This is small CLI UX polish: once an operator supplies the note ID, the next missing argument error now tells them exactly what kind of scope and target are expected instead of forcing them back to help output.
