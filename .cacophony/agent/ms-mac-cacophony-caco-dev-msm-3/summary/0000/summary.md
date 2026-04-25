# Session summary — test/build job missing-id wording

## Goal

This session aligned the queued job inspection/cancel commands so missing `--id` errors point operators to the matching list command.

## Bead(s)

- `bd-a9908e` — [CLI polish] test/build job commands missing-id discoverability wording

## Before state

- Failing tests: no exact regression covered missing `--id` for `caco test show/logs/cancel` or `caco build show/logs/cancel`.
- Relevant metrics: these commands emitted bare `--id is required ...` errors.
- Context: recent CLI polish work established list pointers for discoverable missing identifiers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli test_and_build_job_commands_missing_id_use_list_pointers --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: test job commands now point to `caco test list`, and build job commands point to `caco build list`.

## Diff summary

- Commits: `407350c9b`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `test_and_build_job_commands_missing_id_use_list_pointers`.
- Behavioural delta: six job-related command surfaces now guide users to discover valid job IDs.

## Operator-takeaway

Missing job IDs on test/build show, logs, and cancel now lead users to the appropriate list command instead of a dead-end required-argument error.
