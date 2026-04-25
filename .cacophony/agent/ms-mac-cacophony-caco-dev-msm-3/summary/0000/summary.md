# Session summary — release cancel missing-id wording

## Goal

This session aligned `caco release cancel` with the rest of the release job inspection family by adding a discoverability pointer when `--id` is missing.

## Bead(s)

- `bd-6c1c52` — [CLI polish] release cancel missing-id discoverability wording

## Before state

- Failing tests: no exact regression covered missing `--id` for `caco release cancel`.
- Relevant metrics: release status/logs already pointed to `caco release list`, while release cancel emitted a bare `--id is required for release cancel` error.
- Context: this was a narrow sibling consistency miss found during CLI polish burn-down.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli release_cancel_missing_id_points_to_release_list --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: release cancel missing-id now guides operators to `caco release list` to find queued/active jobs.

## Diff summary

- Commits: `a930ea557`
- Files touched: `crates/caco-cli/src/release_cmd.rs`, `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `release_cancel_missing_id_points_to_release_list`.
- Behavioural delta: `caco release cancel` missing-id error now has the same release-list pointer as status/logs.

## Operator-takeaway

The release cancellation path no longer dead-ends on missing job IDs; it now tells operators how to discover the relevant release job.
