# Session summary — outbox drop missing-id wording

## Goal

This session fixed the final outbox missing-id surface found during the CLI-polish burn-down: `caco outbox drop` still emitted old usage-style text when `--id` was omitted.

## Bead(s)

- `bd-413fa7` — [CLI polish] outbox drop missing-id discoverability wording

## Before state

- Failing tests: no regression covered `caco outbox drop` with no `--id`.
- Relevant metrics: `caco outbox drop` returned `usage: caco outbox drop --id <entry_id>`.
- Context: `caco outbox show` and `caco outbox retry` had just been aligned to required-argument wording with `caco outbox list` pointers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli show_missing_arguments_use_discoverability_pointers --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; targeted regression is rerun after replay.
- Context: `caco outbox drop` now tells operators `--id` is required and points to `caco outbox list` for queued entries.

## Diff summary

- Commits: `c1c54a46e`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: extended `show_missing_arguments_use_discoverability_pointers` to cover `caco outbox drop`.
- Behavioural delta: drop missing-id errors now match the canonical outbox show/retry wording shape.

## Operator-takeaway

The outbox show, retry, and drop commands now consistently guide users from missing IDs to `caco outbox list`, eliminating another small CLI dead-end.
