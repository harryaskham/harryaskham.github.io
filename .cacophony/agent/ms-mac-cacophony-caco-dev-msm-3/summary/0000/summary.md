# Session summary — outbox retry missing-id wording

## Goal

This session fixed the remaining outbox missing-id surface discovered immediately after `bd-b0fadd`: `caco outbox retry` still emitted old usage-style text when `--id` was omitted.

## Bead(s)

- `bd-ee8e16` — [CLI polish] outbox retry missing-id discoverability wording

## Before state

- Failing tests: no regression covered `caco outbox retry` with no `--id`.
- Relevant metrics: `caco outbox retry` returned `usage: caco outbox retry --id <entry_id>`.
- Context: `caco outbox show` and other show surfaces had just been aligned to required-argument wording with list pointers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli show_missing_arguments_use_discoverability_pointers --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; targeted regression is rerun after replay.
- Context: `caco outbox retry` now tells operators `--id` is required and points to `caco outbox list` for queued entries.

## Diff summary

- Commits: `eda95d90b`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: extended `show_missing_arguments_use_discoverability_pointers` to cover `caco outbox retry`.
- Behavioural delta: retry missing-id errors now match the canonical outbox show wording shape.

## Operator-takeaway

The outbox command family now has consistent, discoverable missing-id guidance for both show and retry paths, reducing another CLI dead-end.
