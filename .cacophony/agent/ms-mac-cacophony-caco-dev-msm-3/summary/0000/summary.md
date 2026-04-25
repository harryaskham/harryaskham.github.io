# Session summary — cron show missing-name wording

## Goal

This session aligned `caco cron show` missing-name guidance with the existing `caco cron run` wording so operators get a discoverability pointer instead of a bare required-argument error.

## Bead(s)

- `bd-790015` — [CLI polish] cron show missing-name discoverability wording

## Before state

- Failing tests: no exact regression covered `caco cron show` without `--name`.
- Relevant metrics: `cron show` returned `--name is required for cron show`, while `cron run` already pointed to `caco cron list`.
- Context: this was a small consistency miss found after the old usage-string sweep.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli cron_show_missing_name_uses_discoverability_pointer --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: missing-name cron show now points to `caco cron list` to discover configured cron entries.

## Diff summary

- Commits: `eb4133c25`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `cron_show_missing_name_uses_discoverability_pointer`.
- Behavioural delta: `caco cron show` missing-name errors now match the discoverable cron-family wording.

## Operator-takeaway

The cron inspection path now tells users where to find valid cron names, matching the run path and reducing another CLI dead-end.
