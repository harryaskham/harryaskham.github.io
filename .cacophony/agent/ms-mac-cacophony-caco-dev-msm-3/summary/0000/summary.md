# Session summary — at-all missing-command wording

## Goal

This session polished the `caco @all` empty-command error so the remote-target shortcut follows the same required-command style as the other recently fixed CLI surfaces.

## Bead(s)

- `bd-6cf7eb` — [CLI polish] at-all missing-command discoverability wording

## Before state

- Failing tests: no exact regression covered `caco @all` with no following command.
- Relevant metrics: the command emitted `no command specified after @-target; usage: caco @all <command>`.
- Context: this was the last obvious old usage-style CLI error found by the burn-down grep.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli at_all_missing_command_uses_discoverability_pointer --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: bare `caco @all` now names the missing command and points to `caco --help`.

## Diff summary

- Commits: `54af5ad98`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `at_all_missing_command_uses_discoverability_pointer`.
- Behavioural delta: empty remote-target invocations now provide actionable required-command guidance.

## Operator-takeaway

The obvious old usage-style CLI error sweep is now complete for the surfaces found by grep; `@all` joins exec, foreach, ssh/mosh, scp, and outbox in giving discoverable guidance.
