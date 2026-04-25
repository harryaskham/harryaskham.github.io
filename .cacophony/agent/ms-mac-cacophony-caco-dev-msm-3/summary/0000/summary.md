# Session summary — Fail closed on caco update typos

## Goal

This session fixed a state-mutating CLI safety regression where `caco update bogus` or a typo like `caco update statuus` silently fell through to the default update path instead of reporting an unknown subcommand.

## Bead(s)

- `bd-079021` — caco update bogus silently swallows unknown subcommand and runs default update

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: the bead repro showed `caco update bogus` exited 0 after performing the normal update check.
- Context: sister command families already emitted canonical `unknown subcommand ... Allowed: ...` errors, but `update` had a default action and did not reject extra positional subcommands first.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: targeted regression `update_unknown_subcommand_errors_with_allowed_list` passed; existing `update_status_subcommand_advertises_stable_only_flag` passed; `cargo check -p caco-cli --lib` passed; `cargo test-small` passed with 256 tests before replay.
- Context: the `update` dispatcher now checks for unexpected positionals before entering the network/self-update flow.

## Diff summary

- Commits: `2fb3556b4`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added a regression asserting `caco update statuus` returns `unknown subcommand 'statuus' for 'caco update'. Allowed: status`.
- Behavioural delta: typoed `caco update` subcommands fail closed instead of starting the updater.

## Operator-takeaway

A typo on the sensitive update command can no longer trigger network/self-update behavior. The command now matches the canonical fail-closed UX used by sister subtrees.
