# Session summary — msg broadcast/reply validation

## Goal

Close the sister validation gap reported after `bd-00102d`: `caco msg broadcast --body ''` and `caco msg reply --body ''` should fail before mutating state, and msg write surfaces should refuse unknown flags instead of warning and proceeding.

## Bead(s)

- `bd-dfe473` — Reject empty msg broadcast/reply bodies

## Before state

- Failing tests: the user/test disclosure reported an accidental empty broadcast created by `caco msg broadcast --body ''`.
- Relevant metrics: `bd-00102d` had already made `msg send` reject empty bodies and invalid targets, but broadcast/reply still needed parity.
- Context: `caco msg snapshot --agent ''` already had a validator, and `msg reply --message-id bogus` already preflighted parent message lookup; the remaining state-mutating drift was empty body handling plus non-strict unknown-flag metadata for msg write commands.

## After state

- Failing tests: none observed.
- Relevant metrics: focused daemon broadcast validation test passed; focused CLI body-validator and non-idempotent command-spec tests passed; `cargo clippy -p caco-cli -p caco-daemon --all-targets -- -D warnings` passed; `cargo test-small` passed.
- Context: CLI and daemon now reject blank broadcast bodies before persistence or fan-out; CLI reply rejects blank bodies and empty message IDs before lookup/send; msg send/broadcast/speak/reply are marked non-idempotent so unknown flags are refused by the bd-4c8fdd guard before state mutation.

## Diff summary

- Commits: `ce3f278f7` (code), plus this recorded-summary commit
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/msg_cmd.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: +1 daemon regression test for empty project/global broadcast bodies; +1 CLI unit test for shared body validation; extended non-idempotent command-spec test to include msg write surfaces.
- Behavioural delta: empty msg broadcasts/replies now fail with explicit `--body must not be empty...` messages, and unknown flags on msg write commands no longer warn-and-mutate.

## Operator-takeaway

The accidental empty broadcast class is now blocked at both the CLI and daemon layers, and the broader msg write namespace is safer against typoed state-mutating invocations.
