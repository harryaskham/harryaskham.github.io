# Session summary — bd-8a2f03 align changelog help-json test with bd-02c5f7

## Goal

Retire the last remaining caco-cli test-regression I filed during
bd-b7a3c5 triage: `changelog_branch_root_renders_help_json` expected
the bd-2wn help-json shape, but bd-02c5f7 landed over the top of it
to emit a structured no_subcommand error envelope for every bare
branch-root `--json` invocation (scripts expect the standard error
contract).

## Bead(s)

- `bd-8a2f03` — [broken-on-main] changelog_branch_root_renders_help_json

## Before state

Test asserted `doc["command"] == "caco changelog"` and a help-shape
`subcommands` array, but current runtime emits
`{ok:false, error:{code:"no_subcommand", message, available:[...]}}`
so the test panicked with `left: Null, right: "caco changelog"`.

## After state

Test now asserts:
- exit code 2
- `ok == false`
- `error.code == "no_subcommand"`
- `available` array includes `"show"`
- `message` mentions the branch and its subcommands

All other `*_branch_root_renders_help_json` tests were already
adapted; the changelog one was missed because it was written before
bd-02c5f7 introduced the error envelope.

## Diff summary

- Commit: `7fe2754b8 bd-8a2f03: align changelog_branch_root_renders_help_json`
- Files touched: `crates/caco-cli/src/lib.rs` (+31 / -6)
- Production code unchanged.

## Operator-takeaway

Test realignment with a contract that already landed. No
behavioural change; the CLI has been emitting the no_subcommand
envelope correctly for weeks. Pattern: when a later bead narrows a
contract, grep every test asserting the prior shape and align them
in the same PR. The next agent hitting the same suite benefits.
