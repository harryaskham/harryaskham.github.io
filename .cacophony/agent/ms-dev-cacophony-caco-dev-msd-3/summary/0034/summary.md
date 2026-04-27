# Session summary — msg send @target recipient preservation

## Goal

Fix `caco msg send --target @...` after the newly-landed global `@target` resolver started treating message recipient values as remote-dispatch targets, causing the recipient sent to the daemon to become the string `true` rather than the intended agent address.

## Bead(s)

- `bd-42d036` — `caco msg send --target '@<exact-unique>'` resolves to literal `true` and `--json` errors are not enveloped.

## Before state

- Failing tests: no focused coverage for `--target @agent:...` or bare `@agent-id` in `msg send` recipient position.
- Relevant metrics: focused pre-change repro from the bead showed `--target '@helsinki-cacophony-test-user-hel'` failing with `--target 'true' is not a valid direct-message recipient`.
- Context: global suffix/prefix `@target` extraction was intended for remote command dispatch, but it was running before command parsing and consumed any `@...` token, including values for `msg send --target`.

## After state

- Failing tests: none observed in the validation run.
- Relevant metrics: 3 focused `bd-42d036` tests pass; `cargo clippy -p caco-cli --all-targets -- -D warnings` passes; `cargo test-small` passes.
- Context: `extract_at_target` now skips `@...` tokens that are syntactically a value for the preceding long flag, while preserving suffix remote-dispatch after known boolean globals such as `--json`.

## Diff summary

- Commits: `193f8e24c` (`bd-42d036: preserve msg send at-target recipients`).
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/msg_cmd.rs`.
- Tests: +3 focused tests for message-recipient `@...` handling and suffix `@target` preservation.
- Behavioural delta: `msg send --target @agent:<id>` and bare `--target @<unique-agent-id>` are kept as message recipient values and resolved through the message target alias path, while invalid daemon rejections in `--json` mode now return the daemon JSON envelope with a non-zero exit code.

## Operator-takeaway

The global `@target` feature now coexists with `msg send` recipient aliases instead of stealing recipient values before the message command can parse them. Exact `@agent` recipients can be used for direct messages again, and machine callers get structured JSON failures instead of stderr-only text on daemon rejections.
