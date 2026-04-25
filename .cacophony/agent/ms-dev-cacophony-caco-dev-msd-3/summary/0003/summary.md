# Session summary — msg send validation

## Goal

Fix the `caco msg send` validation gap so direct-message sends cannot silently create garbage messages for empty targets, malformed targets, or empty bodies.

## Bead(s)

- `bd-00102d` — caco msg send accepts empty target, bogus target, and empty body

## Before state

- Failing tests: no pre-existing failure observed; the bead included repro commands showing exit-0 sends for invalid inputs.
- Relevant metrics: `caco msg speak --body ''` already had a body validator, but `POST /messages/send` accepted empty strings and one-token malformed targets.
- Context: invalid direct-message requests could persist rows with unrecoverable or non-deliverable targets, producing `unknown`/phantom send output at the CLI layer.

## After state

- Failing tests: none observed.
- Relevant metrics: focused `cargo test -p caco-daemon msg_send_ -- --nocapture` passed; `cargo clippy -p caco-daemon --all-targets -- -D warnings` passed; `cargo test-small` passed.
- Context: daemon-side request validation now rejects empty bodies, empty targets, and malformed direct-message recipients before persistence or tmux injection.

## Diff summary

- Commits: `ea30f42ff` (code), plus this recorded-summary commit
- Files touched: `crates/caco-daemon/src/lib.rs`
- Tests: +1 daemon regression test covering empty target, malformed target, and empty body rejection.
- Behavioural delta: `caco msg send` requests now require a non-empty body and a recipient shaped as `<project>:<agent-id>` or `<node>:<project>:<agent-id>`, returning HTTP 400/CLI failure instead of recording garbage messages.

## Operator-takeaway

The message-send path now fails before state mutation for the three destructive invalid-input cases from the test-user report, matching the neighbouring `msg speak` guardrail style.
