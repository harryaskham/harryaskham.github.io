# Session summary — CLI show missing-argument discoverability

## Goal

This session fixed the `bd-c3fcdd` addendum by aligning `caco outbox show` and sister show surfaces with canonical missing-required-argument wording plus list/discoverability pointers.

## Bead(s)

- `bd-b0fadd` — [CLI polish] outbox show and sister missing-arg discoverability wording

## Before state

- Failing tests: no exact regression covered the outbox/profile/choices/bd show no-argument wording cohort.
- Relevant metrics: `caco outbox show` emitted old usage-style text; `caco bd show`, `caco profile show`, and `caco choices show` used required-argument wording but lacked a list pointer.
- Context: `bd-c3fcdd` had already fixed the same pattern for `caco node show`.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli show_missing_arguments_use_discoverability_pointers --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; targeted regression is rerun after replay.
- Context: all four surfaces now point operators to the relevant list command when a required show identifier is missing.

## Diff summary

- Commits: `bcd1f947f`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact CLI regression `show_missing_arguments_use_discoverability_pointers`.
- Behavioural delta: `outbox show`, `bd show`, `profile show`, and `choices show` now share canonical missing-argument plus discoverability guidance.

## Operator-takeaway

The CLI no longer dead-ends users with bare usage or sparse required-argument errors on these show commands; every missing-id case tells them which list command can discover valid targets.
