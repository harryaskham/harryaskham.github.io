# Session summary — skip operator-action beads in auto-claim

## Goal

Implement and document `bd-7f6c9b` so generic no-ID auto-claim no longer assigns idle workers to operator/manual handoff beads such as signing-secret requests. The intent was to keep the ready queue safe for workers while preserving deliberate explicit claims by operators, controllers, or specialists.

## Bead(s)

- `bd-7f6c9b` — Skip operator-action beads in generic auto-claim

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: `caco bd claim --project` could select ready beads marked as operator action, including manual signing/provisioning requests.
- Context: Helsinki had just recovered from a restart-loop incident; I avoided live-node remediation and chose a bounded code/policy fix in the local checkout.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: current `origin/main` source now has `Bead::requires_explicit_assignment` and `claim_next_ready` skips `operator-action` labels / `[operator-action]` title prefixes, while explicit `claim_bead(<id>)` remains available.
- Context: while rebasing, equivalent source implementation had already landed on main, so this branch preserves that code and lands the missing docs/profile contract alignment in `SPEC.md`, `README.md`, `AGENTS.md`, and the `auto-claim` profile.

## Diff summary

- Commits: `b42cd63f5`, `96eaf18c1`
- Files touched by this branch after rebase: `SPEC.md`, `README.md`, `AGENTS.md`, `.cacophony/profiles/auto-claim.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0052/summary.md`
- Source implementation present on current main: `crates/caco-beads/src/model.rs`, `crates/caco-beads/src/store.rs`
- Tests: current source includes the `claim_next_ready_skips_operator_action_beads` unit test; no tests removed or disabled.
- Behavioural delta: no-ID auto-claim and spawn-and-claim paths backed by `BeadsStore::claim_next_ready` skip operator-action beads, but explicit ID-based claims remain allowed for deliberate manual ownership.
- Validation: `cargo fmt --all -- --check`; pre-rebase `caco test run --wait true --command "cargo test -p caco-beads claim_next_ready_skips_operator_action_beads"` (`tj-c8f8902b`), `caco test run --wait true --command "cargo test -p caco-beads claim_next_ready"` (`tj-49b99e57`), and `caco build run --wait true --command "cargo clippy -p caco-beads --all-targets -- -D warnings"` (`bj-c327caf9`); post-rebase `caco test run --wait true --command "cargo test -p caco-beads claim_next_ready_skips_operator_action_beads"` (`tj-165dd98a`) and `caco test run --wait true --command "cargo test -p caco-beads claim_next_ready"` (`tj-84c1c9a9`).

## Operator-takeaway

Generic workers should no longer briefly claim secret/provisioning/operator-action tasks from the ready queue; those beads remain visible and deliberately claimable by ID when the right human or specialist owns the manual step.
