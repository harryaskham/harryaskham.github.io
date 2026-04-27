# Session summary — PR backend direct branch recurrence guard

## Goal

Respond to Harry's repeated P0 escalation around agents being able to commit work into projects by turning the remaining draft recurrence evidence into active work and locking down the exact PR-backend branch-selection case that caused recorded direct attempts to publish to `fork/main` before PR creation.

## Bead(s)

- `bd-b8470c` — [reintegration] recorded direct path still fails after bd-95cda5 closure

## Before state

- Failing tests: none known for this targeted path; the recurrence evidence was historical technical-writer `direct,recorded` failures where PR-backend direct intent published code to `fork/main` and then failed with no PR URL.
- Relevant metrics: open queue was empty until `bd-b8470c` was promoted from draft P1 evidence to active P0 by operator escalation; `bd-9e4be4` was already closed on current main.
- Context: current main already contained the broader bd-9e4be4 reintegration redesign and a regression for `origin/main`, but the checked test did not explicitly cover the configured `reintegrate_target: fork/main` shape from the recurrence.

## After state

- Failing tests: none in the targeted validation.
- Relevant metrics: `cargo test -p caco-cli pr_backend_direct_uses_agent_pr_branch_even_when_target_remote_is_writable_bd_95cda5 -- --nocapture` passes: 1 passed, 0 failed.
- Context: the PR-backend direct branch resolver now has a regression assertion for `reintegrate_target: fork/main` + `pr_base: upstream/main`, proving it publishes to the agent PR branch (`agent/node/example/agent-123/pr`) instead of `fork/main`.

## Diff summary

- Commits: `b5cad8fec` (code/test regression) plus this summary commit
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: extended 1 existing unit test / removed 0 / flipped 0
- Behavioural delta: no production logic changed; the existing fixed branch-selection behavior is now covered for the exact `fork/main` recurrence topology so the P0 cannot silently regress.

## Operator-takeaway

The remaining P0 was not a missing production-code fix on current main; it was an untracked recurrence gap. I promoted and claimed it, then added concrete regression coverage for the fork-main PR-backend shape that previously stranded technical-writer work before PR creation.
