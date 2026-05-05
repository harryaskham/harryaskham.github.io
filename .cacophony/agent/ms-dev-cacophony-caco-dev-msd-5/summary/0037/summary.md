# Session summary — no-ready claim diagnostics

## Goal

Improve the operator and worker experience when no-ID `caco bd claim` refuses to auto-claim even though ordinary open-board views still show work. The goal was to make the refusal explain that visible beads can be intentionally non-dispatchable instead of looking like a contradictory queue state.

## Bead(s)

- `bd-f7f688` — Clarify caco bd claim no-ready diagnostics when open beads are non-dispatchable

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: `caco bd claim --project cacophony --json` returned only `no_ready_beads` / “No ready beads available to claim” while `caco bd list --status open` showed open beads that were assigned or operator-action/manual handoffs.
- Context: The terse error made generic auto-claim look inconsistent with open-board listings and did not point callers at the canonical ready-list view.

## After state

- Failing tests: none from targeted validation.
- Relevant metrics: targeted test filter `cargo test -p caco-daemon beads_claim_no_ready -- --nocapture` passed 2 daemon integration tests.
- Context: The daemon now returns a hint and structured exclusion counts for `no_ready_beads`, including assigned, blocked, dispatch-queued, operator-action, epic, permanent-tracker, and non-generic-ready-like counts.

## Diff summary

- Commits: `0e90e13ad`
- Files touched: `crates/caco-daemon/src/beads.rs`, `crates/caco-daemon/tests/daemon.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: added 1 daemon integration regression for open-but-operator-action no-ready diagnostics; updated the empty-store no-ready test to assert the hint/details.
- Behavioural delta: no-ID claim still returns 404/no_ready_beads when nothing is dispatchable, but the error now tells callers to run `caco bd list --ready` and explains why open/permanent beads may be excluded.

## Operator-takeaway

When generic auto-claim says there is no work, operators and workers now get a first-party diagnostic path instead of having to infer why open beads were skipped. This should reduce stuck-worker confusion during high-load sweeps without changing claim eligibility.
