# Session summary — project-controller Pi APPEND_SYSTEM guard

## Goal

Complete `bd-28859e` by tightening the managed Pi APPEND_SYSTEM guard for project-controller profiles so role-neutral persistent/controller lifecycle text remains explicit and worker completion boilerplate cannot regress into controller prompts.

## Bead(s)

- `bd-28859e` — Profile audit: controller Pi APPEND_SYSTEM has worker completion lifecycle.

## Before state

- Failing tests: none known for this guardrail.
- Relevant metrics: existing `managed_pi_append_system_md` already selected the persistent/controller APPEND_SYSTEM block for non-worker authorization scopes, but tests covered router/cluster-controller and persistent env rather than `project_controller` explicitly.
- Context: the bead came from a profile audit where a reified controller Pi `APPEND_SYSTEM.md` appeared to contain worker-style completion guidance, conflicting with project-controller lifecycle rules.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: queued `tj-b2e22236` passed `cargo test -p caco-daemon managed_pi_append_system_uses_persistent_lifecycle_for_project_controller_scope_bd_28859e -- --nocapture`.
- Context: the persistent/controller APPEND_SYSTEM wording now explicitly mentions project-controller roles, and a regression test asserts `project_controller` scope does not receive assigned-bead / one-shot-worker `caco agent complete` lifecycle text.

## Diff summary

- Code/content commits: `842af6302d`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`.
- Tests: +1 daemon unit test; no tests removed or flipped.
- Behavioural delta: project-controller-scoped managed Pi APPEND_SYSTEM materialization is now explicitly guarded by test and prompt copy against worker completion lifecycle text.

## Operator-takeaway

This was a small hardening slice: current role selection was already mostly correct, and the landed change makes the project-controller case explicit so future prompt/materialization changes do not accidentally reintroduce worker completion instructions.
