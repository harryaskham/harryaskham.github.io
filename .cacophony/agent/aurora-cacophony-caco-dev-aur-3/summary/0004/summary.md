# Session summary — zombie-agent detection canary (bd-dfefb0 slice 1)

## Goal

Address Harry's operator-reported bug: the daemon reports agents `running` when their tmux server and process are actually dead (zombies he couldn't attach to). The bead asks for liveness reconciliation + an audit surface so operators don't hand-audit. This slice lands the safe detection foundation; the riskier reconcile fix is split out.

## Bead(s)

- `bd-dfefb0` — Daemon reports agents 'running' when tmux/process are dead; needs liveness reconciliation + audit surface (this slice: detection canary)
- `bd-ec1c23` — (filed) Reconcile loop leaves persistent-agent running zombies (restart-grace suppression gap) — the risky reconcile auto-recovery piece, split out for careful review

## Before state

- No first-party detection for the "non-terminal agent record but no live tmux/process" zombie class. The deep-doctor stale-state canary framework (built incrementally by msm-1/msm-3 as bd-972746/bd-ff2ec6) covered orphan tmux sessions and agent-dirs-without-records, but not the inverse zombie case.
- Operator had to hand-audit across nodes (caco ssh <node> tmux check vs daemon running set) to find 3 ms-mac zombies.

## After state

- New `DeepDoctorStaleStateKind::ZombieRunningAgent` + pure `sample_deep_doctor_zombie_running_agents` sampler in `crates/caco-daemon/src/doctor_deep.rs`, mirroring the existing canary pattern (bounded sampling, severity/hint plumbing).
- Positive-death-evidence gate: only flags agents that are observable (not remote/unreachable), non-terminal, with NO live tmux AND NO live process — matching the reconciler's "never condemn without positive death evidence" invariant (AGENTS.md). Safe-repair hint suggests `caco agent resume --id <id>` or marking failed.
- Queued validation: `cargo test -p caco-daemon --lib doctor_deep::tests::deep_doctor_zombie_running_agent` = 2 passed / 0 failed.

## Diff summary

- Code commit(s): pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-daemon/src/doctor_deep.rs` (+207, pure additive; no pre-existing-drift churn).
- Tests: +2 (detection + bounded-truncation).
- Behavioural delta: additive detection primitive; no reconcile/lifecycle mutation in this slice.

## Operator-takeaway

The canonical zombie-agent detection logic now exists in the deep-doctor framework, ready to be surfaced once that framework is wired to a live `caco doctor` command (the framework is still detection-only / not yet command-wired — that wiring is the remaining audit step on bd-dfefb0). The actual reconcile auto-recovery (so zombies self-heal instead of needing `caco agent resume`) is intentionally split to bd-ec1c23 because it touches the sensitive restart-grace/idle-restart path where a wrong change could mass-fail live workers — that piece needs careful review with the lifecycle owners.
