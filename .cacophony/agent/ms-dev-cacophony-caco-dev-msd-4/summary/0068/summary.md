# Session summary — no-tool-activity stale detector is advisory

## Goal

Stop the daemon from treating lack of recent tool calls as proof that a live agent is dead. The operator directive was explicit: while tmux/runtime liveness is positive, no-tool-activity and startup-readiness detectors should warn only and must not fail, restart, recreate, discard, roll back, or reassign an agent.

## Bead(s)

- `bd-800fe3` — P0: make no-tool-activity and startup-readiness stale detectors advisory only

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: health/router evidence had recently shown startup-readiness warnings but no confirmed dead sessions; the stale detector itself was clear at the latest pass, but the code still transitioned one-off live-tmux idle agents to `Stalled` after timeout + grace.
- Context: startup readiness with live tmux was already warning-only via bd-5e336a; the remaining unsafe mutation path was the no-tool-activity stale transition for Running agents with confirmed-live tmux sessions.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: live-tmux no-tool-activity candidates now remain `Running`; after threshold + grace the daemon records an advisory `last_error` explaining that the live tmux was preserved and no stale transition occurs without positive death evidence.
- Context: dead tmux/session/process evidence remains handled by the existing liveness passes. Fleet-wide stale-drift batches still refresh activity in bulk, but individual no-tool-activity observations now use advisory telemetry.

## Diff summary

- Commits: `765c9a334`.
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-daemon/src/agent/types.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`.
- Tests: updated stale-threshold tests to assert live tmux remains Running with advisory diagnostics; updated runtime repair telemetry expectations to `no_tool_activity_advisory`.
- Behavioural delta: no-tool-activity after stale threshold + grace no longer transitions a live agent to `Stalled` or returns it in `reconcile.stale`; it records an advisory warning and keeps the pane live/Running.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-daemon reconcile_default_stale_timeout_has_transition_grace --lib`; `cargo test -p caco-daemon reconcile_default_stale_timeout_warns_after_transition_grace_bd_800fe3 --lib`; `cargo test -p caco-daemon reconcile_uses_per_agent_stale_timeout --lib`; `cargo test -p caco-daemon runtime_repair_records_bd_e91436_stale_drift_events --lib`; `cargo check -p caco-daemon`; `git diff --check`.

## Operator-takeaway

The daemon should now stop converting silent-but-live workers into stale/stalled ownership churn solely because they have not emitted tool activity recently. It will still surface the condition for operators, but destructive recovery requires stronger evidence than silence.
