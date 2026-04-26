# Session summary — persistent idle recovery is advisory

## Goal

Finish the P0 hardening so the lifecycle supervisor/daemon no longer restarts or mutates live agents solely because they are idle or not emitting tool activity. This bead focused on the persistent-agent idle auto-restart path after `bd-800fe3` made live-tmux stale detection advisory.

## Bead(s)

- `bd-db327a` — P0: disable systemd lifecycle supervisor auto-kill and auto-restart of live agents

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: persistent agents over the configured idle timeout could still be selected by the daemon and sent through `state.agents.restart(...)` despite live state; this conflicted with the operator directive that silence is not positive death evidence.
- Context: related work had already restored Helsinki's native supervisor and made no-tool-activity stale detection advisory while live tmux remained positive.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `auto_restart_idle_persistent_agents` now emits warning-level `persistent_idle_advisory` events with `auto_restart_suppressed=true` and `requires_positive_death_evidence=true`, and no longer calls restart for live idle candidates.
- Context: explicit positive-death recovery paths still exist for dead tmux/session/runtime evidence; this only removes idle/no-tool-age as an automatic destructive trigger.

## Diff summary

- Commits: source branch commit `18e082b7c` after rebase; reintegration will squash this into a mainline commit.
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`.
- Tests: added a source-level regression test asserting the persistent idle handler emits advisory metadata and does not invoke the restart path.
- Behavioural delta: persistent idle timeout now reports advisory state instead of automatically recycling the agent runtime.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-daemon persistent_idle_handler_is_advisory_only_bd_db327a --lib`; `cargo test -p caco-daemon reconcile_default_stale_timeout_warns_after_transition_grace_bd_800fe3 --lib`; `cargo test -p caco-daemon reconcile_uses_per_agent_stale_timeout --lib`; `cargo check -p caco-daemon`; `git diff --check`.

## Operator-takeaway

The remaining automatic live-agent restart path based only on idle/no-tool age is now warning-only. Recovery that mutates a live persistent runtime should require either real death evidence or explicit operator/controller action.
