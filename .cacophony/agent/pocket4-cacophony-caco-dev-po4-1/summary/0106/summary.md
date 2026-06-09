# Session summary — retroactive socket-dead cleanup (bd-7beb2f)

## Goal

Transition PRE-EXISTING agents already stuck in Retrying with a tmux_socket_dead resume blocker (which bd-be60bb's new-edge sweep never sees), so they stop being reconcile-resume candidates that wedge startup agent_reconciliation (bd-b8af56).

## Bead

- `bd-7beb2f` — Retroactively transition pre-existing tmux_socket_dead retrying agents (follow-up to my bd-be60bb; assigned to me by msm-1)

## Before state

- bd-be60bb transitions NEW socket-deaths on the heartbeat sweep, but `Retrying` is NOT in that sweep's candidate state filter (Running|Waiting|Blocked|Starting), so an agent already stuck Retrying+tmux_socket_dead (e.g. the kind=worker 86r5, 0-byte checkout) is never transitioned and lingers as a reconcile-resume candidate — exactly what can wedge startup agent_reconciliation (bd-b8af56).
- No code-level preserve/do-not-disturb marker exists (bd-074eac is an operator/broadcast directive only).

## After state

- `runtime_sweep::should_retroactively_fail_stuck_socket_dead(is_retrying, resume_blocker_is_socket_dead, is_recovery_eligible_persistent)`: pure decision helper. A `Retrying` row with `resume_blocker==TmuxSocketDead` reliably means the last resume failed/still-blocked (the blocker clears on a successful resume), so the marker is trustworthy without a live re-probe. Only NON-persistent agents transition; declared persistents keep the sentinel route.
- `runtime_sweep::retroactive_socket_dead_cleanup_enabled()`: opt-in gate `CACO_RETROACTIVE_SOCKET_DEAD_CLEANUP` (default OFF).
- `agent/lifecycle.rs` Pass 2b-rt (in the periodic reconcile tick, before the heartbeat sweep): when enabled, finds Retrying + TmuxSocketDead + non-persistent agents, re-checks under the lock, transitions to `Failed`, persists, and pushes to `failed_agents` (so beads are unclaimed).

## Design decision: env-gate (gate-on-bisect) instead of a per-agent preserve marker

msm-1 endorsed either a durable per-agent preserve marker OR gate-on-bisect. I attempted a typed `preserve` bool on `AgentInfo` but reverted it: `AgentInfo` has NO `Default` derive and ~250 explicit struct-literal construction sites, so a new field would require touching all of them (massive churn + high risk). So the exclusion is the **gate-on-bisect** path: the sweep is opt-in (default off), and the operator/doctor enables `CACO_RETROACTIVE_SOCKET_DEAD_CLEANUP` only AFTER any pinned diagnostic subject (the bd-b03101 bisect target 86r5, under the bd-074eac do-not-disturb directive) is resolved. A durable per-agent preserve marker (requiring an `AgentInfo` Default or a builder refactor) is the recommended follow-up so the sweep can run while surgically protecting a single subject.

## Diff summary

- Code commit: `35ffa6fbd` (final landed squash SHA from the reintegration receipt).
- Files: `crates/caco-daemon/src/agent/runtime_sweep.rs` (helper + gate + 3 tests), `crates/caco-daemon/src/agent/lifecycle.rs` (Pass 2b-rt).
- Validation: `cargo test -p caco-daemon --lib runtime_sweep::tests` (26 pass incl 3 new bd-7beb2f), `clippy -p caco-daemon --lib -D warnings` clean.

## Coordination

- Cleanly separable from msm-1's bd-b8af56 (spawn-admission decouple; does not transition 86r5). I'll flag msm-1 that the exclusion is env-gate/gate-on-bisect (not a marker) and recommend the durable preserve marker follow-up.

## Operator-takeaway

The mechanism to clean up pre-existing stuck Retrying+tmux_socket_dead non-persistent agents is in place but OFF by default. Enable `CACO_RETROACTIVE_SOCKET_DEAD_CLEANUP=1` (then restart) only after the 86r5 / bd-b03101 bisect is resolved, since there is no per-agent preserve marker yet.
