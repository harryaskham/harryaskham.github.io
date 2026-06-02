# Session summary — health watchdog no longer kills a healthy daemon over a single hanging /api/v1/node

## Goal

Fix a P1 daemon-instability bug: the health watchdog repeatedly self-exited a
*functionally healthy* caco-daemon (forcing supervisor respawn in a ~12-minute
restart loop) because the single endpoint it probes, `/api/v1/node`, hung
(code=000, ~10 min) while `/api/v1/agents`, `/api/v1/beads`, and `/api/v1/feed`
all served 200 fast. Operator (Harry) confirmed live; 116 self-exit events on
ms-mac since 2026-05-06. This watchdog-misfire is the mechanism behind the
fleet's daemon restart-loop / node-api intermittency.

## Bead

- `bd-e32dba` (P1 bug, profile senior-dev) — Health watchdog kills healthy
  daemon: /api/v1/node endpoint hangs while agents/beads/feed serve 200.

## Root cause

Two independent contributors, both addressed:

1. **`/api/v1/node` handler hang.** bd-13ae27 had bounded the agent
   `list_all()` and `queue_status()` sub-fetches in `handle_node`, but left
   three other contended sub-fetches **unbounded**:
   - `state.agents.runtime_repair_summary().await` — takes the SAME
     agent-manager inner lock as the bounded `list_all()`, so under
     reconcile/aggregation contention the bounded sections returned degraded
     after 2s and then this await blocked the whole handler indefinitely.
   - `state.bootstrap_state.lock().await` — async mutex, contendable by
     lifecycle/bootstrap convergence.
   - `beads_primary_view_with_maintenance().await` — locks `planned_outages`
     (tokio mutex) which `apply_node_lifecycle_event` holds while awaiting the
     contended store lock during feed/lifecycle churn (the helsinki :12100 flap
     window produces a lot of this). The beads endpoint never touches
     `planned_outages`, which is exactly why it stayed fast while node hung.

2. **Watchdog killed a healthy daemon on a single-endpoint failure.** The
   self-exit decision trusted only `/api/v1/node`.

## Fix

- **Bounded all three remaining contended sub-fetches** in `handle_node` with
  the existing bd-13ae27 `effective_node_probe_section_timeout()` (default
  2000ms), each with a degraded fallback (no runtime-repair section / no
  bootstrap section / maintenance-free routing view) instead of blocking. The
  liveness probe now always returns within bounds even when a lock is contended.
- **Hardened the watchdog**: before self-exiting on `/api/v1/node` failures,
  corroborate with an independent lightweight liveness endpoint
  (`/api/v1/feed?limit=1`). If that answers 200, the listener + async runtime
  are alive (single-handler hang, not a wedged daemon), so we log and keep
  running (resetting failure pressure) instead of forcing a respawn. Genuine
  wedges where both endpoints fail still self-exit. Decision extracted into the
  pure, unit-tested `watchdog_should_self_exit(...)` helper.

## Before / after state

- Before: 116 self-exits since 2026-05-06; 5 in the last hour, looping ~every
  12 min. `/api/v1/node` code=000 timeout while siblings serve 200.
- After: `handle_node` sub-fetches are individually bounded so `/api/v1/node`
  stays responsive under lock contention; even if it does hang, the watchdog
  no longer self-exits a daemon whose `/api/v1/feed` endpoint is alive.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (+180/-12).
- Tests: +1 unit test `watchdog_suppresses_self_exit_when_corroboration_endpoint_alive_bd_e32dba`
  (passing); existing `effective_node_probe_section_timeout_respects_override_bd_13ae27`
  re-verified.
- Validation: `cargo check -p caco-daemon` clean, `cargo clippy -p caco-daemon`
  clean, focused tests pass (queued lanes); cacophony-fast-tests gate runs at
  reintegration.

## Operator takeaway

The daemon restart-loop driven by the watchdog misfire is fixed two ways: the
`/api/v1/node` handler is hardened so it doesn't hang on a contended lock, and
the watchdog requires a second independent liveness endpoint to also fail before
killing the process. Nodes pick this up once their installed binary is rebuilt
past this commit (binary rollout is operator territory). Related-but-separate:
bd-eead91 / bd-a9419e (helsinki :12100 beads-proxy flap) remain their own issue.
