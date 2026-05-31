# Session summary — Harden /api/v1/node liveness under backpressure (bd-81f3c8)

## Goal

The doctor reported ms-mac's daemon stuck `api_backpressured=true` /
`control_plane_ok=false` for 1h+ with the aggregate `caco status` snapshot
blocking ~60s (bd-13ae27). Make the `/api/v1/node` control-plane liveness probe
stay responsive under agent-manager-lock contention and disk-IO pressure so the
daemon does not look sustainedly backpressured, satisfying the "snapshot/liveness
path must not block ~60s" acceptance criterion.

## Bead(s)

- `bd-81f3c8` — Harden /api/v1/node liveness handler so it does not block ~60s
  under agent-manager-lock / disk-IO contention (P2 task).
- Lineage: `bd-13ae27` (doctor symptom tracker, closed on symptom self-recovery
  with no code fix for the liveness angle) and `bd-3780f3` (msm-1, d6579f8d0 —
  fixed the sequential snapshot-fetch serialization; complementary, not a dup).

## Before state

- Failing tests: none specific; this is hardening.
- `/api/v1/node` (the probe `caco status` uses for daemon_reachable ->
  api_backpressured / control_plane_ok) was NOT lightweight: it called
  state.agents.list_all().await (global agent-manager mutex) and ran inline
  blocking `git rev-parse` / `git log` subprocess calls in standby_checkout_health
  (beads-primary-candidate nodes only). Under contention/disk-IO the probe
  serialized behind the mutex / blocked on git, timing out the CLI's ~3s probe
  and reporting sustained false-backpressure for the whole window.

## After state

- Failing tests: none. caco-daemon builds + clippy clean in the changed regions.
- handle_node bounds list_all() + queue_status() with
  effective_node_probe_section_timeout (default 2000ms, env override
  CACO_NODE_PROBE_SECTION_TIMEOUT_MS, floored 100ms); on timeout it returns
  liveness with an empty agent list + a degraded scheduling section (new
  NodeSchedulingInfo.degraded flag, serialized only when true).
- standby_checkout_health uses git_query_bounded (wall-clock bounded, kills the
  child on timeout) instead of inline blocking git calls.
- Tests: node_endpoint_returns_success_envelope asserts non-degraded healthy
  scheduling; git_query_bounded_returns_none_for_non_git_dir (fast-fail);
  effective_node_probe_section_timeout_respects_override (default + 100ms floor).

## Diff summary

- Code commit: 2ce4afe986 (final landed squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: crates/caco-daemon/src/lib.rs (handle_node bounding + new helpers
  effective_node_probe_section_timeout / git_query_bounded + NodeSchedulingInfo
  degraded flag + 3 tests; 4 other NodeSchedulingInfo constructors updated with
  degraded: false).
- Tests: +3.
- Behavioural delta: /api/v1/node stays responsive (~2s bound) under
  agent-manager-lock contention or slow beads-checkout git; healthy fast path
  unchanged. Reduces sustained false api_backpressured / control_plane_ok=false.

## Embedded artefacts

None.

## Operator-takeaway

The "liveness" probe `caco status` relies on was secretly heavy — it took the
global agent-manager mutex and ran blocking git subprocesses — so on a busy /
disk-pressured node it blocked for tens of seconds and the daemon looked
backpressured the whole time even though it was alive. This bounds those sections
so liveness stays fast and degrades gracefully (a `degraded` scheduling flag)
rather than hanging. It complements msm-1's bd-3780f3 snapshot-serialization fix.
A doctor closed the original symptom tracker (bd-13ae27) on self-recovery before
the code fix landed; this re-homed the durable fix under bd-81f3c8 by coordination
rather than reopening the tracker.
