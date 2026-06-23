# Session summary — bd-fbdd22 slice 2: bound the /api/v1/nodes agent read (node/mesh/status view no longer blanks under registry-lock contention)

## Goal

Continue bd-fbdd22 with the next read-path slice while the bug context was loaded: bound the second uncovered contended agent-manager read — the one backing `/api/v1/nodes` (mesh / `caco node list` / `caco status`) — so that surface populates (not blanks) under sustained registry-write contention, mirroring slice 1 and the established bd-13ae27 precedent.

## Bead(s)

- `bd-fbdd22` — [daemon-reliability] Registry-read contention: list/ps/nudge/mesh aggregate reads blank under sustained registry-lock load. **Slices 1+2 landed; bead stays open** for the nudge functional-wake residual (bd-2ba5ed fix(b)).

## Before state

- Failing tests: none.
- `handle_nodes` (`/api/v1/nodes`) called `state.agents.list_all().await` UNBOUNDED to compute per-node agent counts + local scheduling. Same contended agent-manager lock as slice 1; under sustained registry-write load it blocked and blanked the whole node/mesh/`caco status` view.

## After state

- Failing tests: none. caco-daemon compiles clean; `cargo clippy -p caco-daemon` clean (0/0); handle_nodes happy-path tests green (`nodes_endpoint_returns_all_nodes`, `nodes_response_includes_agent_counts`, 12/12 in the queued run).
- `handle_nodes` bounds the read with `effective_agents_list_rebuild_timeout()` (the slice-1 knob, default 3s). On timeout it proceeds with an empty local agent set and sets the local node's `scheduling.degraded = true`, so node rows still populate (from config + dynamic registry + peer snapshots) with an explicit degraded signal — never a blank/blocked response. The `/api/v1/nodes` body is a `Vec<NodeSummary>` array, so the signal rides the existing per-node `scheduling.degraded` field rather than changing the response shape.

## Diff summary

- Code/content commits: this session's slice-2 commit (pending final squash SHA from the reintegration receipt). Slice 1 landed earlier at true main 65caa11909.
- Summary artefact commit: intentionally omitted (must not self-reference its own mutable SHA).
- Files touched: `crates/caco-daemon/src/lib.rs` (bound the handle_nodes agent read; surface degraded via the existing `NodeSchedulingInfo.degraded`).
- Tests: +0 new (the timeout->degraded branch is integration-level, identical to bd-13ae27's untested-in-unit precedent; the happy path is covered by the existing handle_nodes tests, which pass). Flipped 0.
- Behavioural delta: `/api/v1/nodes` (and therefore `caco node list` / mesh / `caco status`) returns populated node rows with a degraded local scheduling marker instead of blocking/blanking when the agent-manager lock is contended past the bound. Shape-preserving.

## Operator-takeaway

bd-fbdd22 now covers both high-traffic agent-manager-backed aggregate reads: `/api/v1/agents` (slice 1, the `caco agent list` / `caco ps` blanking) and `/api/v1/nodes` (slice 2, the mesh / `caco node list` / `caco status` blanking). Both bound the contended `list_all*()` and degrade gracefully (serve-stale or empty-with-marker) instead of blocking, mirroring bd-13ae27's `/api/v1/node` fix. The remaining open piece is the nudge functional-wake-under-contention (bd-2ba5ed fix(b)) — a distinct behavioral fix (actually waking the agent), not a read-snapshot bound, so it should be a separate session rather than folded into this read-path family.
